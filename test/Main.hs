{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import System.Posix.Process (getProcessID)
import System.Posix.Types (CPid(CPid))

import Control.Concurrent (myThreadId)
import Control.Monad (mapM_)
import Control.Monad.IO.Class (liftIO)

import qualified Data.HashMap.Strict as M

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

import Language.Haskell.TH.Syntax (Loc(Loc, loc_filename, loc_start))

import Data.Time.Calendar (Day(ModifiedJulianDay))
import Data.Time.Clock (UTCTime(UTCTime), secondsToDiffTime)

import Pipes ((>->))
import qualified Pipes.Prelude as P
import Pipes.Safe (SafeT, runSafeT)

import Katip (
    Environment(Environment, getEnvironment),
    Item(Item, _itemApp, _itemEnv, _itemLoc, _itemMessage, _itemNamespace, _itemSeverity, _itemThread, _itemTime),
    LogEnv, LogStr(unLogStr), Namespace(Namespace, unNamespace),
    Severity(AlertS, CriticalS, DebugS, EmergencyS, ErrorS, InfoS, NoticeS, WarningS),
    ThreadIdText(ThreadIdText, getThreadIdText), Verbosity(V0, V2),
    closeScribes, defaultScribeSettings, initLogEnv, logStr, logTM, registerScribe, runKatipContextT)
import Katip.Format.Time (formatAsIso8601)

import Systemd.Journal (
    Direction(Forwards), Filter(Match), JournalFlag(LocalOnly), Start(FromCursor, FromEnd),
    journalEntryCursor, journalEntryFields, mkJournalField, openJournal, sendMessage)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (around, it, shouldBe, testSpec)
import Test.Tasty.QuickCheck (Gen, (===), arbitrary, arbitraryBoundedEnum, conjoin, counterexample, forAll, frequency, testProperty)

import Katip.Scribes.Journal (itemToJournalFields, journalScribe)

class Convertible a where
    convert :: a -> ByteString

instance Convertible LogStr where
    convert = T.encodeUtf8 . TL.toStrict . TL.toLazyText . unLogStr

instance Convertible Namespace where
    convert = T.encodeUtf8 . T.intercalate "." . unNamespace

instance Convertible Environment where
    convert = T.encodeUtf8 . getEnvironment

instance Convertible ThreadIdText where
    convert = T.encodeUtf8 . getThreadIdText

instance Convertible UTCTime where
    convert = T.encodeUtf8 . formatAsIso8601

instance Convertible Severity where
    convert s = case s of
        DebugS -> "7"
        InfoS -> "6"
        NoticeS -> "5"
        WarningS -> "4"
        ErrorS -> "3"
        CriticalS -> "2"
        AlertS -> "1"
        EmergencyS -> "0"

testItemToJournalFields :: TestTree
testItemToJournalFields = testGroup "itemToJournalFields" [
      testProperty "maps all expected fields" $ forAll ((,) <$> genFacility <*> genItem) $ \(facility, item) ->
          let fields = itemToJournalFields facility V0 item in
          counterexample (show fields) $ conjoin [
                lookupField "MESSAGE" fields === Just (convert $ _itemMessage item)
              , lookupField "PRIORITY" fields === Just (convert $ _itemSeverity item)
              , lookupField "CODE_FILE" fields === (T.encodeUtf8 . T.pack . loc_filename) `fmap` _itemLoc item
              , lookupField "CODE_LINE" fields === (BS8.pack . show . fst . loc_start) `fmap` _itemLoc item
              , lookupField "SYSLOG_IDENTIFIER" fields === Just (convert $ _itemApp item)
              , lookupField "ENVIRONMENT" fields === Just (convert $ _itemEnv item)
              , lookupField "THREAD" fields === Just (convert $ _itemThread item)
              , lookupField "NAMESPACE" fields === Just (convert $ _itemNamespace item)
              , lookupField "TIME" fields === Just (convert $ _itemTime item)
              , lookupField "PAYLOAD" fields === Just ""
              ]
    ]
  where
    genFacility = frequency [(1, pure Nothing), (3, Just <$> arbitraryBoundedEnum)]

    genItem :: Gen (Item ())
    genItem = Item <$> (Namespace . (:[]) . T.pack) `fmap` arbitrary
                   <*> (Environment . T.pack) `fmap` arbitrary
                   <*> arbitraryBoundedEnum
                   <*> (\i -> ThreadIdText $ T.pack $ "ThreadId " ++ show (i :: Word)) `fmap` arbitrary
                   <*> arbitrary
                   <*> CPid `fmap` arbitrary
                   <*> arbitrary
                   <*> logStr `fmap` (arbitrary :: Gen String)
                   <*> (\(a, b) -> UTCTime (ModifiedJulianDay a) (secondsToDiffTime b)) `fmap` arbitrary
                   <*> (Namespace . map T.pack) `fmap` arbitrary
                   <*> frequency [(1, pure Nothing), (3, Just <$> (Loc <$> arbitrary
                                                                       <*> arbitrary
                                                                       <*> arbitrary
                                                                       <*> arbitrary
                                                                       <*> arbitrary))]
    lookupField f = M.lookup (mkJournalField $ T.pack f)

withLogEnv :: Severity -> Verbosity -> (LogEnv -> IO ()) -> IO ()
withLogEnv sev ver act = do
    logEnv <- registerScribe "journal" (journalScribe Nothing sev ver) defaultScribeSettings =<< initLogEnv "katip-libsystemd-journal-test" "testing"
    act logEnv

testJournalScribe :: IO TestTree
testJournalScribe = testSpec "journalScribe" $ around (withLogEnv DebugS V2) $
    it "logs to journal" $ \env -> (runSafeT :: SafeT IO () -> IO ()) $ do
        pid <- (BS8.pack . show) <$> liftIO getProcessID
        let pidField = mkJournalField "_PID"
            journal = openJournal [LocalOnly] (FromEnd Forwards) (Just $ Match pidField pid) Nothing

        liftIO $ sendMessage "Test starts, this is a marker"
        Just demo <- P.head journal
        let cursor = journalEntryCursor demo

        runKatipContextT env () "main" $ do
            $(logTM) AlertS "Something is very wrong (but that's fine)"

            $(logTM) DebugS "Debug message"
            $(logTM) InfoS "Info message"
            $(logTM) NoticeS "Notice message"
            $(logTM) WarningS "Warning message"
            $(logTM) ErrorS "Error message"
            $(logTM) CriticalS "Critical message"
            $(logTM) AlertS "Alert message"
            $(logTM) EmergencyS "Emergency message"
        _ <- liftIO $ closeScribes env

        let journal2 = openJournal [LocalOnly] (FromCursor cursor Forwards) (Just $ Match pidField pid) Nothing

        [_demo, entry] <- P.toListM $ journal2 >-> P.take 2
        liftIO $ do
            tid <- liftIO myThreadId
            let fields = journalEntryFields entry
                runTest (f, v) = M.lookup (mkJournalField f) fields `shouldBe` Just v
            mapM_ runTest [
                  ("MESSAGE", "Something is very wrong (but that's fine)")
                , ("PRIORITY", "1")
                , ("ENVIRONMENT", "testing")
                , ("NAMESPACE", "katip-libsystemd-journal-test.main")
                , ("THREAD", BS8.pack (show tid))
                , ("SYSLOG_IDENTIFIER", "katip-libsystemd-journal-test")
                ]

main :: IO ()
main = do
    testJournalScribe' <- testJournalScribe
    defaultMain $ testGroup "katip-libsystemd-journal" [
          testItemToJournalFields
        , testJournalScribe'
        ]
