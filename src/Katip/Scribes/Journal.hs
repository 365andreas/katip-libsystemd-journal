{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module:      Katip.Scribes.Journal
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: OverloadedStrings, RecordWildCards
--
-- Linking "Katip" code with <https://www.freedesktop.org/wiki/Software/systemd/ systemd>'s
-- <https://www.freedesktop.org/software/systemd/man/systemd-journald.service.html journal service>.

module Katip.Scribes.Journal (
    -- * Scribe
      journalScribe
    -- * Internals
    , itemToJournalFields
    ) where

import Control.Monad (when)

import qualified Data.ByteString.Lazy as BL (toStrict)

import qualified Data.Text as T (empty, intercalate)
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Data.Text.Lazy.Builder as TL (toLazyText)
import qualified Data.Text.Lazy.Encoding as TL (encodeUtf8)

import qualified Data.HashMap.Strict as M (empty, fromList)

import Language.Haskell.TH.Syntax (Loc(Loc, loc_filename, loc_start))

import Katip (
    Environment(getEnvironment),
    Item(Item, _itemApp, _itemEnv, _itemLoc, _itemMessage, _itemNamespace, _itemPayload, _itemSeverity, _itemThread, _itemTime),
    LogItem, LogStr(unLogStr), Namespace(unNamespace), Scribe(Scribe),
    Severity(AlertS, CriticalS, DebugS, EmergencyS, ErrorS, InfoS, NoticeS, WarningS),
    ThreadIdText(getThreadIdText), Verbosity,
    permitItem)
import Katip.Format.Time (formatAsIso8601)
import Katip.Scribes.Handle (brackets, getKeys)

import Systemd.Journal (
    JournalFields, Priority(Alert, Critical, Debug, Emergency, Error, Info, Notice, Warning),
    codeFile, codeLine, message, mkJournalField, priority, sendJournalFields,
    syslogFacility, syslogIdentifier)
import System.Posix.Syslog (Facility)

-- | "Katip" 'Scribe' which emits log entries to the
-- <https://www.freedesktop.org/wiki/Software/systemd/ systemd>
-- <https://www.freedesktop.org/software/systemd/man/systemd-journald.service.html journal service>.
--
-- Standard fields included in each entry are the log message ('MESSAGE'),
-- severity ('PRIORITY'), source code location if available ('CODE_FILE' and
-- 'CODE_LINE') and syslog identifier, using the application name passed to
-- Katip ('SYSLOG_IDENTIFIER'). An optional syslog facility ('SYSLOG_FACILITY')
-- can be provided as well.
--
-- See <https://www.freedesktop.org/software/systemd/man/systemd.journal-fields.html systemd.journal-fields>
-- for more information about these fields.
--
-- Some non-standard fields are also emitted: the environment name passed to
-- Katip ('ENVIRONMENT'), the thread ID where the log message was emitted
-- ('THREAD'), the entry's namespace, concatenated using dots ('NAMESPACE'),
-- the timestamp at which the entry was emitted, formatted as an ISO8601 string
-- ('TIME'), and the payload of the log entry, flattened out into bracketed
-- fields ('PAYLOAD').
journalScribe :: Maybe Facility  -- ^ Optional 'SYSLOG_FACILITY' added to every log item
              -> Severity  -- ^ Minimal 'Severity' of messages to emit
              -> Verbosity  -- ^ 'Verbosity' of payloads to render
              -> Scribe
journalScribe facility severity verbosity = Scribe liPush scribeFinalizer
  where
    liPush :: LogItem a => Item a -> IO ()
    liPush i = when (permitItem severity i) $
        sendJournalFields $ itemToJournalFields facility verbosity i

    scribeFinalizer :: IO ()
    scribeFinalizer = pure ()


-- | Convert a "Katip" 'Item' into a "libsystemd-journal" 'JournalFields' map.
--
-- /Note:/ Exported for benchmarking purposes only.
itemToJournalFields :: LogItem a
                    => Maybe Facility  -- ^ Optional 'SYSLOG_FACILITY'
                    -> Verbosity  -- ^ Verbosity level of payload to log
                    -> Item a  -- ^ Item to convert
                    -> JournalFields
itemToJournalFields facility verbosity item = mconcat [ defaultFields item
                                                      , maybe M.empty facilityFields facility
                                                      , maybe M.empty locFields (_itemLoc item)
                                                      ]
  where
    defaultFields Item{..} = mconcat [ message (TL.toStrict $ TL.toLazyText $ unLogStr _itemMessage)
                                     , priority (mapSeverity _itemSeverity)
                                     , syslogIdentifier (unNS _itemApp)
                                     , M.fromList [ (environment, T.encodeUtf8 $ getEnvironment _itemEnv)
                                                  , (namespace, T.encodeUtf8 $ unNS _itemNamespace)
                                                  , (payload, BL.toStrict $ TL.encodeUtf8 $ TL.toLazyText $ unPayload _itemPayload)
                                                  , (thread, T.encodeUtf8 $ getThreadIdText _itemThread)
                                                  , (time, T.encodeUtf8 $ formatAsIso8601 _itemTime)
                                                  ]
                                     ]
    facilityFields = syslogFacility
    locFields Loc{..} = mconcat [ codeFile loc_filename
                                , codeLine (fst loc_start)
                                ]

    environment = mkJournalField "environment"
    namespace = mkJournalField "namespace"
    payload = mkJournalField "payload"
    thread = mkJournalField "thread"
    time = mkJournalField "time"

    unNS ns = case unNamespace ns of
        [p] -> p -- Short-circuit common cases
        [] -> T.empty
        parts -> T.intercalate "." parts

    unPayload = mconcat . map brackets . getKeys verbosity

    mapSeverity s = case s of
        DebugS -> Debug
        InfoS -> Info
        NoticeS -> Notice
        WarningS -> Warning
        ErrorS -> Error
        CriticalS -> Critical
        AlertS -> Alert
        EmergencyS -> Emergency
{-# INLINE itemToJournalFields #-}
