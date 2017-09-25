{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.DeepSeq (NFData(rnf))

import Data.Time.Calendar (Day(ModifiedJulianDay))
import Data.Time.Clock (UTCTime(UTCTime), secondsToDiffTime)

import Katip (
    Item(Item, _itemApp, _itemEnv, _itemHost, _itemLoc, _itemMessage, _itemNamespace, _itemPayload, _itemProcess, _itemSeverity, _itemThread, _itemTime),
    Severity(InfoS), ThreadIdText(ThreadIdText), Verbosity(V0))

import Systemd.Journal (JournalField)

import Criterion (bench, nf)
import Criterion.Main (defaultMain)

import Katip.Scribes.Journal (itemToJournalFields)

instance NFData JournalField where
    rnf f = f `seq` ()

main :: IO ()
main = defaultMain [
      bench "itemToJournalFields" $ nf (itemToJournalFields Nothing V0) item
    ]
  where
    item :: Item ()
    item = Item { _itemApp = "katip-libsystemd-journal-bench"
                , _itemEnv = "testing"
                , _itemSeverity = InfoS
                , _itemThread = ThreadIdText "ThreadId 1"
                , _itemHost = "localhost"
                , _itemProcess = 1
                , _itemPayload = ()
                , _itemMessage = "Benchmark running"
                , _itemTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
                , _itemNamespace = "benchmark"
                , _itemLoc = Nothing
                }
