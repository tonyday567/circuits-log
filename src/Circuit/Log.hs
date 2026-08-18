{-# LANGUAGE OverloadedStrings #-}

-- | Generic JSONL logs with YAML-frontmattered bodies.
--
-- Each line in the log is a JSON envelope @{ "id": N, "ts": "...", "body": "..." }@.
-- The body is expected to be a markdown string with optional YAML frontmatter;
-- metadata lives in the frontmatter, not in the envelope.
module Circuit.Log
  ( -- * Configuration
    LogConfig (..),
    defaultLogConfig,

    -- * Entries
    LogEntry (..),
    readLog,
    readLogEither,
    nextId,
    renderEntry,
    appendEntry,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    eitherDecodeStrict,
    encode,
    object,
    withObject,
    (.:),
    (.=),
  )
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

-- | Configuration for a log file.
newtype LogConfig = LogConfig
  { -- | Path to the JSONL log file.
    logPath :: FilePath
  }
  deriving (Show)

-- | Default log configuration.
--
-- Points at the coffee permanent archive. Override 'logPath' for other logs.
defaultLogConfig :: LogConfig
defaultLogConfig =
  LogConfig
    { logPath = "/Users/tonyday567/archive/coffee-permanent.jsonl"
    }

-- | One log entry: envelope only.
data LogEntry = LogEntry
  { entryId :: Int,
    entryTs :: UTCTime,
    entryBody :: Text
  }
  deriving (Show)

instance FromJSON LogEntry where
  parseJSON = withObject "LogEntry" $ \v ->
    LogEntry
      <$> v .: "id"
      <*> v .: "ts"
      <*> v .: "body"

instance ToJSON LogEntry where
  toJSON e =
    object
      [ "id" .= entryId e,
        "ts" .= entryTs e,
        "body" .= entryBody e
      ]

-- | Read all entries from the log.
--
-- Fails on the first parse error. For fault-tolerant reading, use
-- 'readLogEither'.
readLog :: LogConfig -> IO [LogEntry]
readLog cfg = do
  es <- readLogEither cfg
  case sequence es of
    Left err -> error ("readLog: " <> err)
    Right entries -> pure entries

-- | Read all entries, preserving parse errors.
readLogEither :: LogConfig -> IO [Either String LogEntry]
readLogEither cfg = do
  exists <- doesFileExist (logPath cfg)
  if not exists
    then pure []
    else do
      contents <- BL.readFile (logPath cfg)
      case decodeUtf8' (BL.toStrict contents) of
        Left err -> pure [Left ("UTF-8 decode error: " <> show err)]
        Right text -> pure (parseLine <$> filter (not . T.null) (T.lines text))
  where
    parseLine line =
      case eitherDecodeStrict (encodeUtf8 line) of
        Left err -> Left ("parse error: " <> err)
        Right entry -> Right entry

-- | Next id for a log: one greater than the maximum existing id, or 1.
nextId :: [LogEntry] -> Int
nextId entries =
  case entryId <$> entries of
    [] -> 1
    ids -> maximum ids + 1

-- | Render one entry as a single JSONL line.
renderEntry :: LogEntry -> Text
renderEntry = decodeUtf8 . BL.toStrict . encode

-- | Append a new entry with the given body to the log.
appendEntry :: LogConfig -> Text -> IO ()
appendEntry cfg body = do
  entries <- readLog cfg
  now <- getCurrentTime
  let entry = LogEntry (nextId entries) now body
      line = renderEntry entry <> "\n"
  createDirectoryIfMissing True (takeDirectory (logPath cfg))
  TIO.appendFile (logPath cfg) line
