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
    formatUtc,
  )
where

import Circuit.Parser.Json (Json (..), decodeJson, encodeJson)
import Data.ByteString.Lazy qualified as BL
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
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
    -- | ISO-8601 timestamp stored as text so legacy offsets (e.g.
    -- @+10:00@) and clean UTC stamps (e.g. @Z@) coexist.
    entryTs :: Text,
    entryBody :: Text
  }
  deriving (Show)

entryToJson :: LogEntry -> Json
entryToJson e =
  JObject
    [ ("id", JNumber (fromIntegral (entryId e))),
      ("ts", JString (entryTs e)),
      ("body", JString (entryBody e))
    ]

entryFromJson :: Json -> Either String LogEntry
entryFromJson (JObject pairs) = do
  i <- lookupField "id" pairs >>= asInt
  t <- lookupField "ts" pairs >>= asText
  b <- lookupField "body" pairs >>= asText
  pure (LogEntry i t b)
entryFromJson _ = Left "log entry must be a JSON object"

lookupField :: Text -> [(Text, Json)] -> Either String Json
lookupField key pairs = case lookup key pairs of
  Just v -> Right v
  Nothing -> Left ("missing field: " <> T.unpack key)

asInt :: Json -> Either String Int
asInt (JNumber n) = case toBoundedInteger n of
  Just i -> Right i
  Nothing -> Left ("id out of int range: " <> show n)
asInt _ = Left "id must be a number"

asText :: Json -> Either String Text
asText (JString t) = Right t
asText _ = Left "timestamp and body must be strings"

-- | Format a UTC time as @YYYY-MM-DDTHH:MM:SSZ@.
formatUtc :: UTCTime -> Text
formatUtc = T.pack . formatTime defaultTimeLocale "%FT%TZ"

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
      case decodeJson (encodeUtf8 line) of
        Left err -> Left ("parse error: " <> err)
        Right j -> entryFromJson j

-- | Next id for a log: one greater than the maximum existing id, or 1.
nextId :: [LogEntry] -> Int
nextId entries =
  case entryId <$> entries of
    [] -> 1
    ids -> maximum ids + 1

-- | Render one entry as a single JSONL line.
renderEntry :: LogEntry -> Text
renderEntry = decodeUtf8 . encodeJson . entryToJson

-- | Append a new entry with the given body to the log.
appendEntry :: LogConfig -> Text -> IO ()
appendEntry cfg body = do
  entries <- readLog cfg
  now <- getCurrentTime
  let entry = LogEntry (nextId entries) (formatUtc now) body
      line = renderEntry entry <> "\n"
  createDirectoryIfMissing True (takeDirectory (logPath cfg))
  TIO.appendFile (logPath cfg) line
