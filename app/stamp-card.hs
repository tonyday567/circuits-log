{-# LANGUAGE OverloadedStrings #-}

-- | Archive a loom card into a JSONL log.
--
-- Usage:
--
--   stamp-card preview --input loom/card.md
--   stamp-card append --input loom/card.md [--log PATH]
--
-- 'preview' prints the JSONL envelope to stdout. 'append' writes it to the
-- configured log. The default log is the coffee permanent archive.
module Main (main) where

import Circuit.Log
  ( LogConfig (..),
    LogEntry (..),
    appendEntry,
    defaultLogConfig,
    formatUtc,
    nextId,
    readLog,
    renderEntry,
  )
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)
import Options.Applicative

-- ---------------------------------------------------------------------------
-- Command type
-- ---------------------------------------------------------------------------

data Command
  = Preview FilePath LogConfig
  | Append FilePath LogConfig
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

inputOpt :: Parser FilePath
inputOpt =
  option
    str
    ( long "input"
        <> short 'i'
        <> metavar "PATH"
        <> help "Path to the loom card to stamp"
    )

logOpt :: Parser FilePath
logOpt =
  option
    str
    ( long "log"
        <> short 'l'
        <> metavar "PATH"
        <> value (logPath defaultLogConfig)
        <> showDefault
        <> help "Path to the JSONL log"
    )

logConfigP :: Parser LogConfig
logConfigP = LogConfig <$> logOpt

previewP :: Parser Command
previewP = Preview <$> inputOpt <*> logConfigP

appendP :: Parser Command
appendP = Append <$> inputOpt <*> logConfigP

commandParser :: Parser Command
commandParser =
  subparser
    ( command "preview" (info (previewP <**> helper) (progDesc "Print the JSONL envelope to stdout"))
        <> command "append" (info (appendP <**> helper) (progDesc "Append the JSONL envelope to the log"))
    )

opts :: ParserInfo Command
opts =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "Archive a loom card as a JSONL log entry"
        <> header "stamp-card - JSONL archiver for coffee cards"
    )

-- ---------------------------------------------------------------------------
-- Main dispatch
-- ---------------------------------------------------------------------------

main :: IO ()
main = execParser opts >>= run

run :: Command -> IO ()
run (Preview path cfg) = do
  body <- TIO.readFile path
  entries <- readLog cfg
  now <- getCurrentTime
  TIO.putStrLn (renderEntry (LogEntry (nextId entries) (formatUtc now) body))
run (Append path cfg) = do
  body <- TIO.readFile path
  appendEntry cfg body
