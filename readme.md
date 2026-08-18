# circuits-log

Generic JSONL logs with YAML-frontmattered bodies.

Each log line is a JSON envelope:

```json
{"id": 1, "ts": "2026-08-18T23:30:00Z", "body": "---\nstatus: done\n---\n# title\n..."}
```

The `body` is a markdown string with optional YAML frontmatter. The envelope
carries only `id` and `ts`; metadata lives in the body's frontmatter.

## library

`Circuit.Log` provides:

- `LogConfig { logPath :: FilePath }` with `defaultLogConfig`
- `LogEntry { entryId :: Int, entryTs :: UTCTime, entryBody :: Text }`
- `readLog`, `readLogEither`
- `nextId`
- `renderEntry`
- `appendEntry`

## stamp-card executable

```bash
cabal run stamp-card -- preview --input loom/card.md
# review the JSONL, then:
cabal run stamp-card -- append --input loom/card.md
```

Both commands read the log to compute the next id. `preview` prints the JSONL
line to stdout; `append` appends it to the configured log path.
