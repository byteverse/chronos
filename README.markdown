# Chronos

Time library for haskell. The main differences between this and `time` are
that this library:

- Uses machine integers where possible. This means that some time-related
  arithmetic should be faster. It also means that the types are incapable
  of representing times that are very far in the future or the past.
- Provides `ToJSON` and `FromJSON` instances.
- Uses normal non-overloaded haskell functions for encoding and decoding time. It provides
  `attoparsec` parsers for both `Text` and `ByteString`. Additionally, it
  provides functions for encoding time as `Text` or `ByteString`. The `time`
  library uses accomplishes these with the `Data.Time.Format` module,
  which uses UNIX-style datetime format strings. It is expected that
  the approach taken in this library is faster at the cost of being
  less expressive.
- Only provides nanosecond resolution instead of picosecond resolution.

## Benchmarks

`chronos` library performance is compared with `time` and `thyme`.

### Parsing

| Benchmark name            | Time     |
|---------------------------|----------|
| Time.parseTimeM           | 9.679 μs |
| Thyme.parseTime           | 1.743 μs |
| Thyme.timeParser          | 1.113 μs |
| Chronos.parserUtf8_YmdHMS | 301.4 ns |
| Chronos.zeptoUtf8_YmdHMS  | 173.6 ns |

### Pretty-printing

| Benchmark name          | Time     |
|-------------------------|----------|
| dmy/Time.formatTime     | 4.404 μs |
| dmy/Thyme.formatTime    | 663.0 ns |
| dmy/Chronos.builder_Dmy | 340.9 ns |
| HMS/Time.formatTime     | 1.987 μs |
| HMS/Thyme.formatTime    | 879.1 ns |
| HMS/Chronos.builder_HMS | 481.3 ns |
