# Chronos

Chronos is a performance-oriented time library for Haskell, with a
straightforward API. The main differences between this
and the [http://hackage.haskell.org/package/time](time) library
are:
  * Chronos uses machine integers where possible. This means
    that time-related arithmetic should be faster, with the
    drawback that the types are incapable of representing times
    that are very far in the future or the past (because Chronos
    provides nanosecond, rather than picosecond, resolution).
    For most users, this is not a hindrance.
  * Chronos provides 'ToJSON'/'FromJSON' instances for serialisation.
  * Chronos provides 'Unbox' instances for working with unboxed vectors.
  * Chronos provides 'Prim' instances for working with byte arrays/primitive arrays.
  * Chronos uses normal non-overloaded haskell functions for
    encoding and decoding time. It provides [http://hackage.haskell.org/package/attoparsec](attoparsec) parsers for both 'Text' and
    'ByteString'. Additionally, Chronos provides functions for
    encoding time to 'Text' or 'ByteString'. The [http://hackage.haskell.org/package/time](time) library accomplishes these with the
    [http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html](Data.Time.Format) module, which uses UNIX-style datetime
    format strings. The approach taken by Chronos is faster and
    catches more mistakes at compile time, at the cost of being
    less expressive.

## Benchmarks

Benchmarks of `chronos` against `time` and `thyme`.

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
