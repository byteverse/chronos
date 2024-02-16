# Chronos

Chronos is a performance-oriented time library for Haskell, with a
straightforward API. The main differences between this
and the [time](http://hackage.haskell.org/package/time) library
are:
  * Chronos uses machine integers where possible. This means
    that time-related arithmetic should be faster, with the
    drawback that the types are incapable of representing times
    that are very far in the future or the past (because Chronos
    provides nanosecond, rather than picosecond, resolution).
    For most users, this is not a hindrance and the tradeoff is
    worthwhile.
  * Chronos provides 'ToJSON'/'FromJSON' instances for serialisation.
  * Chronos provides 'Unbox' instances for working with unboxed vectors.
  * Chronos provides 'Prim' instances for working with byte arrays/primitive arrays.
  * Chronos uses normal non-overloaded haskell functions for
    encoding and decoding time. It provides [attoparsec](http://hackage.haskell.org/package/attoparsec) parsers for both 'Text' and
    'ByteString'. Additionally, Chronos provides functions for
    encoding time to 'Text' or 'ByteString'. The [time](http://hackage.haskell.org/package/time) library accomplishes these with the
    [Data.Time.Format](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html) module, which uses UNIX-style datetime
    format strings. The approach taken by Chronos is faster and
    catches more mistakes at compile time, at the cost of being
    less expressive.

Jacob Stanley has written
[a blog post comparing the features and performance](https://jacobstanley.io/3-packages-you-need-to-know-about-before-processing-timestamps-in-haskell/)
of `time`, `thyme`, and `chronos`. It has a good bulleted breakdown of why
you may want to use each library along with some benchmarks.

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

## Doctest

Doctest used to be provided as a test suite, but `doctest-0.20` and higher
do not require this to be run. To run the doctests, make sure you have
`doctest` on your path (i.e. run `cabal install doctest`), and then run:

    cabal build
    cabal repl --build-depends=QuickCheck --with-ghc=doctest --repl-options='-fno-warn-orphans'

This runs incredibly slowly, but it works for now. Doctest is not run by CI,
so if you make a change that adds more doctests, it needs to be run by hand
by someone. (The maintainer is happy to do this if you're on a platform
where doctest is finicky.)
