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


