1.1.5.1: [2023.08.24]
---------------------
* Allow newer `natural-arithmetic`, `primitive`, and `vector`.
* Allow base 4.18.
* Add units to offset docs.

1.1.5: [2022.11.03]
-------------------
* Add `decodeShortTextIso8601Zulu`.
* Add `decodeUtf8BytesIso8601ZonelessSpaced`.

1.1.4: [2022.02.21]
-------------------
* Fix February length in `normalYearMonthLength`. Fixes GitHub issue #67.
* Support aeson-2.x, with backward compatibility for aeson-1.x.
* Add helper `dateToDayOfWeek`
* Add helpers `encode_Ymd` and `encode_Dmy` to more easily pretty-print dates
* Add various lenses for core data type fields and isos
* Drop support for GHC 8.8 and earlier.

1.1.3: [2021.09.07]
-------------------
* Dropped support for GHC < 8.6.
* Integrated with `byteverse` libraries and `text-short`, adding efficient
  builders and parsers targeting UT8-encoded unpinned byte arrays. The new
  functions are: `boundedBuilderUtf8BytesIso8601Zoneless`,
  `encodeShortTextIso8601Zulu`, `encodeShortTextIso8601Zoneless`,
  `parserUtf8BytesIso8601`, `boundedBuilderUtf8BytesIso8601`,
  `decodeUtf8BytesIso8601`, `decodeShortTextIso8601`,
  `decodeShortTextIso8601Zoneless`, `decodeUtf8BytesIso8601Zoneless`,
  `encodeShortTextIso8601`.
* Corrected an implementation mistake that had caused many parsers to
  incorrectly identifier a subseconds part of 0 (i.e. `23:59:17.000`).
* Improved layout of cabal file.

1.1.2: [2021.02.08]
-------------------
* Adds `NFData` typeclass instances for all data types.
* Add `encodeIso8601` and `builderIso8601`.
* Soft deprecate `builderW3C` in favor of `builderIso8601`.
* Fix formatting in cabal file. 

1.1.1: [2020.04.17]
-------------------
* Add `timeToDayOfWeek`, `datetimeToDayOfWeek`, `todayDayOfWeek`,
  `yesterdayDayOfWeek`, and `tomorrowDayOfWeek`.
* Remove `stopwatchWith(_)` on GHC 8.6+.
* Fix build on 32-bit POSIX systems.

1.1: [2019.11.28]
-----------------
* Drop dependency of `clock` on GHC 8.6+.
* Remove `stopwatchWith(_)` on GHC 8.6+.
* Deprecate `stopwatchWith(_)` on GHC <8.6.
* Fix build on Windows. Thanks @SpaceKitteh for reporting this.

1.0.9: [2019.11.09]
-------------------
* Add `TimeParts` for custom formatting.

1.0.8: [2019.11.07]
-------------------
* Allow newer semigroups.

1.0.7: [2019.08.16]
-------------------
* Fix build on windows. Chronos now builds on windows, macos, and linux. Thanks
  to @nprindle for the fix/testing and @ShrykerWindgrace for reporting it!

1.0.6: [2019.07.19]
-----------------
* Add 'TimeInterval' type and related functions.

1.0.5: [2019.05.01]
-------------------
* Allow newer version of `clock` (==0.7.* ===> >=0.7 && < 0.9)
* Build with -Wall
* Build with -O2

1.0.4: [2018.08.14]
-------------------
* Initial version, w.r.t. CHANGELOG (i.e. there was no changelog before).
