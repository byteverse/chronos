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
