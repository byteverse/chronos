{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Chronos.Types
import Data.List                            (intercalate)
import Test.QuickCheck                      (Gen, Arbitrary(..), choose)
import Test.QuickCheck.Property             (failed,succeeded,Result(..))
import Test.Framework                       (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (Assertion,(@?=),assertBool)

import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Chronos.Internal.Conversion as Conv
import qualified Chronos.Calendar as Month
import qualified Chronos.Posix as Posix
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Attoparsec.Text as Atto
import qualified Chronos.TimeOfDay.Text as TimeOfDayText
import qualified Chronos.Datetime.Text as DatetimeText
import qualified Chronos.OffsetDatetime.Text as OffsetDatetimeText
import qualified Chronos.Date.Text as DateText

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Time of Day"
    [ testGroup "Parser Spec Tests" $
      [ testCase "No Separator + microseconds"
          (timeOfDayParse Nothing "165956.246052" (TimeOfDay 16 59 56246052000))
      , testCase "Separator + microseconds"
          (timeOfDayParse (Just ':') "16:59:56.246052" (TimeOfDay 16 59 56246052000))
      , testCase "Separator + milliseconds"
          (timeOfDayParse (Just ':') "05:00:58.675" (TimeOfDay 05 00 58675000000))
      , testCase "Separator + deciseconds"
          (timeOfDayParse (Just ':') "05:00:58.9" (TimeOfDay 05 00 58900000000))
      , testCase "Separator + no subseconds"
          (timeOfDayParse (Just ':') "23:08:01" (TimeOfDay 23 8 1000000000))
      , testCase "Separator + nanoseconds"
          (timeOfDayParse (Just ':') "05:00:58.111222999" (TimeOfDay 05 00 58111222999))
      , testCase "Separator + 10e-18 seconds (truncate)"
          (timeOfDayParse (Just ':') "05:00:58.111222333444555666" (TimeOfDay 05 00 58111222333))
      ]
    , testGroup "Builder Spec Tests"
      [ testCase "No Separator + microseconds"
          (timeOfDayBuilder Nothing "165956.246052000" (TimeOfDay 16 59 56246052000))
      , testCase "Separator + microseconds"
          (timeOfDayBuilder (Just ':') "16:59:56.246052000" (TimeOfDay 16 59 56246052000))
      , testCase "Separator + no subseconds"
          (timeOfDayBuilder (Just ':') "23:08:01" (TimeOfDay 23 8 1000000000))
      ]
    , testProperty "Builder Parser Isomorphism (H:M:S)" $ propEncodeDecodeIso
        (LText.toStrict . Builder.toLazyText . TimeOfDayText.builder_HMS (Just ':'))
        (either (const Nothing) Just . Atto.parseOnly (TimeOfDayText.parser_HMS (Just ':')))
    ]
  , testGroup "Date"
    [ testGroup "Parser Spec Tests" $
      [ testCase "No Separator"
          (dateParse Nothing "20160101" (Date (Year 2016) (Month 0) (DayOfMonth 1)))
      , testCase "Separator 1"
          (dateParse (Just '-') "2016-01-01" (Date (Year 2016) (Month 0) (DayOfMonth 1)))
      , testCase "Separator 2"
          (dateParse (Just '-') "1876-09-27" (Date (Year 1876) (Month 8) (DayOfMonth 27)))
      ]
    , testGroup "Builder Spec Tests" $
      [ testCase "No Separator"
          (dateBuilder Nothing "20160101" (Date (Year 2016) (Month 0) (DayOfMonth 1)))
      , testCase "Separator 1"
          (dateBuilder (Just '-') "2016-01-01" (Date (Year 2016) (Month 0) (DayOfMonth 1)))
      , testCase "Separator 2"
          (dateBuilder (Just '-') "1876-09-27" (Date (Year 1876) (Month 8) (DayOfMonth 27)))
      ]
    , testProperty "Builder Parser Isomorphism (Y-m-d)" $ propEncodeDecodeIso
        (LText.toStrict . Builder.toLazyText . DateText.builder_Ymd (Just '-'))
        (either (const Nothing) Just . Atto.parseOnly (DateText.parser_Ymd (Just '-')))
    ]
  , testGroup "Datetime"
    [ testProperty "Builder Parser Isomorphism (Y-m-dTH:M:S)" $ propEncodeDecodeIso
        (LText.toStrict . Builder.toLazyText . DatetimeText.builder_YmdHMS (DatetimeFormat (Just '-') (Just 'T') (Just ':')))
        (either (const Nothing) Just . Atto.parseOnly (DatetimeText.parser_YmdHMS (DatetimeFormat (Just '-') (Just 'T') (Just ':'))))
    , testProperty "Builder Parser Isomorphism (YmdHMS)" $ propEncodeDecodeIso
        (LText.toStrict . Builder.toLazyText . DatetimeText.builder_YmdHMS (DatetimeFormat Nothing Nothing Nothing))
        (either (const Nothing) Just . Atto.parseOnly (DatetimeText.parser_YmdHMS (DatetimeFormat Nothing Nothing Nothing)))
    ]
  , testGroup "Offset Datetime"
    [ testGroup "Builder Spec Tests" $
      [ testCase "W3C" $ matchBuilder "1997-07-16T19:20:30.450000000+01:00" $
          OffsetDatetimeText.builderW3 $ OffsetDatetime
            ( Datetime
              ( Date (Year 1997) Month.july (DayOfMonth 16) )
              ( TimeOfDay 19 20 30450000000 )
            ) (Offset 60)
      ]
    ]
  , testGroup "Posix Time"
    [ testCase "Get now" $ do
        now <- Posix.now
        assertBool "Current time is the beginning of the epoch." (now /= Posix.epoch)
    ]
  , testGroup "Conversion"
    [ testGroup "POSIX to UTC"
      [ -- Technically, this should fail, but it is very unlikely that quickcheck
        -- will pick a leapsecond.
        testProperty "Isomorphism" $ propEncodeDecodeFullIso Posix.toUtc Posix.fromUtc
      ]
    , testGroup "UTC to Datetime"
      [ testProperty "Isomorphism"
          $ propEncodeDecodeFullIso Conv.utcTimeToDatetime Conv.datetimeToUtcTime
      ]
    , testGroup "POSIX to Datetime"
      [ testCase "Epoch" $ Posix.toDatetime (PosixTime 0)
          @?= Datetime (Date (Year 1970) Month.january (DayOfMonth 1))
                       (TimeOfDay 0 0 0)
      , testCase "Billion Seconds" $ Posix.toDatetime (PosixTime $ 10 ^ 18)
          @?= Datetime (Date (Year 2001) Month.september (DayOfMonth 9))
                       (TimeOfDay 1 46 (40 * 10 ^ 9))
      , testProperty "Isomorphism" $ propEncodeDecodeFullIso Posix.toDatetime Posix.fromDatetime
      ]
    ]
  ]

failure :: String -> Result
failure msg = failed
  { reason = msg
  , theException = Nothing
  }

propEncodeDecodeFullIso :: (Eq a,Show a,Show b) => (a -> b) -> (b -> a) -> a -> Result
propEncodeDecodeFullIso f g a =
  let fa = f a
      gfa = g fa
   in if gfa == a
        then succeeded
        else failure $ concat
          [ "x:       ", show a, "\n"
          , "f(x):    ", show fa, "\n"
          , "g(f(x)): ", show gfa, "\n"
          ]

propEncodeDecodeIso :: Eq a => (a -> b) -> (b -> Maybe a) -> a -> Bool
propEncodeDecodeIso f g a = g (f a) == Just a

timeOfDayParse :: Maybe Char -> Text -> TimeOfDay -> Assertion
timeOfDayParse m t expected =
  Atto.parseOnly (TimeOfDayText.parser_HMS m <* Atto.endOfInput) t
  @?= Right expected

timeOfDayBuilder :: Maybe Char -> Text -> TimeOfDay -> Assertion
timeOfDayBuilder m expected tod =
  LText.toStrict (Builder.toLazyText (TimeOfDayText.builder_HMS m tod))
  @?= expected

dateParse :: Maybe Char -> Text -> Date -> Assertion
dateParse m t expected =
  Atto.parseOnly (DateText.parser_Ymd m <* Atto.endOfInput) t
  @?= Right expected

dateBuilder :: Maybe Char -> Text -> Date -> Assertion
dateBuilder m expected tod =
  LText.toStrict (Builder.toLazyText (DateText.builder_Ymd m tod))
  @?= expected

matchBuilder :: Text -> Builder -> Assertion
matchBuilder a b = LText.toStrict (Builder.toLazyText b) @?= a

instance Arbitrary TimeOfDay where
  arbitrary = TimeOfDay
    <$> choose (0,23)
    <*> choose (0,59)
    -- never use leap seconds for property-based tests
    <*> choose (0,60000000000 - 1)

instance Arbitrary Date where
  arbitrary = Date
    <$> fmap Year (choose (1800,2100))
    <*> fmap Month (choose (0,11))
    <*> fmap DayOfMonth (choose (1,28))

instance Arbitrary Datetime where
  arbitrary = Datetime <$> arbitrary <*> arbitrary

instance Arbitrary PosixTime where
  arbitrary = fmap PosixTime arbitrary

instance Arbitrary UtcTime where
  arbitrary = UtcTime
    <$> fmap Day (choose (-100000,100000))
    <*> choose (0,24 * 60 * 60 * 1000000000 - 1)
