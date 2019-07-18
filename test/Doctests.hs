import Test.DocTest

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/Chronos.hs"
  , "src/Chronos/Locale/English.hs"
  ]
