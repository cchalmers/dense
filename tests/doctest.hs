import Test.DocTest

main = doctest
  [ "src/Data/Shaped/Index.hs"
  , "src/Data/Shaped/Mutable.hs"
  , "src/Data/Shaped/Generic.hs"
  , "src/Data/Shaped/Base.hs"
  ]
