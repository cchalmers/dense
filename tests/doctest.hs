import Test.DocTest

main = doctest
  [ "src/Data/Dense/Index.hs"
  , "src/Data/Dense/Mutable.hs"
  , "src/Data/Dense/Unboxed.hs"
  , "src/Data/Dense/Storable.hs"
  , "src/Data/Dense/Boxed.hs"
  , "src/Data/Dense/Generic.hs"
  , "src/Data/Dense/Base.hs"
  ]
