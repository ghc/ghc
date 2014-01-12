module ShouldCompile where

data R a = R {
    field1 :: a -- | comment for field2
  , field2 :: a 
  , field3 :: a -- ^ comment for field3
  , {-| comment for field4 -} field4 :: a
}
