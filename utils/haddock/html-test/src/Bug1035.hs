{-# LANGUAGE Haskell2010 #-}
module Bug1035 where

data Foo = Bar

data Bar = Foo

-- | A link to 'Bar'
foo :: ()
foo = ()
