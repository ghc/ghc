{-# LANGUAGE DuplicateRecordFields, NoFieldSelectors #-}

module T23279_aux where

data Bar = Bar
  { x :: Int
  , y :: Char
  , z :: Bool
  , w :: Double
  }

data Baz = Baz { z :: Float }

w :: ()
w = ()

{-# DEPRECATED x "Don't use x" #-}
{-# DEPRECATED z "Don't use z" #-}
{-# DEPRECATED w "Don't use w" #-}
