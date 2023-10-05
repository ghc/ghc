{-# OPTIONS_GHC -haddock #-}
module HsDocTy where

class C1 a where
  f1 :: a -> Int
    -- ^ comment on Int
