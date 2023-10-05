{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module T17646 where

data T a where
  A :: T True
  B :: T False

g :: ()
g | B <- A = ()
