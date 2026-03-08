{-# LANGUAGE NoImplicitPrelude #-}
module B (T(..), g) where

import A

{-# INLINE g #-}
g :: T -> T
g x = f x
