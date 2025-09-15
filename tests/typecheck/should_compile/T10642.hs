{-# LANGUAGE TypeFamilies #-}
module T10642 where

import Data.Coerce

type family F a

newtype D a = D (F a)

-- | This works on 7.10.1, but fails on HEAD (20150711)
coerceD :: F a -> D a
coerceD = coerce
