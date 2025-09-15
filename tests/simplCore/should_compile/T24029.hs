{-# OPTIONS_GHC -O #-}
module T24029 (surround) where

data Buffer where
  Buffer :: !Int -> Buffer

newtype Builder = Builder (Buffer -> Buffer)

c :: Builder -> Builder -> Builder
c (Builder f) (Builder g) = Builder (\b -> f (g b))

i :: Buffer -> Buffer
i (Buffer x) = Buffer x

surround :: Builder -> Builder
surround f = f
{-# NOINLINE [1] surround #-}

{-# RULES
"surround/surround" forall a. surround a = c (Builder (i . i)) a
  #-}
