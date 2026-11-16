{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module RetainedKeywordOff where

foreach :: a -> a
foreach x = x

keep :: foreach (b :: Bool) -> foreach b
keep x = x
