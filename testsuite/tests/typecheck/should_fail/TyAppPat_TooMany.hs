{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}

module TyAppPat_TooMany where

f :: Maybe Int -> Int
f (Just @Int @Bool x) = x
f Nothing = 10
