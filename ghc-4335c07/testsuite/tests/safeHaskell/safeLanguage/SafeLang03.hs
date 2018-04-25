{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}

-- | Test SafeLanguage disables things
module SafeLang03 where

{-# RULES "f" f = undefined #-}
f :: Int
f = 1

