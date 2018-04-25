{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Safe #-}

-- | Test SafeLanguage disables things
-- Testing ordering doesn't matter this time
-- with Safe appearing after TH.
module SafeLang16 where

f :: Int
f = 1

