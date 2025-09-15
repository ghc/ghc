{-# LANGUAGE PatternSynonyms #-}

module T11633 where

data ARecord = ARecord {anInt :: Int, aString :: String}

-- This works...
pattern AGoodPat :: Int -> String -> ARecord
pattern AGoodPat n s = ARecord {anInt=n, aString=s}

pattern ABadPat :: Int -> String -> ARecord
pattern ABadPat n s = ARecord {aString=s, anInt=n}
