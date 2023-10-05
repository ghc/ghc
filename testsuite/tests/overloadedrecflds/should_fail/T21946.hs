{-# LANGUAGE DuplicateRecordFields, PatternSynonyms #-}

module T21946 where

pattern R1 :: Int -> Int
pattern R1 { fld } = fld

pattern R2 :: Bool -> Bool
pattern R2 { fld } = fld

f r = (r :: Int) { fld = undefined }
