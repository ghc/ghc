{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Main ( main ) where

import GHC.Exts
import GHC.Prim
import GHC.ST

main = putStr
       (test_sizeofArray
        ++ "\n" ++ test_sizeofMutableArray
        ++ "\n"
       )

test_sizeofArray :: String
test_sizeofArray = flip shows "\n" $ runST $ ST $ \ s# -> go 0 [] s#
  where
    go i@(I# i#) acc s#
        | i < 1000 = case newArray# i# 0 s# of
            (# s2#, marr# #) -> case unsafeFreezeArray# marr# s2# of
                (# s3#, arr# #) -> case sizeofArray# arr# of
                    j# -> go (i+1) ((I# j#):acc) s3#
        | otherwise = (# s#, reverse acc #)

test_sizeofMutableArray :: String
test_sizeofMutableArray = flip shows "\n" $ runST $ ST $ \ s# -> go 0 [] s#
  where
    go i@(I# i#) acc s#
        | i < 1000 = case newArray# i# 0 s# of
            (# s2#, marr# #) -> case sizeofMutableArray# marr# of
                    j# -> go (i+1) ((I# j#):acc) s2#
        | otherwise = (# s#, reverse acc #)

