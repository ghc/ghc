{-# LANGUAGE MagicHash #-}
module Main (main) where

import GHC.Exts (Double(D#), Float(F#), Word(W#), word2Double#, word2Float#)

main :: IO ()
main = do
    print (D# (word2Double# 0##))
    -- 4294967295 is 2^32 - 1, the largest 32-bit integer, and can be
    -- stored in a 64-bit IEEE floating-point value without loss of
    -- precision.
    print (D# (word2Double# 4294967295##))
    print (F# (word2Float# 0##))
    -- 16777216 is 2^24, which is the largest integer which can be
    -- stored in a 32-bit IEEE floating-point value without loss of
    -- precision
    print (F# (word2Float# 16777216##))

    -- We also want to check for sane behaviour for cases that lose precision
    let W# max_word = (maxBound :: Word)
    print (F# (word2Float# max_word))

    -- 2^31 sits exactly at the MSB boundary exercised by the i386 algorithms
    -- (see Notes [Word-to-float64 conversion on i386] and
    -- [Word-to-float32 conversion on i386] in GHC.CmmToAsm.X86.CodeGen)
    print (D# (word2Double# 2147483648##))
    print (F# (word2Float# 2147483648##))
