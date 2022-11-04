{-# LANGUAGE MagicHash #-}
module Main (main) where

import GHC.Exts (Double(D#), Float(F#), Word(W#), word2Double#, word2Float#)

main :: IO ()
main = do
    print (D# (word2Double# 0##))
    -- 9007199254740992 is 2^53, which is the largest integer which
    -- can be stored in a 64-bit IEEE floating-point value without
    -- loss of precision.
    print (D# (word2Double# 9007199254740992##))
    print (F# (word2Float# 0##))
    -- 16777216 is 2^24, which is the largest integer which can be
    -- stored in a 32-bit IEEE floating-point value without loss of
    -- precision
    print (F# (word2Float# 16777216##))

    -- We also want to check for sane behaviour for cases that lose precision
    let W# max_word = (maxBound :: Word)
    print (F# (word2Float# max_word))
    print (D# (word2Double# max_word))

    -- 2^63 is the first value that requires the halve-and-double path
    -- (see Note [Word-to-float conversion on x86-64] in GHC.CmmToAsm.X86.CodeGen)
    let W# two63 = 0x8000000000000000
    print (F# (word2Float# two63))
    print (D# (word2Double# two63))
