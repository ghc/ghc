-- We suppress arity and LfInfo with the option.
{-# OPTIONS_GHC -fomit-interface-pragmas #-}
module InferTags007_A (fun, caf, bot, con) where

fun :: Int -> Int
fun x = x + 1
{-# NOINLINE fun #-}

-- A CAF/thunk
caf :: [Int]
caf = map (* 2) [1 .. 10]
{-# NOINLINE caf #-}

bot :: Int -> Int
bot _ = error "bot called"
{-# NOINLINE bot #-}

con :: Maybe Int
con = Just 42
{-# NOINLINE con #-}
