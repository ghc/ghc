{-
 - Defaults.hs
 -
 - Contains the tuning values for compress and uncompress
 -
 -}

module Defaults
where

-- Maximum number of table entries (probably = 2^code_bits)

max_entries :: Int
max_entries = 2 ^ code_bits

-- First code value available

first_code :: Int
first_code = 256

-- Number of bits per output character

ascii_bits :: Int
ascii_bits = 8

-- Number of bits to represent code by

code_bits :: Int
code_bits = 12
