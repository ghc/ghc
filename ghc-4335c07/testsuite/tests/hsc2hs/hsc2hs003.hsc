
-- Test that hsc2hs actually gives the right answer in "direct" mode

#include <stdint.h>

module Main (main) where

main :: IO ()
main = print #size uint16_t

