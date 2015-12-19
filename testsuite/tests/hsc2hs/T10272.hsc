module Main where

#include "T10272.h"

main :: IO ()
main = print #{alignment eight}
