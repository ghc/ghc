{-# LANGUAGE CPP #-}

module A where

#include "a.h"
#include "b.h"

-- Test including a header from an external package.
#include "processFlags.h"

main :: IO ()
main = do
    putStrLn a
    putStrLn b