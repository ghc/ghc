{-# LANGUAGE CPP #-}

-- | CPP should still be allowed
module Main where

#include "Flags01_A.cpp"

#define mainn main=putStrLn str

mainn

