{-# LANGUAGE RebindableSyntax, MonadFailDesugaring #-}
{-# OPTIONS_GHC -Wno-missing-monadfail-instances #-}

-- Test that rebindable clash warnings are not displayed. This program
-- should not generate anything on stderr at compile time.

module Main where

import Prelude

catMaybes xs = do
    Just x <- xs
    return x

main = return ()
