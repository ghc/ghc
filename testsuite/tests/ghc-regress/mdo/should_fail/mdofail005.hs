-- {-# OPTIONS -fglasgow-exts #-}

-- use of mdo requires -fglasgow-exts to be given,
-- so let's try leaving it out

module Main (main) where

import Control.Monad.Fix

main :: IO ()
main = mdo x <- return (1:x)
	   return ()
