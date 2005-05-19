{-# OPTIONS_GHC -fth #-}

-- Test error message when the code in a splice
-- fails (e.g. with pattern match failure)

module ShouldCompile where

$( case reverse "no" of
	[] -> return []
 )




