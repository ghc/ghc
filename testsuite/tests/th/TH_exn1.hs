{-# LANGUAGE TemplateHaskell #-}

-- Test error message when the code in a splice
-- fails in an immediate fashion (e.g. with a
-- pattern match failure)

module ShouldCompile where

$( case reverse "no" of
	[] -> return []
 )
