{-# OPTIONS -fth #-}

-- Test the use of tupleDataName, tupleTypeName

module ShouldCompile where

import Language.Haskell.TH

foo = $( sigE (appsE [varE (tupleDataName 2),
		     litE (integerL 1),
		     litE (integerL 2)])
	      (appT (appT (varT (tupleTypeName 2))
			  (varT ''Integer))
		    (varT ''Integer))
	)
