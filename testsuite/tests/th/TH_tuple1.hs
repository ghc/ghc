{-# LANGUAGE TemplateHaskell #-}

-- Test the use of tupleDataName, tupleTypeName

module ShouldCompile where

import Language.Haskell.TH

foo = $( sigE (appsE [conE (tupleDataName 2),
		     litE (integerL 1),
		     litE (integerL 2)])
	      (appT (appT (conT (tupleTypeName 2))
			  (conT ''Integer))
		    (conT ''Integer))
	)
