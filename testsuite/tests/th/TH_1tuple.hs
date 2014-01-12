{-# LANGUAGE TemplateHaskell #-}

-- Trac #2358

module ShouldFail where
import Language.Haskell.TH

x = $(sigE [|1|] (tupleT 1 `appT` conT ''Int))	
	-- 1 :: (Int)	( a 1-tuple type)

y = $(sigE [|1|] (tupleT 1))
	-- 1 :: (1)  	(a 1-tuple tycon not applied)

z = $(tupE [ [| "yes" |] ])
	-- ("yes")  	(a 1-tuple expression)
