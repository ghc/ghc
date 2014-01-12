{-# LANGUAGE RankNTypes #-}

-- Make sure for-alls can occur in data types

module Foo where

newtype CPS1 a = CPS1 { unCPS1 :: forall ans . (a -> ans) -> ans }

newtype CPS2 a = CPS2 (forall ans . (a -> ans) -> ans)
	-- This one also has an interesting record selector;
	-- caused an applyTypeArgs crash in 5.02.1

data 	CPS3 a = CPS3 { unCPS3 :: forall ans . (a -> ans) -> ans }
data 	CPS4 a = CPS4 (forall ans . (a -> ans) -> ans)
