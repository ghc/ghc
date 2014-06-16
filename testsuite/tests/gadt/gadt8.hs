{-# LANGUAGE GADTs, KindSignatures #-}

-- Test a couple of trivial things:
-- 	explicit layout
--	trailing semicolons
--	kind signatures
module ShouldCompile where

data Expr :: * -> * where {
   EInt    :: Int                                 -> Expr Int  ;
   EBool   :: Bool                                -> Expr Bool ;
   EIf     :: (Expr Bool) -> (Expr a) -> (Expr a) -> Expr a    ;
		-- Note trailing semicolon, should be ok
  }

