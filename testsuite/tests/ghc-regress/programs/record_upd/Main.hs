{- 	The purpose of this is to test that record update is
	sufficiently polymorphic.  See comments with
	tcExpr (RecordUpd) in TcExpr.lhs
-}

module Main where

data T a b c d  = MkT1 { op1 :: a, op2 :: b }
	       | MkT2 { op1 :: a, op3 :: c }
	       | MkT3 { op4 :: a, op5 :: d }

update1 :: a2 -> T a b c d -> T a2 b c d2
update1 x t = t { op1 = x }
	-- NB: the MkT3.op4 case doesn't constrain the result because
	-- it doesn't have an op1 field

update2 :: a2 -> T a b c d -> T a2 b2 c2 d
update2 x t = t { op4 = x }

main = print (op4 $ 
	      update2 True $ 
	      MkT3 { op4 = op2 $
			   update1 (1::Int) $
			   MkT1 { op1 = True }
	      })
