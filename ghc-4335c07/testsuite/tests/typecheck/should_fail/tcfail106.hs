-- This one risked building a recursive dictionary rather than
-- failing, in GHC before 5.03.   Actually, 5.02 managed it ok,
-- but I think more by luck than good judgement.

module ShouldFail where

class S a
class S a => C a where { opc :: a -> a }
class S b => D b where { opd :: b -> b }

instance C Int where
   opc = opd

instance D Int where
   opd = opc
