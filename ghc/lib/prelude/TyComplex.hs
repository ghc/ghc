module PreludeComplex ( Complex(..) ) where

-- infix  6  :+

data {- (RealFloat a) => -} Complex a = a :+ a  deriving () -- (Eq,{-ToDo:Binary,-}Text)

--  (NB: compiler's builtin idea of "data Complex ..." must also
--	omit the context!)

{-# SPECIALIZE data Complex Double# #-}
