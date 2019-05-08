{-# LANGUAGE DataKinds, PolyKinds #-}

module T12928 where

data P (a::k) = MkP

data FffSym0 (l :: P a)

-- Make sure that the kind of 'k' is not defaulted:
--
--    data FffSym0 (l :: P (a :: Type))
--
-- We expect kind polymorphism:
--
--    data FffSym0 (l :: P (a :: k))
--
type Inst (a :: P Either) (b :: P Maybe) = (FffSym0 a, FffSym0 b)
