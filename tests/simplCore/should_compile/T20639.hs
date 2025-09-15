module CtxRed(ctxRedCQType', CtxRed(..)) where

import Control.Monad.State
import Control.Monad.Except

type TI = StateT Bool (ExceptT [Integer] (State Bool))

data CDefn = Cclass (Maybe Bool) [CPred]
data CQType = CQType [CPred]
data CTypeclass = CTypeclass
data CPred = CPred CTypeclass

class CtxRed a where
    ctxRed :: a -> TI a

instance CtxRed CDefn where
    ctxRed d@(Cclass incoh cpreds) = do
       (CQType _) <- ctxRedCQType' (CQType [])
       return (Cclass incoh cpreds)

ctxRedCQType' :: CQType -> TI (CQType)
ctxRedCQType' cqt = do
    let CQType cqs = cqt
    case [p | p@(CPred CTypeclass) <- cqs] of
      p : _ -> return ()
      [] -> return ()
    return (cqt)
