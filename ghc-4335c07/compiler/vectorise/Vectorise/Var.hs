{-# LANGUAGE TupleSections #-}

-- |Vectorise variables and literals.

module Vectorise.Var
  ( vectBndr
  , vectBndrNew
  , vectBndrIn
  , vectBndrNewIn
  , vectBndrsIn
  , vectVar
  , vectConst
  )
where

import GhcPrelude

import Vectorise.Utils
import Vectorise.Monad
import Vectorise.Env
import Vectorise.Vect
import Vectorise.Type.Type
import CoreSyn
import Type
import VarEnv
import Id
import FastString

-- Binders ----------------------------------------------------------------------------------------

-- |Vectorise a binder variable, along with its attached type.
--
vectBndr :: Var -> VM VVar
vectBndr v
 = do (vty, lty) <- vectAndLiftType (idType v)
      let vv = v `Id.setIdType` vty
          lv = v `Id.setIdType` lty

      updLEnv (mapTo vv lv)

      return  (vv, lv)
  where
    mapTo vv lv env = env { local_vars = extendVarEnv (local_vars env) v (vv, lv) }

-- |Vectorise a binder variable, along with its attached type, but give the result a new name.
--
vectBndrNew :: Var -> FastString -> VM VVar
vectBndrNew v fs
 = do vty <- vectType (idType v)
      vv  <- newLocalVVar fs vty
      updLEnv (upd vv)
      return vv
  where
    upd vv env = env { local_vars = extendVarEnv (local_vars env) v vv }

-- |Vectorise a binder then run a computation with that binder in scope.
--
vectBndrIn :: Var -> VM a -> VM (VVar, a)
vectBndrIn v p
 = localV
 $ do vv <- vectBndr v
      x <- p
      return (vv, x)

-- |Vectorise a binder, give it a new name, then run a computation with that binder in scope.
--
vectBndrNewIn :: Var -> FastString -> VM a -> VM (VVar, a)
vectBndrNewIn v fs p
 = localV
 $ do vv <- vectBndrNew v fs
      x  <- p
      return (vv, x)

-- |Vectorise some binders, then run a computation with them in scope.
--
vectBndrsIn :: [Var] -> VM a -> VM ([VVar], a)
vectBndrsIn vs p
 = localV
 $ do vvs <- mapM vectBndr vs
      x   <- p
      return (vvs, x)


-- Variables --------------------------------------------------------------------------------------

-- |Vectorise a variable, producing the vectorised and lifted versions.
--
vectVar :: Var -> VM VExpr
vectVar var
  = do { vVar <- lookupVar var
       ; case vVar of
           Local (vv, lv) -> return (Var vv, Var lv) -- local variables have a vect & lifted version
           Global vv      -> vectConst (Var vv)      -- global variables get replicated
       }


-- Constants --------------------------------------------------------------------------------------

-- |Constants are lifted by replication along the integer context in the `VM` state for the number
-- of elements in the result array.
--
vectConst :: CoreExpr -> VM VExpr
vectConst c = (c,) <$> liftPD c
