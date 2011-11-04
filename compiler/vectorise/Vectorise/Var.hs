
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- | Vectorise variables and literals.
module Vectorise.Var (
	vectBndr,
	vectBndrNew,
	vectBndrIn,
	vectBndrNewIn,
	vectBndrsIn,
	vectVar,
	vectPolyVar,
	vectLiteral
) where
import Vectorise.Utils
import Vectorise.Monad
import Vectorise.Env
import Vectorise.Vect
import Vectorise.Type.Type
import CoreSyn
import Type
import VarEnv
import Literal
import Id
import FastString
import Control.Monad


-- Binders ----------------------------------------------------------------------------------------
-- | Vectorise a binder variable, along with its attached type.
vectBndr :: Var -> VM VVar
vectBndr v
 = do (vty, lty) <- vectAndLiftType (idType v)
      let vv = v `Id.setIdType` vty
          lv = v `Id.setIdType` lty

      updLEnv (mapTo vv lv)

      return  (vv, lv)
  where
    mapTo vv lv env = env { local_vars = extendVarEnv (local_vars env) v (vv, lv) }


-- | Vectorise a binder variable, along with its attached type, 
--   but give the result a new name.
vectBndrNew :: Var -> FastString -> VM VVar
vectBndrNew v fs
 = do vty <- vectType (idType v)
      vv  <- newLocalVVar fs vty
      updLEnv (upd vv)
      return vv
  where
    upd vv env = env { local_vars = extendVarEnv (local_vars env) v vv }


-- | Vectorise a binder then run a computation with that binder in scope.
vectBndrIn :: Var -> VM a -> VM (VVar, a)
vectBndrIn v p
 = localV
 $ do vv <- vectBndr v
      x <- p
      return (vv, x)


-- | Vectorise a binder, give it a new name, then run a computation with that binder in scope.
vectBndrNewIn :: Var -> FastString -> VM a -> VM (VVar, a)
vectBndrNewIn v fs p
 = localV
 $ do vv <- vectBndrNew v fs
      x  <- p
      return (vv, x)


-- | Vectorise some binders, then run a computation with them in scope.
vectBndrsIn :: [Var] -> VM a -> VM ([VVar], a)
vectBndrsIn vs p
 = localV
 $ do vvs <- mapM vectBndr vs
      x	  <- p
      return (vvs, x)


-- Variables --------------------------------------------------------------------------------------
-- | Vectorise a variable, producing the vectorised and lifted versions.
vectVar :: Var -> VM VExpr
vectVar v
 = do 
      -- lookup the variable from the environment.
      r	<- lookupVar v

      case r of
        -- If it's been locally bound then we'll already have both versions available.
        Local (vv,lv) 
         -> return (Var vv, Var lv)

        -- To create the lifted version of a global variable we replicate it
	-- using the integer context in the VM state for the number of elements.
        Global vv     
         -> do let vexpr = Var vv
               lexpr <- liftPD vexpr
               return (vexpr, lexpr)


-- | Like `vectVar` but also add type applications to the variables.
vectPolyVar :: Var -> [Type] -> VM VExpr
vectPolyVar v tys
 = do vtys	<- mapM vectType tys
      r		<- lookupVar v
      case r of
        Local (vv, lv) 
         -> liftM2 (,) (polyApply (Var vv) vtys)
                       (polyApply (Var lv) vtys)

        Global poly    
         -> do vexpr <- polyApply (Var poly) vtys
               lexpr <- liftPD vexpr
               return (vexpr, lexpr)


-- Literals ---------------------------------------------------------------------------------------
-- | Lifted literals are created by replicating them
--   We use the the integer context in the `VM` state for the number
--   of elements in the output array.
vectLiteral :: Literal -> VM VExpr
vectLiteral lit
 = do lexpr <- liftPD (Lit lit)
      return (Lit lit, lexpr)

