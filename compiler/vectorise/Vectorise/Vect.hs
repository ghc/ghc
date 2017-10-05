-- |Simple vectorised constructors and projections.
--
module Vectorise.Vect
  ( Vect, VVar, VExpr, VBind

  , vectorised
  , lifted
  , mapVect

  , vVarType
  , vNonRec
  , vRec
  , vVar
  , vType
  , vTick
  , vLet
  , vLams
  , vVarApps
  , vCaseDEFAULT
  )
where

import GhcPrelude

import CoreSyn
import Type           ( Type )
import Var

-- |Contains the vectorised and lifted versions of some thing.
--
type Vect a = (a,a)
type VVar   = Vect Var
type VExpr  = Vect CoreExpr
type VBind  = Vect CoreBind

-- |Get the vectorised version of a thing.
--
vectorised :: Vect a -> a
vectorised = fst

-- |Get the lifted version of a thing.
--
lifted :: Vect a -> a
lifted = snd

-- |Apply some function to both the vectorised and lifted versions of a thing.
--
mapVect :: (a -> b) -> Vect a -> Vect b
mapVect f (x, y) = (f x, f y)

-- |Combine vectorised and lifted versions of two things componentwise.
--
zipWithVect :: (a -> b -> c) -> Vect a -> Vect b -> Vect c
zipWithVect f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

-- |Get the type of a vectorised variable.
--
vVarType :: VVar -> Type
vVarType = varType . vectorised

-- |Wrap a vectorised variable as a vectorised expression.
--
vVar :: VVar -> VExpr
vVar = mapVect Var

-- |Wrap a vectorised type as a vectorised expression.
--
vType :: Type -> VExpr
vType ty = (Type ty, Type ty)

-- |Make a vectorised note.
--
vTick :: Tickish Id -> VExpr -> VExpr
vTick = mapVect . Tick

-- |Make a vectorised non-recursive binding.
--
vNonRec :: VVar -> VExpr -> VBind
vNonRec = zipWithVect NonRec

-- |Make a vectorised recursive binding.
--
vRec :: [VVar] -> [VExpr] -> VBind
vRec vs es = (Rec (zip vvs ves), Rec (zip lvs les))
  where
    (vvs, lvs) = unzip vs
    (ves, les) = unzip es

-- |Make a vectorised let expression.
--
vLet :: VBind -> VExpr -> VExpr
vLet = zipWithVect Let

-- |Make a vectorised lambda abstraction.
--
-- The lifted version also binds the lifting context 'lc'.
--
vLams :: Var      -- ^ Var bound to the lifting context.
      -> [VVar]   -- ^ Parameter vars for the abstraction.
      -> VExpr    -- ^ Body of the abstraction.
      -> VExpr
vLams lc vs (ve, le)
  = (mkLams vvs ve, mkLams (lc:lvs) le)
  where
    (vvs, lvs) = unzip vs

-- |Apply an expression to a set of argument variables.
--
-- The lifted version is also applied to the variable of the lifting context.
--
vVarApps :: Var -> VExpr -> [VVar] -> VExpr
vVarApps lc (ve, le) vvs
  = (ve `mkVarApps` vs, le `mkVarApps` (lc : ls))
  where
    (vs, ls) = unzip vvs


vCaseDEFAULT :: VExpr  -- scrutinee
             -> VVar   -- bnder
             -> Type   -- type of vectorised version
             -> Type   -- type of lifted version
             -> VExpr  -- body of alternative.
             -> VExpr
vCaseDEFAULT (vscrut, lscrut) (vbndr, lbndr) vty lty (vbody, lbody)
  = (Case vscrut vbndr vty (mkDEFAULT vbody),
     Case lscrut lbndr lty (mkDEFAULT lbody))
  where
    mkDEFAULT e = [(DEFAULT, [], e)]
