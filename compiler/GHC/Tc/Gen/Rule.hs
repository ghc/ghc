{-# LANGUAGE TypeFamilies #-}

{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998

-}

-- | Typechecking rewrite rules
module GHC.Tc.Gen.Rule (
      tcRules,
      tcRuleBndrs,
      mkTcRuleBndrs
  ) where

import GHC.Prelude

import GHC.Hs
import GHC.Tc.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Solver
import GHC.Tc.Solver.Monad ( runTcS )
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.TcType
import GHC.Tc.Gen.HsType
import GHC.Tc.Gen.Sig( tcRuleBndrs )
import GHC.Tc.Gen.Expr
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Unify( buildImplicationFor )
import GHC.Tc.Zonk.TcType

import GHC.Core.Type
import GHC.Core.Coercion( mkCoVarCo )
import GHC.Core.TyCon( isTypeFamilyTyCon )
import GHC.Core.Predicate

import GHC.Types.Id
import GHC.Types.Var( EvVar, tyVarName )
import GHC.Types.Var.Set
import GHC.Types.Basic ( RuleName, NonStandardDefaultingStrategy(..) )
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.FastString
import GHC.Data.Bag

