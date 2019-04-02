{-# LANGUAGE TypeFamilies, DataKinds, GADTs, FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE UndecidableInstances #-}
 -- To permit: type instance XLet 'TaggedSimon = XLet 'Vanilla

module GHC.Stg.InferTags.Types

where

#include "HsVersions.h"


import GHC.Prelude

import GHC.Core.DataCon
import GHC.Types.Id
import GHC.Stg.Syntax
import GHC.Types.Basic ( Arity, RecFlag (NonRecursive) )
import GHC.Types.Var.Env
import GHC.Core (AltCon(..))
import Data.List (mapAccumL)
import GHC.Utils.Outputable
import GHC.Utils.Misc( zipWithEqual )

{- *********************************************************************
*                                                                      *
                         Supporting data types
*                                                                      *
********************************************************************* -}

type instance BinderP 'TaggedSimon = (Id, TagSig)
type instance XStgConApp   'TaggedSimon = XStgConApp   'Vanilla
type instance XLet         'TaggedSimon = XLet         'Vanilla
type instance XLetNoEscape 'TaggedSimon = XLetNoEscape 'Vanilla
type instance XRhsClosure  'TaggedSimon = XRhsClosure  'Vanilla
type instance XStgApp      'TaggedSimon = XStgApp      'Vanilla
type instance XRhsCon      'TaggedSimon = XRhsCon      'Vanilla

type InferStgTopBinding = GenStgTopBinding 'TaggedSimon
type InferStgBinding    = GenStgBinding    'TaggedSimon
type InferStgExpr       = GenStgExpr       'TaggedSimon
type InferStgRhs        = GenStgRhs        'TaggedSimon
type InferStgAlt        = GenStgAlt        'TaggedSimon

instance OutputableBndr (Id,TagSig) where
  pprInfixOcc  = ppr
  pprPrefixOcc = ppr

data TagInfo
  = TagDunno
  | TagTuple [TagInfo]  -- Unboxed tuple
  | TagProper           -- Heap pointer to properly-tagged value
                        -- Bottom of the domain
  deriving( Eq )

instance Outputable TagInfo where
  ppr TagDunno       = text "TagDunno"
  ppr TagProper      = text "TagProper"
  ppr (TagTuple tis) = text "TagTuple" <> brackets (pprWithCommas ppr tis)

combineAltInfo :: TagInfo -> TagInfo -> TagInfo
combineAltInfo TagDunno         _              = TagDunno
combineAltInfo TagProper        ti             = ti
combineAltInfo (TagTuple {})    TagDunno       = TagDunno
combineAltInfo ti@(TagTuple {}) TagProper      = ti
combineAltInfo (TagTuple is1)   (TagTuple is2) = TagTuple (zipWithEqual "combineAltInfo" combineAltInfo is1 is2)

type TagSigEnv = IdEnv TagSig
data TagEnv p = TE { te_env :: TagSigEnv
                   , te_get :: BinderP p -> Id
                   , te_ext :: ExtEqEv (XStgConApp p) (XLet p)
                                       (XLetNoEscape p) (XRhsClosure p) }

instance Outputable (TagEnv p) where
    ppr te = ppr (te_env te)


getBinderId :: TagEnv p -> BinderP p -> Id
getBinderId = te_get

-- This tiresome value is a proof that the extension fields
-- have the same type in pass p as in pass Tagged
-- ToDo: write a Note to explain properly
data ExtEqEv a b c d where
  ExtEqEv :: ExtEqEv (XStgConApp 'TaggedSimon)      (XLet 'TaggedSimon)
                     (XLetNoEscape 'TaggedSimon) (XRhsClosure 'TaggedSimon)

initEnv :: TagEnv 'Vanilla
initEnv = TE { te_env = emptyVarEnv
             , te_get = \x -> x
             , te_ext = ExtEqEv }

-- | Simple convert env to a env of the 'TaggedSimon pass
-- with no other changes.
makeTagged :: TagEnv p -> TagEnv 'TaggedSimon
makeTagged env = TE { te_env = te_env env
                    , te_get = fst
                    , te_ext = ExtEqEv }

data TagSig  -- The signature for each binding
  = TagSig Arity TagInfo -- TODO: I think we can skip the arity, it should always be available via idArity
                         -- for all cases where we compute it.
  deriving( Eq )

instance Outputable TagSig where
  ppr (TagSig ar ti) = char '<' <> ppr ar <> comma <> ppr ti <> char '>'

noSig :: TagEnv p -> BinderP p -> (Id, TagSig)
noSig env bndr = (getBinderId env bndr, TagSig 0 TagDunno)

lookupSig :: TagEnv p -> Id -> Maybe TagSig
lookupSig env fun = lookupVarEnv (te_env env) fun

lookupInfo :: TagEnv p -> StgArg -> TagInfo
lookupInfo env (StgVarArg var)
  -- Variables in the environment
  | Just (TagSig 0 info) <- lookupVarEnv (te_env env) var
  = info

  -- Nullary data constructors like True, False
  | Just dc <- isDataConWorkId_maybe var
  , isNullaryRepDataCon dc
  = TagProper

  | otherwise
  = TagDunno

lookupInfo _ (StgLitArg {})
  = TagProper

isDunnoSig :: TagSig -> Bool
isDunnoSig (TagSig _ TagDunno) = True
isDunnoSig (TagSig _ TagProper) = False
isDunnoSig (TagSig _ TagTuple{}) = False

isTaggedSig :: TagSig -> Bool
isTaggedSig (TagSig _ TagProper) = True
isTaggedSig _ = False

extendSigEnv :: TagEnv p -> [(Id,TagSig)] -> TagEnv p
extendSigEnv env@(TE { te_env = sig_env }) bndrs
  = env { te_env = extendVarEnvList sig_env bndrs }
