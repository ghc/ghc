{-# LANGUAGE TypeFamilies, DataKinds, GADTs, FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE UndecidableInstances #-}
 -- To permit: type instance XLet 'InferTaggedBinders = XLet 'Vanilla

module GHC.Stg.InferTags.Types

where

#include "HsVersions.h"


import GHC.Prelude

import GHC.Core.DataCon
import GHC.Types.Id
import GHC.Stg.Syntax
import GHC.Types.Basic ( Arity )
import GHC.Types.Var.Env
import GHC.Utils.Outputable
import GHC.Utils.Misc( zipWithEqual )

import GHC.StgToCmm.Types ( LambdaFormInfo(..) )
import GHC.Utils.Panic

{- *********************************************************************
*                                                                      *
                         Supporting data types
*                                                                      *
********************************************************************* -}

type instance BinderP 'InferTaggedBinders = (Id, TagSig)
type instance XLet         'InferTaggedBinders = XLet         'Vanilla
type instance XLetNoEscape 'InferTaggedBinders = XLetNoEscape 'Vanilla
type instance XRhsClosure  'InferTaggedBinders = XRhsClosure  'Vanilla
type instance XStgApp      'InferTaggedBinders = XStgApp      'Vanilla

type InferStgTopBinding = GenStgTopBinding 'InferTaggedBinders
type InferStgBinding    = GenStgBinding    'InferTaggedBinders
type InferStgExpr       = GenStgExpr       'InferTaggedBinders
type InferStgRhs        = GenStgRhs        'InferTaggedBinders
type InferStgAlt        = GenStgAlt        'InferTaggedBinders

instance OutputableBndr (Id,TagSig) where
  pprInfixOcc  = ppr
  pprPrefixOcc = ppr

data TagInfo
  = TagDunno
  | TagTuple [TagInfo]  -- Unboxed tuple
  | TagProper           -- Heap pointer to properly-tagged value
  | TagTagged           -- Bottom of the domain.
  deriving( Eq )

instance Outputable TagInfo where
  ppr TagTagged      = text "TagTagged"
  ppr TagDunno       = text "TagDunno"
  ppr TagProper      = text "TagProper"
  ppr (TagTuple tis) = text "TagTuple" <> brackets (pprWithCommas ppr tis)

combineAltInfo :: TagInfo -> TagInfo -> TagInfo
combineAltInfo TagDunno         _              = TagDunno
combineAltInfo _                TagDunno       = TagDunno
combineAltInfo (TagTuple {})    TagProper      = panic "Combining unboxed tuple with non-tuple result"
combineAltInfo TagProper       (TagTuple {})   = panic "Combining unboxed tuple with non-tuple result"
combineAltInfo TagProper        TagProper      = TagProper
combineAltInfo (TagTuple is1)  (TagTuple is2)  = TagTuple (zipWithEqual "combineAltInfo" combineAltInfo is1 is2)
combineAltInfo (TagTagged)      ti             = ti
combineAltInfo ti               TagTagged      = ti

type TagSigEnv = IdEnv TagSig
data TagEnv p = TE { te_env :: TagSigEnv
                   , te_get :: BinderP p -> Id
                   , te_ext :: ExtEqEv (XLet p)
                                       (XLetNoEscape p) (XRhsClosure p) }

instance Outputable (TagEnv p) where
    ppr te = ppr (te_env te)


getBinderId :: TagEnv p -> BinderP p -> Id
getBinderId = te_get

-- This tiresome value is a proof that the extension fields
-- have the same type in pass p as in pass Tagged
-- ToDo: write a Note to explain properly
data ExtEqEv b c d where
  ExtEqEv :: ExtEqEv (XLet 'InferTaggedBinders)
                     (XLetNoEscape 'InferTaggedBinders) (XRhsClosure 'InferTaggedBinders)

initEnv :: TagEnv 'Vanilla
initEnv = TE { te_env = emptyVarEnv
             , te_get = \x -> x
             , te_ext = ExtEqEv }

-- | Simple convert env to a env of the 'InferTaggedBinders pass
-- with no other changes.
makeTagged :: TagEnv p -> TagEnv 'InferTaggedBinders
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
  -- Nullary data constructors like True, False
  | Just dc <- isDataConWorkId_maybe var
  , isNullaryRepDataCon dc
  = TagProper

  -- Variables in the environment
  | Just (TagSig 0 info) <- lookupVarEnv (te_env env) var
  = info

  -- | Just lf_info <- idLFInfo_maybe var
  -- =   case lf_info of
  --         -- Function, tagged (with arity)
  --         LFReEntrant {}
  --             -> TagProper
  --         -- Thunks need to be entered.
  --         LFThunk {}
  --             -> TagDunno
  --         -- Constructors, already tagged.
  --         LFCon {}
  --             -> TagProper
  --         LFUnknown {}
  --             -> TagDunno
  --         LFUnlifted {}
  --             -> TagProper
  --         -- Shouldn't be possible. I don't think we can export letNoEscapes
  --         LFLetNoEscape {} -> panic "LFLetNoEscape exported"

  | otherwise
  = TagDunno

lookupInfo _ (StgLitArg {})
  = TagProper

isDunnoSig :: TagSig -> Bool
isDunnoSig (TagSig _ TagDunno) = True
isDunnoSig (TagSig _ TagProper) = False
isDunnoSig (TagSig _ TagTuple{}) = False
isDunnoSig (TagSig _ TagTagged{}) = False

isTaggedSig :: TagSig -> Bool
isTaggedSig (TagSig _ TagProper) = True
isTaggedSig (TagSig _ TagTagged) = True
isTaggedSig _ = False

isTaggedInfo :: TagInfo -> Bool
isTaggedInfo TagProper = True
isTaggedInfo TagTagged = True
isTaggedInfo _         = False

extendSigEnv :: TagEnv p -> [(Id,TagSig)] -> TagEnv p
extendSigEnv env@(TE { te_env = sig_env }) bndrs
  = env { te_env = extendVarEnvList sig_env bndrs }
