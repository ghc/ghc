{-# LANGUAGE TypeFamilies, DataKinds, GADTs, FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE UndecidableInstances #-}
 -- To permit: type instance XLet 'InferTaggedBinders = XLet 'CodeGen

module GHC.Stg.EnforceEpt.Types
    ( module GHC.Stg.EnforceEpt.Types
    , module TagSig)
where

import GHC.Prelude

import GHC.Core.DataCon
import GHC.Core.Type (isUnliftedType)
import GHC.Types.Id
import GHC.Stg.Syntax
import GHC.Stg.EnforceEpt.TagSig as TagSig
import GHC.Types.Var.Env
import GHC.Utils.Outputable
import GHC.Utils.Misc( zipWithEqual )
import GHC.Utils.Panic

import GHC.StgToCmm.Types

{- *********************************************************************
*                                                                      *
                         Supporting data types
*                                                                      *
********************************************************************* -}

type InferStgTopBinding = GenStgTopBinding 'InferTaggedBinders
type InferStgBinding    = GenStgBinding    'InferTaggedBinders
type InferStgExpr       = GenStgExpr       'InferTaggedBinders
type InferStgRhs        = GenStgRhs        'InferTaggedBinders
type InferStgAlt        = GenStgAlt        'InferTaggedBinders

-- | Combine TagInfo from the alternatives of a case expression.
-- Note that this operates at the value level: case alternatives return
-- values. See Note [TagSig and TagInfo].
combineAltInfo :: TagInfo -> TagInfo -> TagInfo
combineAltInfo TagBottoming    ti              = ti
combineAltInfo ti              TagBottoming    = ti
combineAltInfo TagDunno        TagDunno        = TagDunno
combineAltInfo TagDunno        TagEPT          = TagDunno
combineAltInfo TagDunno        (TagTuple {})   = TagDunno
combineAltInfo TagEPT          TagDunno        = TagDunno
combineAltInfo (TagTuple {})   TagDunno        = TagDunno
combineAltInfo TagEPT          TagEPT          = TagEPT
-- TagEPT/TagTuple are incompatible (can arise with rep-polymorphic
-- results, see #26107); fall through to TagDunno.
combineAltInfo TagEPT          (TagTuple {})   = TagDunno
combineAltInfo (TagTuple {})   TagEPT          = TagDunno
combineAltInfo (TagTuple is1)  (TagTuple is2)  = TagTuple (zipWithEqual combineAltInfo is1 is2)

type TagSigEnv = IdEnv TagSig
data TagEnv p = TE { te_env :: TagSigEnv
                   , te_get :: BinderP p -> Id
                   , te_bytecode :: !Bool
                   }

instance Outputable (TagEnv p) where
    ppr te = for_txt <+> ppr (te_env te)
        where
            for_txt = if te_bytecode te
                then text "for_bytecode"
                else text "for_native"

getBinderId :: TagEnv p -> BinderP p -> Id
getBinderId = te_get

initEnv :: Bool -> TagEnv 'CodeGen
initEnv for_bytecode = TE { te_env = emptyVarEnv
             , te_get = \x -> x
             , te_bytecode = for_bytecode }

-- | Simple convert env to a env of the 'InferTaggedBinders pass
-- with no other changes.
makeTagged :: TagEnv p -> TagEnv 'InferTaggedBinders
makeTagged env = TE { te_env = te_env env
                    , te_get = fst
                    , te_bytecode = te_bytecode env }

noSig :: TagEnv p -> BinderP p -> (Id, TagSig)
noSig env bndr
  | isUnliftedType (idType var) = (var, TagVal TagEPT)
  | otherwise = (var, TagVal TagDunno)
  where
    var = getBinderId env bndr

-- | Look up the return-value tag of a function for saturated call analysis.
-- Returns 'Just retInfo' if the function binding has a 'TagFun' signature,
-- 'Nothing' otherwise.
lookupReturnInfo :: TagEnv p -> Id -> Maybe TagInfo
lookupReturnInfo env fun = case lookupVarEnv (te_env env) fun of
  Just (TagFun ret_info) -> Just ret_info
  Just (TagVal _)        -> Nothing
  Nothing                -> Nothing

-- | Look up a value-level tag for an argument: either from the env (where
-- a function-typed argument flattens to TagEPT — its closure is tagged)
-- or derived from information on the variable itself.
lookupInfo :: TagEnv p -> StgArg -> TagInfo
lookupInfo env (StgVarArg var)
  -- Nullary data constructors like True, False
  | Just dc <- isDataConWorkId_maybe var
  , isNullaryRepDataCon dc
  , not for_bytecode
  = TagEPT

  | isUnliftedType (idType var)
  = TagEPT

  -- Variables in the environment. A function binding flattens to TagEPT
  -- since a function closure pointer is properly tagged; its return info
  -- is not relevant when the function is used as a value.
  | Just sig <- lookupVarEnv (te_env env) var
  = case sig of
      TagVal info -> info
      TagFun _    -> TagEPT

  | Just lf_info <- idLFInfo_maybe var
  , not for_bytecode
  =   case lf_info of
          -- Function, tagged (with arity)
          LFReEntrant {}
              -> TagEPT
          -- Thunks need to be entered.
          LFThunk {}
              -> TagDunno
          -- Constructors, already tagged.
          LFCon {}
              -> TagEPT
          LFUnknown {}
              -> TagDunno
          LFUnlifted {}
              -> TagEPT
          -- Shouldn't be possible. I don't think we can export letNoEscapes
          LFLetNoEscape {} -> panic "LFLetNoEscape exported"

  | otherwise
  = TagDunno
  where
    for_bytecode = te_bytecode env

lookupInfo _ (StgLitArg {})
  = TagEPT

isDunnoSig :: TagSig -> Bool
isDunnoSig (TagVal TagDunno) = True
isDunnoSig _                 = False

-- | Extend the tag environment.
extendSigEnv :: TagEnv p -> [(Id,TagSig)] -> TagEnv p
extendSigEnv env@(TE { te_env = sig_env }) bndrs
  = env { te_env = extendVarEnvList sig_env bndrs }
