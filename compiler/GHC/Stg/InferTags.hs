{-# LANGUAGE TypeFamilies, DataKinds, GADTs, FlexibleInstances #-}

{-# LANGUAGE UndecidableInstances #-}
 -- To permit: type instance XLet 'Tagged = XLet 'Vanilla



module GHC.Stg.InferTags ( inferTags ) where

import GHC.Prelude

import GHC.Core.DataCon
import GHC.Types.Id
import GHC.Stg.Syntax
import GHC.Types.Basic ( Arity )
import GHC.Types.Var.Env
import GHC.Core (AltCon(..))
import Data.List (mapAccumL)
import GHC.Utils.Outputable
import GHC.Utils.Misc( zipWithEqual )


{- Note [Tag inference]
~~~~~~~~~~~~~~~~~~~~~~~
The purpose of this pass is to attach to every binder a flag
to indicate whether or not it is "properly tagged".  A binder
is properly tagged if it is guaranteed:
 - to point to a heap-allocated value
 - and to have the tag of the value encoded in the pointer

  inferTags :: [GenStgTopBinding 'Vanilla] -> [GenStgTopBinding 'Tagged]

For example
  let x = Just y in ...

Here x will be properly tagged: it will point to the heap-allocated
values for (Just y), and the tag-bits of the pointer will encode
the tag for Just.
-}

{- *********************************************************************
*                                                                      *
                         Main inference algorithm
*                                                                      *
********************************************************************* -}

inferTags :: [GenStgTopBinding 'Vanilla] -> [GenStgTopBinding 'Tagged]
inferTags binds = snd (mapAccumL inferTagTopBind initEnv binds)

-----------------------
inferTagTopBind :: TagEnv 'Vanilla -> GenStgTopBinding 'Vanilla
                -> (TagEnv 'Vanilla, GenStgTopBinding 'Tagged)
inferTagTopBind env (StgTopStringLit id bs)
  = (env, StgTopStringLit id bs)
inferTagTopBind env (StgTopLifted bind)
  = (env', StgTopLifted bind')
  where
    (env', bind') = inferTagBind env bind


-----------------------
inferTagExpr :: TagEnv p -> GenStgExpr p -> (TagInfo, GenStgExpr 'Tagged)
inferTagExpr env (StgApp fun args)
  = (info, StgApp fun args)
  where
    info | Just (TagSig arity res_info) <- lookupSig env fun
         , arity == length args  -- Saturated
         = res_info
         | otherwise
         = TagDunno

inferTagExpr env (StgConApp con ext args tys)
  = (info, StgConApp con ext' args tys)
  where
    ext' = case te_ext env of ExtEqEv -> ext
    info | isUnboxedTupleDataCon con
         = TagTuple (map (lookupInfo env) args)
         | otherwise
         = TagDunno

inferTagExpr _ (StgLit l)
  = (TagDunno, StgLit l)

inferTagExpr env (StgTick tick body)
  = (info, StgTick tick body')
  where
    (info, body') = inferTagExpr env body

inferTagExpr _ (StgOpApp op args ty)
  = -- Do any primops guarantee to return a properly tagged value?
    -- I think not.  Ditto foreign calls.
    (TagDunno, StgOpApp op args ty)

inferTagExpr env (StgLet ext bind body)
  = (info, StgLet ext' bind' body')
  where
    ext' = case te_ext env of ExtEqEv -> ext
    (env', bind') = inferTagBind env bind
    (info, body') = inferTagExpr env' body

inferTagExpr env (StgLetNoEscape ext bind body)
  = (info, StgLetNoEscape ext' bind' body')
  where
    ext' = case te_ext env of ExtEqEv -> ext
    (env', bind') = inferTagBind env bind
    (info, body') = inferTagExpr env' body

inferTagExpr env (StgCase scrut bndr ty alts)
  | [(DataAlt con, bndrs, rhs)] <- alts
  , isUnboxedTupleDataCon con
  , TagTuple infos <- scrut_info
  , let bndrs' = zipWithEqual "inferTagExpr" mk_bndr bndrs infos
        mk_bndr bndr info = (getBinderId env bndr, TagSig 0 info)
        alt_env = extendSigEnv env bndrs'
        (info, rhs') = inferTagExpr alt_env rhs
  = (info, StgCase scrut' (noSig env bndr) ty [(DataAlt con, bndrs', rhs')])

  | otherwise
  = ( foldr combineAltInfo TagProper infos
    , StgCase scrut' bndr' ty alts')
  where
    (scrut_info, scrut') = inferTagExpr env scrut
    bndr' = (getBinderId env bndr, TagSig 0 TagProper)
    alt_env = extendSigEnv env [bndr']
    (infos, alts')
       = unzip [ (info, (con, bndrs', rhs'))
               | (con, bndrs, rhs) <- alts
               , let (info, rhs') = inferTagExpr alt_env rhs
                     bndrs' = addAltBndrInfo env con bndrs ]

addAltBndrInfo :: TagEnv p -> AltCon -> [BinderP p] -> [BinderP 'Tagged]
addAltBndrInfo env (DataAlt con) bndrs
  = zipWithEqual "inferTagAlt" mk_bndr bndrs (dataConRepStrictness con)
  where
    mk_bndr bndr NotMarkedStrict = noSig env bndr
    mk_bndr bndr MarkedStrict    = (getBinderId env bndr, TagSig 0 TagProper)

addAltBndrInfo env _ bndrs = map (noSig env) bndrs

-----------------------------
inferTagBind :: TagEnv p -> GenStgBinding p -> (TagEnv p, GenStgBinding 'Tagged)
inferTagBind env (StgNonRec bndr rhs)
  = (env', StgNonRec (id, sig) rhs')
  where
    id   = getBinderId env bndr
    env' = extendSigEnv env [(id, sig)]
    (sig,rhs') = inferTagRhs env rhs

inferTagBind env (StgRec pairs)
  = (env { te_env = sig_env }, StgRec pairs')
  where
    (bndrs, rhss)     = unzip pairs
    ids               = map (getBinderId env) bndrs
    init_sigs         = map initSig rhss
    (sig_env, pairs') = go env init_sigs rhss

    go :: forall q. TagEnv q -> [TagSig] -> [GenStgRhs q]
                 -> (TagSigEnv, [((Id,TagSig), GenStgRhs 'Tagged)])
    go env sigs rhss
       | sigs == sigs' = (te_env rhs_env, bndrs `zip` rhss')
       | otherwise     = go env' sigs' rhss'
       where
         bndrs = ids `zip` sigs
         rhs_env = extendSigEnv env bndrs
         (sigs', rhss') = unzip (map (inferTagRhs rhs_env) rhss)
         env' = makeTagged env

initSig :: GenStgRhs p -> TagSig
-- Initial signature for the fixpoint loop
initSig (StgRhsCon {})                = TagSig 0              TagProper
initSig (StgRhsClosure _ _ _ bndrs _) = TagSig (length bndrs) TagProper

-----------------------------
inferTagRhs :: TagEnv p -> GenStgRhs p -> (TagSig, GenStgRhs 'Tagged)
inferTagRhs env (StgRhsClosure ext cc upd bndrs body)
  = (TagSig arity info, StgRhsClosure ext' cc upd bndrs' body')
  where
    ext' = case te_ext env of ExtEqEv -> ext
    (info, body') = inferTagExpr env body
    arity = length bndrs
    bndrs' = map (noSig env) bndrs

inferTagRhs _ (StgRhsCon cc con cn ticks args)
  = (TagSig 0 TagProper, StgRhsCon cc con cn ticks args)


{- *********************************************************************
*                                                                      *
                         Supporting data types
*                                                                      *
********************************************************************* -}

type instance BinderP 'Tagged = (Id, TagSig)
type instance XConApp      'Tagged = XConApp      'Vanilla
type instance XLet         'Tagged = XLet         'Vanilla
type instance XLetNoEscape 'Tagged = XLetNoEscape 'Vanilla
type instance XRhsClosure  'Tagged = XRhsClosure  'Vanilla

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
                   , te_ext :: ExtEqEv (XConApp p) (XLet p)
                                       (XLetNoEscape p) (XRhsClosure p) }

getBinderId :: TagEnv p -> BinderP p -> Id
getBinderId = te_get

-- This tiresome value is a proof that the extension fields
-- have the same type in pass p as in pass Tagged
-- ToDo: write a Note to explain properly
data ExtEqEv a b c d where
  ExtEqEv :: ExtEqEv (XConApp 'Tagged)      (XLet 'Tagged)
                     (XLetNoEscape 'Tagged) (XRhsClosure 'Tagged)

initEnv :: TagEnv 'Vanilla
initEnv = TE { te_env = emptyVarEnv
             , te_get = \x -> x
             , te_ext = ExtEqEv }

makeTagged :: TagEnv p -> TagEnv 'Tagged
makeTagged env = TE { te_env = te_env env
                    , te_get = fst
                    , te_ext = ExtEqEv }

data TagSig  -- The signature for each binding
  = TagSig Arity TagInfo
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
  = TagDunno

extendSigEnv :: TagEnv p -> [(Id,TagSig)] -> TagEnv p
extendSigEnv env@(TE { te_env = sig_env }) bndrs
  = env { te_env = extendVarEnvList sig_env bndrs }
