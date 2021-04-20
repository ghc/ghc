{-# LANGUAGE TypeFamilies, DataKinds, GADTs, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE UndecidableInstances #-}
 -- To permit: type instance XLet 'TaggedSimon = XLet 'Vanilla



{-# OPTIONS_GHC -Wno-unused-imports #-}
module GHC.Stg.InferTags ( inferTags ) where

import GHC.Prelude

import GHC.Core.DataCon
import GHC.Types.Id
import GHC.Stg.Syntax
import GHC.Types.Basic ( Arity, TopLevelFlag(..), RecFlag(..) )
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Core (AltCon(..))
import Data.List (mapAccumL)
import GHC.Utils.Outputable
import GHC.Utils.Misc( zipWithEqual )

import GHC.Stg.InferTags.Types
import GHC.Driver.Ppr

{- Note [Tag inference]
~~~~~~~~~~~~~~~~~~~~~~~
The purpose of this pass is to attach to every binder a flag
to indicate whether or not it is "properly tagged".  A binder
is properly tagged if it is guaranteed:
 - to point to a heap-allocated value
 - and to have the tag of the value encoded in the pointer

  inferTags :: [GenStgTopBinding 'Vanilla] -> [GenStgTopBinding 'TaggedSimon]

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

inferTags :: [GenStgTopBinding 'Vanilla] -> [GenStgTopBinding 'TaggedSimon]
inferTags binds =
  -- pprTrace "Binds" (pprGenStgTopBindings shortStgPprOpts $ binds) $
  snd (mapAccumL inferTagTopBind initEnv binds)

-----------------------
inferTagTopBind :: TagEnv 'Vanilla -> GenStgTopBinding 'Vanilla
                -> (TagEnv 'Vanilla, GenStgTopBinding 'TaggedSimon)
inferTagTopBind env (StgTopStringLit id bs)
  = (env, StgTopStringLit id bs)
inferTagTopBind env (StgTopLifted bind)
  = (env', StgTopLifted bind')
  where
    (env', bind') = inferTagBind TopLevel env bind


-----------------------
inferTagExpr :: TagEnv p -> GenStgExpr p -> (TagInfo, GenStgExpr 'TaggedSimon)
inferTagExpr env (StgApp ext fun args)
  = (info, StgApp noEnterInfo fun args)
  where
    info | Just (TagSig arity res_info) <- lookupSig env fun
         , arity == length args  -- Saturated
         = res_info
         | otherwise
         = TagDunno

inferTagExpr env (StgConApp ext con cn args tys)
  = (info, StgConApp ext' con cn args tys)
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
    (env', bind') = inferTagBind NotTopLevel env bind
    (info, body') = inferTagExpr env' body

inferTagExpr env (StgLetNoEscape ext bind body)
  = (info, StgLetNoEscape ext' bind' body')
  where
    ext' = case te_ext env of ExtEqEv -> ext
    (env', bind') = inferTagBind NotTopLevel env bind
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

addAltBndrInfo :: TagEnv p -> AltCon -> [BinderP p] -> [BinderP 'TaggedSimon]
addAltBndrInfo env (DataAlt con) bndrs
  = zipWithEqual "inferTagAlt" mk_bndr bndrs (dataConRepStrictness con)
  where
    mk_bndr bndr NotMarkedStrict = noSig env bndr
    mk_bndr bndr MarkedStrict    = (getBinderId env bndr, TagSig 0 TagProper)

addAltBndrInfo env _ bndrs = map (noSig env) bndrs

-----------------------------
inferTagBind :: TopLevelFlag -> TagEnv p -> GenStgBinding p -> (TagEnv p, GenStgBinding 'TaggedSimon)
inferTagBind top env (StgNonRec bndr rhs)
  = (env', StgNonRec (id, sig) rhs')
  where
    id   = getBinderId env bndr
    env' = extendSigEnv env [(id, sig)]
    (sig,rhs') = inferTagRhs top [] env rhs

inferTagBind top env (StgRec pairs)
  = (env { te_env = sig_env }, StgRec pairs')
  where
    (bndrs, rhss)     = unzip pairs -- :: ([Id], [GenStgRhs 'Vanilla])
    ids               = map (getBinderId env) bndrs
    init_sigs         = map initSig rhss
    (sig_env, pairs') = go env init_sigs rhss

    go :: forall q. TagEnv q -> [TagSig] -> [GenStgRhs q]
                 -> (TagSigEnv, [((Id,TagSig), GenStgRhs 'TaggedSimon)])
    go env sigs rhss
       | sigs == sigs' = (te_env rhs_env, bndrs `zip` rhss')
       | otherwise     = go env' sigs' rhss'
       where
         bndrs = ids `zip` sigs
         rhs_env = extendSigEnv env bndrs
         (sigs', rhss') = unzip (map (inferTagRhs top ids rhs_env) rhss) -- :: ([TagSig], [GenStgRhs 'TaggedSimon])
         env' = makeTagged env

initSig :: GenStgRhs p -> TagSig
-- Initial signature for the fixpoint loop
initSig StgRhsCon {}                = TagSig 0              TagProper
initSig (StgRhsClosure _ _ _ bndrs _) = TagSig (length bndrs) TagProper

-----------------------------
inferTagRhs :: TopLevelFlag -- ^
  -> [Id] -- ^ List of ids in the recursive group, or [] otherwise
  -> TagEnv p -- ^
  -> GenStgRhs p -- ^
  -> (TagSig, GenStgRhs 'TaggedSimon)
inferTagRhs _top _rec env (StgRhsClosure ext cc upd bndrs body)
  = (TagSig arity info', StgRhsClosure ext' cc upd bndrs' body')
  where
    ext' = case te_ext env of ExtEqEv -> ext
    (info, body') = inferTagExpr env body
    arity = length bndrs
    info'
      | TagProper <- info :: TagInfo
      , arity == 0
      = TagDunno -- It's a thunk!
      | otherwise  = info
    bndrs' = map (noSig env) bndrs

inferTagRhs top grp_ids env (StgRhsCon ext cc con cn ticks args)
-- Top level constructors, which have untagged arguments to strict fields
-- become thunks. Same goes for rhs which are part of a recursive group.
-- We encode this by giving changing RhsCon nodes the info TagDunno
  = let
        strictArgs = getStrictConArgs con args
        strictUntaggedIds = [v | StgVarArg v <- strictArgs
                            , lookupInfo env (StgVarArg v) /= TagProper] :: [Id]

        mkResult x = (TagSig 0 x, StgRhsCon noExtFieldSilent cc con cn ticks args)
    in case () of
        -- _ -> mkResult TagProper
          -- All fields tagged or non-strict
        _ | null strictUntaggedIds -> mkResult TagProper
          -- Non-recursive local let
          | null grp_ids
          , NotTopLevel <- top
          -> mkResult TagProper
          -- Recursive local let, no bindings from grp in args
          | NotTopLevel <- top
          -- Recursive groups can probably get large enough for us
          -- to worry about doing union over lists.
          , mkVarSet grp_ids `disjointVarSet` mkVarSet strictUntaggedIds
          -> mkResult TagProper
          -- Otherwise we have a top level let with untagged args,
          -- or a recursive group where a bindings of the group is
          -- passed into a strict field
          | otherwise -> mkResult TagDunno



