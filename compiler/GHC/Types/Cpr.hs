{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Types for the Constructed Product Result lattice. "GHC.Core.Opt.CprAnal"
-- and "GHC.Core.Opt.WorkWrap.Utils" are its primary customers via
-- 'GHC.Types.Id.idCprInfo'.
module GHC.Types.Cpr (
    Cpr, botCpr, topCpr, lubCpr, applyCpr, lamCpr, lamsCpr, asConCpr,
    expandConFieldsCpr, pruneDeepCpr, dropNonBotCpr, trimCprToTerm,
    CprSig, topCprSig, lamsCprSig, mkBindCprSig, seqCprSig,
    cprTransformSig, cprTransformDataConWork, argCprsFromStrictSig
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Types.Basic
import GHC.Types.Demand
import GHC.Types.Termination
import GHC.Types.Unbox
import GHC.Core.DataCon
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Utils.Binary
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Data.Maybe

import Data.Coerce

import GHC.Driver.Ppr
_ = pprTrace -- Tired of commenting out GHC.Driver.Ppr

--------
-- * Cpr

newtype Cpr
  = Cpr_ (CloShape Cpr)
  deriving (Binary, Eq)

-- | Normalises the nested CPR info according to
-- > TopSh === LamSh topCpr
-- Because CPR only cares if ultimately we see a data constructor. A 'Lam' can
-- be eta-expanded independently of whether it cancels away in the body by way
-- of an 'App'.
normCprShape :: CloShape Cpr -> CloShape Cpr
normCprShape (LamSh cpr) | cpr == topCpr = TopSh
normCprShape sh                          = sh

pattern Cpr :: CloShape Cpr -> Cpr
pattern Cpr sh <- (Cpr_ sh)
  where
    Cpr sh = Cpr_ (normCprShape sh)
{-# COMPLETE Cpr #-}

botCpr :: Cpr
botCpr = Cpr BotSh

topCpr :: Cpr
topCpr = Cpr TopSh

lubCpr :: Cpr -> Cpr -> Cpr
lubCpr (Cpr sh1) (Cpr sh2) = Cpr (lubCloShape lubCpr sh1 sh2)

conCpr :: DataCon -> [Cpr] -> Cpr
conCpr dc cprs = Cpr (ConSh (dataConTag dc) cprs)

pruneDeepCpr :: Int -> Cpr -> Cpr
pruneDeepCpr depth = coerce (pruneCloShape pruneDeepCpr depth)

-- | Split for the given 'ConTag' if the 'Cpr' is of 'ConSh' or 'BotSh'.
splitConCpr :: ConTag -> Int -> Cpr -> Maybe [Cpr]
splitConCpr tag arty (Cpr sh) = splitConSh botCpr tag arty sh

expandConFieldsCpr :: DataCon -> Cpr -> [Cpr]
expandConFieldsCpr dc c =
  splitConCpr (dataConTag dc) (dataConRepArity dc) c
    `orElse` replicate (dataConRepArity dc) topCpr

splitLamCpr :: Cpr -> Maybe Cpr
splitLamCpr (Cpr BotSh)     = Just botCpr
splitLamCpr (Cpr (LamSh c)) = Just c
splitLamCpr _               = Nothing

applyCpr :: Cpr -> Cpr
applyCpr cpr
  | cpr == botCpr           = botCpr
  | Cpr (LamSh cpr') <- cpr = cpr'
  | otherwise               = topCpr

lamCpr :: Cpr -> Cpr
lamCpr cpr = Cpr (LamSh cpr)

lamsCpr :: Arity -> Cpr -> Cpr
lamsCpr n cpr = iterate (Cpr. LamSh) cpr !! n

dropNonBotCpr :: Cpr -> Cpr
-- See Note [CPR for thunks]
dropNonBotCpr c
  | is_bot_fun c = c -- Don't forget bot => error thunks should have CPR
  | otherwise    = topCpr
  where
    is_bot_fun (Cpr BotSh)     = True
    is_bot_fun (Cpr (LamSh c)) = is_bot_fun c
    is_bot_fun _               = False

-- | Trims deep CPR information as soon as there is a single 'MightDiverge' in
-- the way.
trimCprToTerm :: Term -> Cpr -> Cpr
-- See Note [Trimming CPR signatures according to Term]
trimCprToTerm term cpr
  -- No further MightDiverge in the way, stop trimming
  | term == botTerm
  = cpr
  -- Handle (expansion to) ConSh
  | Term Terminates (ConSh t terms) <- term
  , Just cprs <- splitConCpr t (length terms) cpr
  = Cpr (ConSh t (zipWith trimCprToTerm terms cprs))
  | Cpr (ConSh t cprs) <- cpr
  , (Terminates, terms) <- splitConTerm t (length cprs) term
  = Cpr (ConSh t (zipWith trimCprToTerm terms cprs))
  -- Handle (expansion to) LamSh
  | Term Terminates (LamSh t) <- term
  , Just c <- splitLamCpr cpr
  = Cpr (LamSh (trimCprToTerm t c))
  | Cpr (LamSh c) <- cpr
  , (Terminates, t) <- splitLamTerm term
  = Cpr (LamSh (trimCprToTerm t c))
  -- Otherwise top
  | otherwise
  = topCpr

seqCpr :: Cpr -> ()
seqCpr = coerce (seqCloShape seqCpr)

asConCpr :: Cpr -> Maybe (ConTag, [Cpr])
-- This is the key function consulted by WW
asConCpr (Cpr (ConSh t cprs)) = Just (t, cprs)
asConCpr _                    = Nothing

-----------
-- * CprSig

-- | Not just 'Cpr', because of the interface needed in "GHC.Core.Opt.CprAnal".
-- We need to take some care to trim CPR results from function bodies when
-- we turn them into a 'CprSig'. See 'mkBindCprSig'.
type CprSig = Sig Cpr

topCprSig :: CprSig
topCprSig = Sig topCpr

lamsCprSig :: Arity -> Cpr -> CprSig
lamsCprSig arty res_cpr = Sig $ lamsCpr arty res_cpr

seqCprSig :: CprSig -> ()
seqCprSig = coerce seqCpr

-- | Turns a 'Cpr' of a function body into a signature that is unleashable
-- at call sites of the particular 'Arity' and minimum call 'Demand'.
--
-- See Note [Trimming CPR signatures according to Term]
-- and Note [Improving CPR by considering strictness demand from call sites],
-- as well as Note [Arity trimming for CPR signatures],
-- all in "GHC.Core.Opt.CprAnal".
mkBindCprSig :: Arity -> Demand -> Term -> Cpr -> CprSig
mkBindCprSig arty fun_demand rhs_term rhs_cpr =
  -- pprTrace "mkBindCprSig" (vcat [ppr arty, ppr fun_demand, ppr call_sd, ppr rhs_term, ppr rhs_cpr, ppr demanded_term, ppr trimmed_term, ppr final_cpr]) $
  Sig final_cpr
  where
    -- See Note [Improving CPR by considering strictness demand from call sites]
    -- Figure out the *least sub-demand* put on the function body by all call sites.
    -- Sub-demand, because we can assume at least seq demand on the body.
    (_card1 :* fn_sd) = fun_demand -- how the function was called
    (_card2, body_sd) = peelManyCalls arty fn_sd
    call_sd = mkCalledOnceDmds arty body_sd -- the minimum demand put on by a
                                            -- single, saturated call
    (_, demanded_term) = forceTerm call_sd rhs_term
    -- See Note [Arity trimming for CPR signatures]
    trimmed_term = trimTermToArity arty demanded_term
    -- See Note [Trimming CPR signatures according to Term]
    final_cpr = trimCprToTerm trimmed_term rhs_cpr

-- | Compared to 'termTransformSig', this one is not higher-order!
cprTransformSig :: CprSig -> Cpr
cprTransformSig = getSig

-- | Get a 'Cpr' for a 'DataCon', given 'Cpr's for its fields.
cprTransformDataConWork :: DataCon -> [Cpr] -> Cpr
-- What about DataCon *wrappers*? See Note [CPR for DataCon wrappers]
cprTransformDataConWork con args
  | null (dataConExTyCoVars con)  -- No existentials
  , wkr_arity <= mAX_CPR_SIZE
  , args `lengthIs` wkr_arity
  -- , pprTrace "cprTransformDataConWork" (ppr con <+> ppr wkr_arity <+> ppr args) True
  = getSig $ lamsCprSig wkr_arity (conCpr con args)
  | otherwise
  = topCpr
  where
    wkr_arity = dataConRepArity con

mAX_CPR_SIZE :: Arity
mAX_CPR_SIZE = 10
-- We do not treat very big tuples as CPR-ish:
--      a) for a start we get into trouble because there aren't
--         "enough" unboxed tuple types (a tiresome restriction,
--         but hard to fix),
--      b) more importantly, big unboxed tuples get returned mainly
--         on the stack, and are often then allocated in the heap
--         by the caller.  So doing CPR for them may in fact make
--         things worse.

-- | Produces 'Cpr's according to how strict argument types will be unboxed.
-- Examples:
--
--   * A head-strict demand `S` on `Int` would translate to `c1(*)`
--   * A tuple demand `S(S,L)` on `(Int, Bool)` would translate to `c1(c1(*),*)`
--   * A tuple demand `S(S,L)` on `(a , Bool)` would translate to `c1(*,*)`,
--     because the unboxing strategy would not unbox the `a`.
argCprsFromStrictSig :: UnboxingStrategy Demand -> [Type] -> StrictSig -> [Cpr]
argCprsFromStrictSig want_to_unbox arg_tys sig
  = zipWith go arg_tys (fst (splitStrictSig sig))
  where
    go ty dmd
      | Unbox (DataConPatContext { dcpc_dc = dc, dcpc_tc_args = tc_args }) dmds
          <- want_to_unbox ty dmd
      -- No existentials; see Note [Which types are unboxed?])
      -- Otherwise we'd need to call dataConRepInstPat here and thread a UniqSupply
      , null (dataConExTyCoVars dc)
      , let arg_tys = map scaledThing (dataConInstArgTys dc tc_args)
      = conCpr dc (zipWith go arg_tys dmds)
      | otherwise
      = topCpr

-- | Formats `lamCpr (conCpr 3 [conCpr 5 [topCpr], conCpr 2 [topCpr, botCpr])`
--   as      `L3(5,2(,b))`.
instance Outputable Cpr where
  ppr (Cpr l) = pprCloShape (char 'b') (any (/= topCpr)) l
