{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Types for the Constructed Product Result lattice. "GHC.Core.Opt.CprAnal"
-- and "GHC.Core.Opt.WorkWrap.Utils" are its primary customers via 'GHC.Types.Id.idCprInfo'.
module GHC.Types.Cpr (
    TerminationFlag (Terminates),
    Cpr, topCpr, conCpr, whnfTermCpr, divergeCpr, lubCpr, asConCpr,
    CprType (..), topCprType, whnfTermCprType, conCprType, lubCprType, lubCprTypes,
    pruneDeepCpr, markOptimisticConCprType, splitConCprTy, applyCprTy, abstractCprTy,
    abstractCprTyNTimes, ensureCprTyArity, trimCprTy,
    forceCprTy, forceCpr, bothCprType,
    cprTransformDataConSig, UnboxingStrategy, cprTransformSig, argCprTypesFromStrictSig,
    CprSig (..), mkCprSig, mkCprSigForArity,
    topCprSig, seqCprSig
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Types.Basic
import GHC.Types.Demand
import GHC.Core.DataCon
import GHC.Core.Type
import GHC.Driver.Session
import GHC.Utils.Binary
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import Data.Tuple (swap)
import qualified Data.Semigroup as Semigroup
import Control.Monad.Trans.Writer.CPS
import Control.Monad (zipWithM, zipWithM_)

--------------
-- * Levitated

data Levitated a
  = Bot
  | Levitate a
  | Top
  deriving Eq

seqLevitated :: (a -> ()) -> Levitated a -> ()
seqLevitated seq_a (Levitate a) = seq_a a
seqLevitated _     _            = ()

liftLevitated :: (a -> Levitated b) -> Levitated a -> Levitated b
liftLevitated _ Top          = Top
liftLevitated _ Bot          = Bot
liftLevitated f (Levitate a) = f a

lubLevitated :: (a -> a -> Levitated a) -> Levitated a -> Levitated a -> Levitated a
lubLevitated _     Bot          l            = l
lubLevitated _     l            Bot          = l
lubLevitated _     Top          _            = Top
lubLevitated _     _            Top          = Top
lubLevitated lub_a (Levitate a) (Levitate b) = lub_a a b

---------------
-- * KnownShape

data KnownShape r = Con !ConTag [r]
  deriving Eq

seqKnownShape :: (r -> ()) -> KnownShape r -> ()
seqKnownShape seq_r (Con _ args) = foldr (seq . seq_r) () args

lubKnownShape :: (r -> r -> r) -> KnownShape r -> KnownShape r -> Levitated (KnownShape r)
lubKnownShape lub_r (Con t1 args1) (Con t2 args2)
  | t1 == t2, args1 `equalLength` args2
  = Levitate (Con t1 (zipWith lub_r args1 args2))
lubKnownShape _ _ _
  = Top

pruneKnownShape :: (Int -> r -> r) -> Int -> KnownShape r -> Levitated (KnownShape r)
pruneKnownShape _       0     _            = Top
pruneKnownShape prune_r depth (Con t args) = Levitate (Con t   (map (prune_r (depth - 1)) args))

---------------
-- * Optimism

data Optimism
  = Conservative
  | Optimistic
  deriving Eq

lubOptimism :: Optimism -> Optimism -> Optimism
lubOptimism Optimistic   _            = Optimistic
lubOptimism _            Optimistic   = Optimistic
lubOptimism Conservative Conservative = Conservative

----------------
-- * Termination

data TerminationFlag
  = Terminates
  | MightDiverge
  deriving Eq

lubTermFlag :: TerminationFlag -> TerminationFlag -> TerminationFlag
lubTermFlag MightDiverge _            = MightDiverge
lubTermFlag _            MightDiverge = MightDiverge
lubTermFlag Terminates   Terminates   = Terminates

data Termination
  = Term_ !TerminationFlag !(Levitated (KnownShape Termination))
  -- Don't use 'Term_', use 'Term' instead! Otherwise the derived Eq instance
  -- is broken.
  deriving Eq

-- | Normalise the nested termination info according to
-- > Top === Levitate (Con t [topTerm..])
-- > Bot === Levitate (Con t [botTerm..])
-- (Note that this identity doesn't hold for CPR!)
normTermShape :: Levitated (KnownShape Termination) -> Levitated (KnownShape Termination)
normTermShape (Levitate (Con _ fields))
  | all (== topTerm) fields = Top
  | all (== botTerm) fields = Bot
normTermShape l_sh         = l_sh

pattern Term :: TerminationFlag -> Levitated (KnownShape Termination) -> Termination
pattern Term tf l <- (Term_ tf l)
  where
    Term tf l = Term_ tf (normTermShape l)

{-# COMPLETE Term #-}

botTerm :: Termination
botTerm = Term Terminates Bot

topTerm :: Termination
topTerm = Term MightDiverge Top

lubTerm :: Termination -> Termination -> Termination
lubTerm (Term tf1 l_sh1) (Term tf2 l_sh2)
  = Term (lubTermFlag tf1 tf2)
         (lubLevitated (lubKnownShape lubTerm) l_sh1 l_sh2)

pruneDeepTerm :: Int -> Termination -> Termination
pruneDeepTerm depth (Term tf (Levitate sh))
  = Term tf (pruneKnownShape pruneDeepTerm depth sh)
pruneDeepTerm _     term                    = term

seqTerm :: Termination -> ()
seqTerm (Term _ l) = seqLevitated (seqKnownShape seqTerm) l

-- Reasons for design:
--  * We want to share between Cpr and Termination, so KnownShape
--  * Cpr is different from Termination in that we give up once one result
--    isn't constructed
--  * That is: For Termination we might or might not have nested info,
--    independent of termination of the current level. This is why Maybe
--    So, i.e. when we return a function (or newtype there-of) we'd have
--    something like @Termination Terminates Nothing@. We know evaluation
--    terminates, but we don't have any information on shape.
--    In fact, it's the same as
--  * Factoring Termination this way (i.e., TerminationFlag x shape) means less
--    duplication
-- Alternative: Interleave everything. Looks like this:
-- data Blub (b::Bool)
--   = NoCpr (Blub 'False)
--   | Cpr  ConTag TerminationFlag (Blub b)
--   | Term ConTag TerminationFlag (Blub b)
--   | TopBlub
--   | BotBlub
--  + More compact
--  + No Maybe (well, not here, still in Termination)
--  + Easier to handle in WW: Termination and Cpr encode compatible shape info
--    by construction
--  - Harder to understand: NoCpr means we can still have Termination info
--  - Spreads Termination stuff between two lattices
-- ... Probably not such a good idea, after all.

--------
-- * Cpr

data Cpr
  = Cpr !Optimism !TerminationFlag !(Levitated (KnownShape Cpr))
  | NoMoreCpr_ !Termination
  deriving Eq

pattern NoMoreCpr :: Termination -> Cpr
pattern NoMoreCpr t <- (NoMoreCpr_ t)
  where
    NoMoreCpr (Term MightDiverge Top) = topCpr
    NoMoreCpr (Term Terminates   Bot) = botCpr
    NoMoreCpr t                       = NoMoreCpr_ t

{-# COMPLETE Cpr, NoMoreCpr #-}

botCpr :: Cpr
botCpr = Cpr Conservative Terminates Bot

topCpr :: Cpr
topCpr = Cpr Conservative MightDiverge Top

whnfTermCpr :: Cpr
whnfTermCpr = Cpr Conservative Terminates Top

-- | Used as
--
--   * The initial CPR of a recursive function in fixed-point iteration
--   * The CPR of 'undefined'/'error'/other sources of divergence.
--
-- We assume that evaluation to WHNF surely diverges (so 'MightDiverge'), but
-- are optimistic about all CPR and nested termination information. I.e., we
-- assume that returned tuple components terminate rapidly and construct a
-- product.
divergeCpr :: Cpr
divergeCpr = Cpr Conservative MightDiverge Bot

conCpr :: ConTag -> [Cpr] -> Cpr
conCpr t fs = Cpr Conservative Terminates (Levitate (Con t fs))

optimisticConCpr :: ConTag -> [Cpr] -> Cpr
optimisticConCpr t fs = Cpr Optimistic Terminates (Levitate (Con t fs))

-- | Forget encoded CPR info, but keep termination info.
forgetCpr :: Cpr -> Termination
forgetCpr (NoMoreCpr t) = t
forgetCpr (Cpr _ tf l_sh) = Term tf (normTermShape (liftLevitated go l_sh))
  where
    go (Con t fields) = Levitate (Con t (map forgetCpr fields))

lubCpr :: Cpr -> Cpr -> Cpr
lubCpr (Cpr op1 tf1 l_sh1) (Cpr op2 tf2 l_sh2)
  = Cpr (lubOptimism op1 op2)
        (lubTermFlag tf1 tf2)
        (lubLevitated (lubKnownShape lubCpr) l_sh1 l_sh2)
lubCpr cpr1            cpr2
  = NoMoreCpr (lubTerm (forgetCpr cpr1) (forgetCpr cpr2))

trimCpr :: Cpr -> Cpr
trimCpr cpr@(Cpr _ _ Bot) = cpr -- don't trim away bottom (we didn't do so before Nested CPR) TODO: Explain; CPR'ing for the error case
trimCpr cpr               = NoMoreCpr (forgetCpr cpr)

pruneDeepCpr :: Int -> Cpr -> Cpr
pruneDeepCpr depth (Cpr op tf (Levitate sh)) = Cpr op tf (pruneKnownShape pruneDeepCpr depth sh)
pruneDeepCpr depth (NoMoreCpr t)             = NoMoreCpr (pruneDeepTerm depth t)
pruneDeepCpr _     cpr                       = cpr

asConCpr :: Cpr -> Maybe (ConTag, [Cpr])
asConCpr (Cpr _ tf (Levitate (Con t fields)))
  | Terminates <- tf = Just (t, fields)
asConCpr _           = Nothing

seqCpr :: Cpr -> ()
seqCpr (Cpr _ _ l)   = seqLevitated (seqKnownShape seqCpr) l
seqCpr (NoMoreCpr t) = seqTerm t

------------
-- * CprType

-- TODO: Maybe formulate CprType like this?
-- data CprType' = TerminatingCall CprType | Ret Cpr Termination
--   deriving Eq
-- data CprType = CT (Levitated CprType')
--   deriving Eq

-- | The abstract domain \(A_t\) from the original 'CPR for Haskell' paper.
data CprType
  = CprType
  { ct_arty :: !Arity
  -- ^ Number of value arguments the denoted expression eats before returning
  -- the 'ct_cpr'
  , ct_cpr  :: !Cpr
  -- ^ 'Cpr' eventually unleashed when applied to 'ct_arty' arguments
  }

instance Eq CprType where
  a == b =  ct_cpr a  == ct_cpr b
         && (ct_arty a == ct_arty b || isTopCprType a)

isTopCprType :: CprType -> Bool
isTopCprType (CprType _ cpr) = cpr == topCpr

topCprType :: CprType
topCprType = CprType 0 topCpr

botCprType :: CprType
botCprType = CprType 0 botCpr

whnfTermCprType :: CprType
whnfTermCprType = CprType 0 whnfTermCpr

lubCprType :: CprType -> CprType -> CprType
lubCprType ty1@(CprType n1 cpr1) ty2@(CprType n2 cpr2)
  | ct_cpr ty1 == botCpr && n1 <= n2 = ty2
  | ct_cpr ty2 == botCpr && n2 <= n1 = ty1
  -- There might be non-bottom CPR types with mismatching arities.
  -- Consider test DmdAnalGADTs. We want to return topCpr in these cases.
  -- Returning topCprType is a safe default.
  | n1 == n2
  = CprType n1 (lubCpr cpr1 cpr2)
  | otherwise
  = topCprType

lubCprTypes :: [CprType] -> CprType
lubCprTypes = foldl' lubCprType botCprType

extractArgCprAndTermination :: [CprType] -> [Cpr]
extractArgCprAndTermination = map go
  where
    go (CprType 0 cpr) = cpr
    -- we didn't give it enough arguments, so terminates rapidly
    go _               = topCpr

conCprType :: ConTag -> [CprType] -> CprType
conCprType con_tag args = CprType 0 (conCpr con_tag cprs)
  where
    cprs = extractArgCprAndTermination args

markOptimisticConCprType :: DataCon -> CprType -> CprType
markOptimisticConCprType dc _ty@(CprType n cpr)
  = -- pprTraceWith "markOptimisticConCpr" (\ty' -> ppr _ty $$ ppr ty') $
    ASSERT2( n == 0, ppr _ty ) CprType 0 (optimisticConCpr con_tag fields)
  where
    con_tag   = dataConTag dc
    wkr_arity = dataConRepArity dc
    fields    = case cpr of
      NoMoreCpr (Term _ (Levitate (Con t terms)))
        | con_tag == t       -> map NoMoreCpr terms
      NoMoreCpr (Term _ Bot) -> replicate wkr_arity (NoMoreCpr botTerm)
      Cpr _ _ (Levitate (Con t cprs))
        | con_tag == t       -> cprs
      Cpr _ _ Bot            -> replicate wkr_arity botCpr
      _                      -> replicate wkr_arity topCpr

splitConCprTy :: DataCon -> CprType -> Maybe [Cpr]
splitConCprTy dc (CprType 0 (Cpr _ _ l))
  | Bot <- l
  = Just (replicate (dataConRepArity dc) botCpr)
  | Levitate (Con t fields) <- l
  , dataConTag dc == t
  = Just fields
splitConCprTy _  _
  = Nothing

applyCprTy :: CprType -> CprType
applyCprTy (CprType n cpr)
  | n > 0         = CprType (n-1) cpr
  | cpr == botCpr = botCprType
  | otherwise     = topCprType

abstractCprTy :: CprType -> CprType
abstractCprTy = abstractCprTyNTimes 1

abstractCprTyNTimes :: Arity -> CprType -> CprType
abstractCprTyNTimes n ty@(CprType m cpr)
  | isTopCprType ty = topCprType
  | otherwise       = CprType (n+m) cpr

ensureCprTyArity :: Arity -> CprType -> CprType
ensureCprTyArity n ty@(CprType m _)
  | m == n    = ty
  | otherwise = topCprType

trimCprTy :: CprType -> CprType
trimCprTy (CprType arty cpr) = CprType arty (trimCpr cpr)

zonkOptimisticCprTy :: Int -> CprType -> CprType
zonkOptimisticCprTy max_depth _ty@(CprType arty cpr)
  = -- pprTraceWith "zonkOptimisticCprTy" (\ty' -> ppr max_depth <+> ppr _ty <+> ppr ty') $
    CprType arty (zonk max_depth cpr)
  where
    -- | The Int is the amount of "fuel" left; when it reaches 0, we no longer
    -- turn OptimisticCpr into Cpr, but into NoMoreCpr.
    zonk :: Int -> Cpr -> Cpr
    zonk n (Cpr op tf sh)
      | n > 0 || op == Conservative
      = Cpr Conservative tf (liftLevitated (Levitate . zonk_sh (n-1)) sh)
    zonk _ cpr
      = NoMoreCpr (forgetCpr cpr)

    zonk_sh :: Int -> KnownShape Cpr -> KnownShape Cpr
    zonk_sh n (Con t fields) = Con t (map (zonk n) fields)

-- | Abusing the Monoid instance of 'Semigroup.Any' to track a
-- 'TerminationFlag'.
newtype TerminationM a = TerminationM (Writer Semigroup.Any a)
  deriving (Functor, Applicative, Monad)

runTerminationM :: TerminationM a -> (TerminationFlag, a)
runTerminationM (TerminationM act) = case runWriter act of
  (a, Semigroup.Any True)  -> (MightDiverge, a)
  (a, Semigroup.Any False) -> (Terminates, a)

noteTermFlag :: TerminationFlag -> TerminationM ()
noteTermFlag MightDiverge = TerminationM (tell (Semigroup.Any True))
noteTermFlag Terminates   = TerminationM (tell (Semigroup.Any False))

-- | Forces possibly deep 'Termination' info of a 'CprType' according to
-- incoming 'ArgStr'. If there's any possibility that this 'MightDiverge',
-- return that.
forceCprTy :: ArgStr -> CprType -> (TerminationFlag, CprType)
-- TODO: This doesn't consider strict fields yet, I think
forceCprTy arg_str ty = runTerminationM (forceCprTyM arg_str ty)

forceCprTyM :: ArgStr -> CprType -> TerminationM CprType
forceCprTyM arg_str ty = go (toStrDmd arg_str) ty
  where
    go (Lazy, _  ) ty              = return ty
    go (_   , str) (CprType 0 cpr) = CprType 0 <$> forceCprM (Str str) cpr
    go (_   , str) ty              =
      abstractCprTy <$> go (swap (peelStrCall str)) (applyCprTy ty)

forceCprM :: ArgStr -> Cpr -> TerminationM Cpr
forceCprM Lazy      t                = return t
forceCprM arg_str   (NoMoreCpr t)    = NoMoreCpr <$> forceTermM arg_str t
forceCprM (Str str) (Cpr op tf l_sh) = do
  -- 1. discharge head strictness by noting the term flag
  noteTermFlag tf
  -- 2. discharge *nested* strictness on available nested info
  l_sh' <- case (str, l_sh) of
    (_, Bot)     -> return Bot
    (HeadStr, _) -> return l_sh
    (SCall _, _) -> do
      -- We called something which might have evaluated an incoming argument
      -- about which we have no info. Assume we diverge and continue with the
      -- original nested info.
      noteTermFlag MightDiverge
      return l_sh
    (HyperStr, _) -> do
      -- l_sh is not Bot, so this might diverge and we leave behind something
      -- completely evaluated
      noteTermFlag MightDiverge
      return Bot
    (SProd args, _) -> do
      let fields = case l_sh of
            Top                     -> replicate (length args) topCpr
            Levitate (Con t fields) -> ASSERT( t == fIRST_TAG && fields `equalLength` args ) fields
#if __GLASGOW_HASKELL__ <= 810
            Bot                     -> panic "impossible, wait for GHC 8.12"
#endif
      fields' <- zipWithM forceCprM args fields
      return (Levitate (Con fIRST_TAG fields'))
  return (Cpr op Terminates l_sh')

forceTermM :: ArgStr -> Termination -> TerminationM Termination
forceTermM Lazy      t              = return t
forceTermM (Str str) (Term tf l_sh) = do
  -- 1. discharge head strictness by noting the term flag
  noteTermFlag tf
  -- 2. discharge *nested* strictness on available nested info
  l_sh' <- case (str, l_sh) of
    (_, Bot)     -> return Bot
    (HeadStr, _) -> return l_sh
    (SCall _, _) -> do
      -- We called something which might have evaluated an incoming argument
      -- about which we have no info. Assume we diverge and continue with the
      -- original nested info.
      noteTermFlag MightDiverge
      return l_sh
    (HyperStr, _) -> do
      -- l_sh is not Bot, so this might diverge and we leave behind something
      -- completely evaluated
      noteTermFlag MightDiverge
      return Bot
    (SProd args, _) -> do
      let fields = case l_sh of
            Top                     -> replicate (length args) topTerm
            Levitate (Con t fields) -> ASSERT( t == fIRST_TAG && fields `equalLength` args ) fields
#if __GLASGOW_HASKELL__ <= 810
            Bot                     -> panic "impossible, wait for GHC 8.12"
#endif
      fields' <- zipWithM forceTermM args fields
      return (normTermShape (Levitate (Con fIRST_TAG fields')))
  return (Term Terminates l_sh')

forceCpr :: ArgStr -> Cpr -> (TerminationFlag, Cpr)
forceCpr str cpr = runTerminationM (forceCprM str cpr)

-- | 'lubTerm's the given outer @TerminationFlag@ on the @CprType@s 'ct_term'.
bothCprType :: CprType -> TerminationFlag -> CprType
-- If tf = Terminates, it's just 'id'.
-- If tf = MightDiverge, it will only set the WHNF layer to MightDiverge,
-- leaving nested termination info (e.g. on product components) intact.
bothCprType ct Terminates   = ct
bothCprType ct MightDiverge = ct { ct_cpr = shallowDivCpr (ct_cpr ct) }

shallowDivCpr :: Cpr -> Cpr
shallowDivCpr (NoMoreCpr (Term _ l_sh)) = NoMoreCpr (Term MightDiverge l_sh)
shallowDivCpr (Cpr op _ l_sh)           = Cpr op MightDiverge l_sh

seqCprType :: CprType -> ()
seqCprType (CprType _ cpr) = seqCpr cpr

--------------
-- * CprSig

-- | The arity of the wrapped 'CprType' is the arity at which it is safe
-- to unleash. See Note [Understanding DmdType and StrictSig] in "GHC.Types.Demand"
-- INVARIANT: The wrapped CprType never has 'OptimisticCpr' somewhere.
newtype CprSig = CprSig { getCprSig :: CprType }
  deriving (Eq, Binary)

-- | Turns a 'CprType' computed for the particular 'Arity' into a 'CprSig'
-- unleashable at that arity. See Note [Understanding DmdType and StrictSig] in
-- "GHC.Types.Demand"
mkCprSigForArity :: DynFlags -> Arity -> CprType -> CprSig
mkCprSigForArity dflags arty
  = CprSig
  . ensureCprTyArity arty
  . zonkOptimisticCprTy (caseBinderCprDepth dflags)

topCprSig :: CprSig
topCprSig = CprSig topCprType

mkCprSig :: Arity -> Cpr -> CprSig
mkCprSig arty cpr = CprSig (CprType arty cpr)

seqCprSig :: CprSig -> ()
seqCprSig (CprSig sig) = seqCprType sig `seq` ()

-- | Get a 'CprType' for a 'DataCon', given 'CprType's for its fields.
cprTransformDataConSig :: DataCon -> [CprType] -> CprType
cprTransformDataConSig con args
  | null (dataConExTyCoVars con)  -- No existentials
  , wkr_arity > 0
  , wkr_arity <= mAX_CPR_SIZE
  , args `lengthIs` wkr_arity
  -- , pprTrace "cprTransformDataConSig" (ppr con <+> ppr wkr_arity <+> ppr args) True
  = abstractCprTyNTimes wkr_arity $ conCprType (dataConTag con) args
  | otherwise -- TODO: Refl binds a coercion. What about these? can we CPR them? I don't see why we couldn't.
  = topCprType
  where
    wkr_arity = dataConRepArity con
    -- Note how we don't handle unlifted args here. That's OK by the let/app
    -- invariant, which specifies that the things we forget to force are ok for
    -- speculation, so exactly what we mean by Terminates.

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

cprTransformSig :: StrictSig -> CprSig -> [CprType] -> CprType
cprTransformSig str_sig (CprSig sig_ty) arg_tys
  | arg_strs <- map getStrDmd $ argDmdsFromStrictSig str_sig
  , arg_strs `leLength` arg_tys
  , arg_tys `lengthIs` ct_arty sig_ty
  -- Maybe we should use resTypeArgDmd instead of strTop here. On the other
  -- hand, I don't think it makes much of a difference; We basically only need
  -- to pad with strTop when str_sig was topSig to begin with.
  , (tf, _) <- runTerminationM $ zipWithM_ forceCprTyM (arg_strs ++ repeat strTop) arg_tys
  = sig_ty `bothCprType` tf
  | otherwise
  = topCprType

-- | We have to be sure that 'cprTransformSig' and 'argCprTypesFromStrictSig'
-- agree in how they compute the 'Demand's for which the 'CprSig' is computed.
-- This function encodes the common (trivial) logic.
argDmdsFromStrictSig :: StrictSig -> [Demand]
argDmdsFromStrictSig = fst . splitStrictSig

type UnboxingStrategy = Type -> Demand -> Maybe (DataCon, [Type], [Demand])

-- | Produces 'CprType's the termination info of which match the given
-- strictness signature. Examples:
--
--   - A head-strict demand @S@ would translate to @#@, a
--   - A tuple demand @S(S,L)@ would translate to @#(#,*)@
--   - A call demand @C(S)@ would translate to @strTop -> #(#,*)@
argCprTypesFromStrictSig :: UnboxingStrategy -> [Type] -> StrictSig -> [CprType]
argCprTypesFromStrictSig want_to_unbox arg_tys sig
  -- TODO: Maybe look at unliftedness from the unboxing strategy, just in case
  -- we e.g. fail to mark an Int# argument as Terminates, which should always be
  -- the case as per the let/app invariant.
  = zipWith go arg_tys (argDmdsFromStrictSig sig)
  where
    go arg_ty arg_dmd
      | Just (dc, arg_tys, arg_dmds) <- want_to_unbox arg_ty arg_dmd
      = conCprType (dataConTag dc) (zipWith go arg_tys arg_dmds)
      | otherwise
      = snd $ forceCprTy (getStrDmd arg_dmd) topCprType

---------------
-- * Outputable

instance Outputable a => Outputable (Levitated a) where
  ppr Bot = char '⊥'
  ppr Top = char 'T'
  ppr (Levitate a) = ppr a

instance Outputable r => Outputable (KnownShape r) where
  ppr (Con t fs) = int t <> pprFields fs

pprFields :: Outputable r => [r] -> SDoc
pprFields fs = parens (pprWithCommas ppr fs)

instance Outputable TerminationFlag where
  ppr MightDiverge = char '*'
  ppr Terminates   = char '#'

instance Outputable Termination where
  ppr (Term tf l) = ppr tf <> case l of
    Top            -> empty
    Bot            -> text "(#..)"
    Levitate shape -> ppr shape

instance Outputable Optimism where
  ppr Optimistic   = char '?'
  ppr Conservative = empty

instance Outputable Cpr where
  ppr (NoMoreCpr t)          = ppr t
  -- I like it better without the special case
  -- ppr (Cpr MightDiverge Top) = empty
  -- ppr (Cpr Terminates   Bot) = char 'b'
  ppr (Cpr op tf l)          = ppr tf <> case l of
    Top            -> empty
    Bot            -> char 'b'
    Levitate shape -> char 'c' <> ppr op <> ppr shape

instance Outputable CprType where
  ppr (CprType arty cpr) = ppr arty <+> ppr cpr

-- | Only print the CPR result
instance Outputable CprSig where
  ppr (CprSig ty) = ppr (ct_cpr ty)

-----------
-- * Binary

instance Binary a => Binary (Levitated a) where
  put_ bh Bot          = putByte bh 0
  put_ bh (Levitate a) = do { putByte bh 1; put_ bh a }
  put_ bh Top          = putByte bh 2
  get  bh = do
    h <- getByte bh
    case h of
      0 -> return Bot
      1 -> Levitate <$> get bh
      2 -> return Top
      _ -> pprPanic "Binary Levitated: Invalid tag" (int (fromIntegral h))

instance Binary r => Binary (KnownShape r) where
  put_ bh (Con t fs) = do { putULEB128 bh t; put_ bh fs }
  get  bh = Con <$> getULEB128 bh <*> get bh

instance Binary TerminationFlag where
  put_ bh Terminates   = put_ bh True
  put_ bh MightDiverge = put_ bh False
  get  bh = do
    b <- get bh
    if b
      then pure Terminates
      else pure MightDiverge

-- | In practice, we should never need to serialise an 'Optimistic' because of
-- the invariant attached to 'CprSig'.
instance Binary Optimism where
  put_ bh Conservative = put_ bh True
  put_ bh Optimistic   = put_ bh False
  get  bh = do
    b <- get bh
    if b
      then pure Conservative
      else pure Optimistic

instance Binary Termination where
  put_ bh (Term tf l) = put_ bh tf >> put_ bh l
  get  bh = Term <$> get bh <*> get bh

instance Binary Cpr where
  put_ bh (Cpr op tf l) = put_ bh True >> put_ bh op >> put_ bh tf >> put_ bh l
  put_ bh (NoMoreCpr t) = put_ bh False >> put_ bh t
  get  bh = do
    b <- get bh
    if b
      then Cpr <$> get bh <*> get bh <*> get bh
      else NoMoreCpr <$> get bh

instance Binary CprType where
  put_ bh (CprType args cpr) = do
    put_ bh args
    put_ bh cpr
  get  bh = CprType <$> get bh <*> get bh
