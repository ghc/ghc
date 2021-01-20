{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Types for the Constructed Product Result lattice. "GHC.Core.Opt.CprAnal"
-- and "GHC.Core.Opt.WorkWrap.Utils" are its primary customers via 'GHC.Types.Id.idCprInfo'.
module GHC.Types.Cpr (
    TerminationFlag (Terminates),
    CAT, topCAT, whnfTermCAT, divergeCAT, lubCAT, pruneDeepCAT, asConCAT,
    CprType (..), topCprType, whnfTermCprType, lubCprType, lubCprTypes,
    splitConCprTy, applyCprTy, abstractCprTy, trimCprTy, forceCprTy, bothCprType,
    cprTransformDataConWorkSig, cprTransformSig, argCprTypesFromStrictSig,
    CprSig (..), topCprSig, mkCprSig, mkCprSigForFunRHS, seqCprSig
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Types.Basic
import GHC.Types.Demand
import GHC.Types.Unbox
import GHC.Core.DataCon
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Utils.Binary
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Coerce
import qualified Data.Semigroup as Semigroup
import Control.Monad.Trans.Writer.CPS
import Control.Monad (zipWithM, zipWithM_)

-- import GHC.Driver.Ppr

---------------
-- * KnownShape

data KnownShape r
  = BotSh
  | ConSh !ConTag [r]
  | TopSh
  deriving Eq

seqKnownShape :: (r -> ()) -> KnownShape r -> ()
seqKnownShape seq_r (ConSh _ args) = foldr (seq . seq_r) () args
seqKnownShape _     _              = ()

lubKnownShape :: (r -> r -> r) -> KnownShape r -> KnownShape r -> KnownShape r
lubKnownShape _     BotSh            sh               = sh
lubKnownShape _     sh               BotSh            = sh
lubKnownShape _     TopSh            _                = TopSh
lubKnownShape _     _                TopSh            = TopSh
lubKnownShape lub_r (ConSh t1 args1) (ConSh t2 args2)
  | t1 == t2, args1 `equalLength` args2
  = (ConSh t1 (zipWith lub_r args1 args2))
  | otherwise
  = TopSh

pruneKnownShape :: (Int -> r -> r) -> Int -> KnownShape r -> KnownShape r
pruneKnownShape _       0     _              = TopSh
pruneKnownShape prune_r depth (ConSh t args) = ConSh t (map (prune_r (depth - 1)) args)
pruneKnownShape _       _     sh             = sh

splitConSh :: r -> r -> ConTag -> Int -> KnownShape r -> [r]
splitConSh bot _top _   arty BotSh = replicate arty bot
splitConSh _   _    tag arty (ConSh t fields)
  | tag == t
  , length fields == arty -- See Note [CPR types and unsafeCoerce]
  = fields
splitConSh _   top  _   arty  _    = replicate arty top

{- Note [CPR types and unsafeCoerce]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unsafe coercions may lead to looking into a CPR type analysed for a different
type than the type of the expression a case scrutinises. Example (like T16893):

  data T = T Int
  data U = U Int Int
  ... (case unsafeCoerce (U 1 2 :: U) of
        T i -> ...) ...

Although we can only derive bogus in these situations, we shouldn't try to
unpack the 2 field CPR type from the scrutinee into a 1 field type matching
the pattern. That led to a deliberate panic when calling @zipEqual@ in
'CprAnal.cprAnalAlt'. Nothing bad was happening, just a precautionary
panic on T16893.
-}

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
  = Term_ !TerminationFlag !(KnownShape Termination)
  -- ^ Don't use 'Term_', use 'Term' instead! Otherwise the derived Eq instance
  -- is broken.
  deriving Eq

-- | Normalises the nested termination info according to
-- > TopSh === ConSh t [topTerm..]
-- > BotSh === ConSh t [botTerm..]
-- (Note that this identity doesn't hold for CPR!):
normTermShape :: KnownShape Termination -> KnownShape Termination
normTermShape (ConSh _ fields)
  | all (== topTerm) fields = TopSh
  | all (== botTerm) fields = BotSh
normTermShape sh            = sh

pattern Term :: TerminationFlag -> KnownShape Termination -> Termination
pattern Term tf s <- (Term_ tf s)
  where
    -- 'normTermShape' is main point of this synonym. The first 4 case alts
    -- are only for interning purposes.
    Term tf (normTermShape -> sh) = case (tf, sh) of
      (Terminates,   BotSh) -> botTerm
      (Terminates,   TopSh) -> whnfTerm
      (MightDiverge, BotSh) -> divergeTerm
      (MightDiverge, TopSh) -> topTerm
      (tf,           sh   ) -> Term_ tf sh
{-# COMPLETE Term #-}

topTerm :: Termination
topTerm = Term_ MightDiverge TopSh

botTerm :: Termination
botTerm = Term_ Terminates BotSh

whnfTerm :: Termination
whnfTerm = Term_ Terminates TopSh

divergeTerm :: Termination
-- Just because we intern all the other combinations
divergeTerm = Term_ MightDiverge BotSh

lubTerm :: Termination -> Termination -> Termination
lubTerm (Term tf1 sh1) (Term tf2 sh2) =
  Term (lubTermFlag tf1 tf2) (lubKnownShape lubTerm sh1 sh2)

pruneDeepTerm :: Int -> Termination -> Termination
pruneDeepTerm depth (Term tf sh) =
  Term tf (pruneKnownShape pruneDeepTerm depth sh)

splitConTerm :: ConTag -> Int -> KnownShape Termination -> [Termination]
splitConTerm = splitConSh botTerm topTerm

seqTerm :: Termination -> ()
seqTerm (Term _ l) = seqKnownShape seqTerm l

-- Reasons for design:
--  * We want to share between Cpr and Termination, so KnownShape
--  * Cpr is different from Termination in that we give up once one result
--    isn't constructed
--  * It is useful to keep non-terminating CPR, because strictness analysis
--    might have found out that we are strict in the non-terminating component
--    anyway, or there's a later seq on it.
--    But that also means we are too optimistic in the following example:
--      h1 :: Int -> Maybe Int
--      h1 n = Just (sum [0..n])
--      {-# NOINLINE h1 #-}
--
--      h2 :: Int -> Int
--      h2 n | n < 0     = n
--           | otherwise = case h1 n of Just blah -> blah
--      {-# NOINLINE h2 #-}
--    We'd wrongfully give h2 the CPR property here. The case on h1 won't cancel away.
--  * These are the key values to support, in case of a redesign. Write them down first:
--      topTerm, botTerm, whnfTerm, topCpr, botCpr, conCpr

--------
-- * Cpr

newtype Cpr
  = Cpr (KnownShape Cpr)
  deriving Eq

botCpr :: Cpr
botCpr = Cpr BotSh

topCpr :: Cpr
topCpr = Cpr TopSh

lubCpr :: Cpr -> Cpr -> Cpr
lubCpr (Cpr sh1) (Cpr sh2) = Cpr (lubKnownShape lubCpr sh1 sh2)

pruneDeepCpr :: Int -> Cpr -> Cpr
pruneDeepCpr depth = coerce (pruneKnownShape pruneDeepCpr depth)

splitConCpr :: ConTag -> Int -> KnownShape Cpr -> [Cpr]
splitConCpr = splitConSh botCpr topCpr

seqCpr :: Cpr -> ()
seqCpr = coerce (seqKnownShape seqCpr)

--------
-- * CAT

-- | Joint lattice of 'Termination' and 'Cpr'.
-- (C)pr (A)nd (T)ermination, hence \"CAT\".
data CAT = CAT !Termination !Cpr
  deriving Eq

unzipCAT :: [CAT] -> ([Termination], [Cpr])
unzipCAT = unzip . map (\(CAT t c) -> (t, c))

-- | Like 'Control.Arrow.(***)' for 'CAT'.
liftCAT :: (Termination -> Termination) -> (Cpr -> Cpr) -> CAT -> CAT
liftCAT ft fc (CAT t c) = CAT (ft t) (fc c)

topCAT :: CAT
topCAT = CAT topTerm topCpr

botCAT :: CAT
botCAT = CAT botTerm botCpr

whnfTermCAT :: CAT
whnfTermCAT = CAT whnfTerm topCpr

-- | Used as
--
--   * The initial CPR of a recursive function in fixed-point iteration
--   * The CPR of 'undefined'/'error'/other sources of divergence.
--
-- We assume that evaluation to WHNF surely diverges (so 'MightDiverge'), but
-- are optimistic about all CPR and nested termination information. I.e., we
-- assume that returned tuple components terminate rapidly and construct a
-- product.
divergeCAT :: CAT
divergeCAT = CAT divergeTerm botCpr

conCAT :: ConTag -> [CAT] -> CAT
conCAT t fs = CAT (Term Terminates (ConSh t terms)) (Cpr (ConSh t cprs))
  where
    (terms, cprs) = unzipCAT fs

asConCAT :: CAT -> Maybe (ConTag, [CAT])
-- This is the key function consulted by WW
-- Note that it completely ignores the termination flag. CPR is assumed to be
-- correct. TODO Write Note about it
asConCAT (CAT (Term _ tsh) (Cpr (ConSh t cprs)))
  | let terms = splitConTerm t (length cprs) tsh
  , let !cats = zipWith CAT terms cprs
  = Just (t, cats)
asConCAT _
  = Nothing

lubCAT :: CAT -> CAT -> CAT
lubCAT (CAT t1 c1) (CAT t2 c2) = CAT (lubTerm t1 t2) (lubCpr c1 c2)

-- | Trims deep CPR information as soon as there is a single 'MightDiverge' in
-- the way.
takeCprWhileTerminates :: Termination -> Cpr -> Cpr
-- See Note [Trimming CPR signatures according to Termination]
takeCprWhileTerminates term cpr
  | term == botTerm || cpr == botCpr = cpr -- Don't trim away bottom, we still
                                           -- want to unbox e.g. error thunks
  | Term Terminates (ConSh t terms) <- term
  , Cpr csh <- cpr
  , let cprs = splitConCpr t (length terms) csh
  = Cpr (ConSh t (zipWith takeCprWhileTerminates terms cprs))
  | otherwise
  = topCpr

-- | Trims CPR information (but keeps termination information) for e.g. thunks
trimCpr :: CAT -> CAT
trimCpr (CAT term cpr) = CAT term (takeCprWhileTerminates topTerm cpr)

pruneDeepCAT :: Int -> CAT -> CAT
pruneDeepCAT depth = liftCAT (pruneDeepTerm depth) (pruneDeepCpr depth)

seqCAT :: CAT -> ()
seqCAT (CAT t c) = seqTerm t `seq` seqCpr c

------------
-- * CprType

-- | The abstract domain \(A_t\) from the original 'CPR for Haskell' paper.
data CprType
  = CprType
  { ct_arty :: !Arity
  -- ^ Number of value arguments the denoted expression eats before returning
  -- the 'ct_cat'
  , ct_cat  :: !CAT
  -- ^ 'CAT' eventually unleashed when applied to 'ct_arty' arguments
  }

instance Eq CprType where
  a == b =  ct_cat a  == ct_cat b
         && (ct_arty a == ct_arty b || isTopCprType a)

isTopCprType :: CprType -> Bool
isTopCprType (CprType _ cpr) = cpr == topCAT

topCprType :: CprType
topCprType = CprType 0 topCAT

botCprType :: CprType
botCprType = CprType 0 botCAT

whnfTermCprType :: CprType
whnfTermCprType = CprType 0 whnfTermCAT

lubCprType :: CprType -> CprType -> CprType
lubCprType ty1@(CprType n1 cat1) ty2@(CprType n2 cat2)
  | ct_cat ty1 == botCAT && n1 <= n2 = ty2
  | ct_cat ty2 == botCAT && n2 <= n1 = ty1
  -- There might be non-bottom CPR types with mismatching arities.
  -- Consider test DmdAnalGADTs. We want to return topCpr in these cases.
  -- Returning topCprType is a safe default.
  | n1 == n2
  = CprType n1 (lubCAT cat1 cat2)
  | otherwise
  = topCprType

lubCprTypes :: [CprType] -> CprType
lubCprTypes = foldl' lubCprType botCprType

extractArgCprAndTermination :: [CprType] -> [CAT]
extractArgCprAndTermination = map go
  where
    go (CprType 0 cat) = cat
    -- we didn't give it enough arguments, so terminates rapidly
    go _               = whnfTermCAT

conCprType :: DataCon -> [CprType] -> CprType
conCprType dc args = CprType 0 (conCAT con_tag cats)
  where
    con_tag = dataConTag dc
    cats = addDataConTermination dc $ extractArgCprAndTermination args

addDataConTermination :: DataCon -> [CAT] -> [CAT]
-- See Note [No unboxed tuple for single, unlifted transit var]
addDataConTermination con cats
  = zipWithEqual "addDataConTermination" add cats strs
  where
    strs = dataConRepStrictness con
    add cat str | isMarkedStrict str = snd $ forceCAT topSubDmd cat -- like seq
                | otherwise          = cat

splitConCprTy :: DataCon -> CprType -> Maybe [CAT]
splitConCprTy dc (CprType 0 (CAT (Term Terminates tsh) (Cpr csh)))
  | let tag = dataConTag dc
  , let arty = dataConRepArity dc
  = Just $! zipWith CAT (splitConTerm tag arty tsh) (splitConCpr tag arty csh)
splitConCprTy _  _                                                =
  Nothing

applyCprTy :: CprType -> CprType
applyCprTy = applyCprTyNTimes 1

applyCprTyNTimes :: Arity -> CprType -> CprType
applyCprTyNTimes n (CprType m cat)
  | m >= n        = CprType (m-n) cat
  | cat == botCAT = botCprType
  | otherwise     = topCprType

abstractCprTy :: CprType -> CprType
abstractCprTy = abstractCprTyNTimes 1

abstractCprTyNTimes :: Arity -> CprType -> CprType
abstractCprTyNTimes n ty@(CprType m cat)
  | isTopCprType ty = topCprType
  | otherwise       = CprType (n+m) cat

trimCprTy :: CprType -> CprType
trimCprTy (CprType arty cat) = CprType arty (trimCpr cat)

----------------------------------------
-- * Forcing 'Termination' with 'Demand'
--
-- See Note [Rapid termination for strict binders]

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
-- See Note [Rapid termination for strict binders]
forceCprTy :: Demand -> CprType -> (TerminationFlag, CprType)
forceCprTy dmd ty = runTerminationM (idIfLazy forceCprTyM dmd ty)

-- | Lifts a 'TerminaionM' action from 'SubDemand's to 'Demand's by returning
-- the original argument if the 'Demand' is not strict.
-- Follows the rule "Don't force if lazy!".
idIfLazy :: (SubDemand -> cpr -> TerminationM cpr) -> Demand -> cpr -> TerminationM cpr
idIfLazy k (n :* sd) cpr
  | isStrict n = k sd cpr
  | otherwise  = return cpr

forceTerm :: SubDemand -> Termination -> (TerminationFlag, Termination)
forceTerm sd term = runTerminationM (forceTermM sd term)

forceCprTyM :: SubDemand -> CprType -> TerminationM CprType
forceCprTyM sd ty
  | let arity = ct_arty ty
  , (m, body_sd) <- peelManyCalls arity sd
  , isStrict m
  , CprType body_arty body_cat  <- applyCprTyNTimes arity ty
  = abstractCprTyNTimes arity . CprType body_arty <$> forceCATM body_sd body_cat
  | otherwise
  = return ty

forceCATM :: SubDemand -> CAT -> TerminationM CAT
forceCATM sd (CAT term cpr) = flip CAT cpr <$> forceTermM sd term

forceTermM :: SubDemand -> Termination -> TerminationM Termination
forceTermM sd (Term tf sh) = do
  -- 1. discharge head strictness by noting the term flag
  noteTermFlag tf
  -- 2. discharge *nested* strictness on available nested info
  sh' <- case (sh, sd) of
    (BotSh, _) -> return BotSh
    (ConSh t fields, viewProd (length fields) -> Just ds) | t == fIRST_TAG -> do
      fields' <- zipWithM (idIfLazy forceTermM) ds fields
      return (ConSh fIRST_TAG fields')
    (TopSh, Prod ds) -> do
      fields' <- mapM (flip (idIfLazy forceTermM) topTerm) ds
      return (ConSh fIRST_TAG fields')
    _ -> return sh -- just don't force anything
  return (Term Terminates sh')

forceCAT :: SubDemand -> CAT -> (TerminationFlag, CAT)
forceCAT sd cpr = runTerminationM (forceCATM sd cpr)

-- | 'lubTerm's the given outer @TerminationFlag@ on the @CprType@s 'ct_term'.
bothCprType :: CprType -> TerminationFlag -> CprType
-- If tf = Terminates, it's just 'id'.
-- If tf = MightDiverge, it will only set the WHNF layer to MightDiverge,
-- leaving nested termination info (e.g. on product components) intact.
bothCprType ct Terminates   = ct
bothCprType ct MightDiverge = ct { ct_cat = liftCAT shallowDivTerm id (ct_cat ct) }

shallowDivTerm :: Termination -> Termination
shallowDivTerm (Term _ sh) = Term MightDiverge sh

seqCprType :: CprType -> ()
seqCprType (CprType _ cat) = seqCAT cat

--------------
-- * CprSig

-- | The arity of the wrapped 'CprType' is the arity at which it is safe
-- to unleash. See Note [Understanding DmdType and StrictSig] in "GHC.Types.Demand"
newtype CprSig = CprSig { getCprSig :: CprType }
  deriving (Eq, Binary)

-- | Turns a 'CprType' of a function body into a signature that is unleashable
-- at call sites of the particular 'Arity' and minimum call 'Demand'.
--
-- See Note [Trimming CPR signatures according to Termination]
-- and Note [Improving CPR by considering strictness demand from call sites],
-- as well as Note [Arity trimming for CPR signatures],
-- all in "GHC.Core.Opt.CprAnal".
mkCprSigForFunRHS :: Arity -> Demand -> CprType -> CprSig
mkCprSigForFunRHS arty fun_demand ty
  | ct_arty ty /= arty = topCprSig -- See Note [Arity trimming for CPR signatures]
  | otherwise          = CprSig ty{ ct_cat = CAT whnf_term final_cpr }
  where
    CAT whnf_term whnf_cpr = ct_cat ty
    -- See Note [Improving CPR by considering strictness demand from call sites]
    -- Figure out the *least sub-demand* put on the function body by all call sites.
    -- Sub-demand, because we can assume at least seq demand on the body.
    (_card1 :* fn_sd) = fun_demand -- how the function was called
    (_card2, body_sd) = peelManyCalls arty fn_sd
    (_, demanded_term) = forceTerm body_sd whnf_term
    -- See Note [Trimming CPR signatures according to Termination]
    final_cpr = takeCprWhileTerminates demanded_term whnf_cpr

topCprSig :: CprSig
topCprSig = CprSig topCprType

mkCprSig :: Arity -> CAT -> CprSig
mkCprSig arty cat = CprSig (CprType arty cat)

seqCprSig :: CprSig -> ()
seqCprSig (CprSig sig) = seqCprType sig `seq` ()

-- | Get a 'CprType' for a 'DataCon', given 'CprType's for its fields.
cprTransformDataConWorkSig :: DataCon -> [CprType] -> CprType
-- What about DataCon *wrappers*? See Note [CPR for DataCon wrappers]
cprTransformDataConWorkSig con args
  | null (dataConExTyCoVars con)  -- No existentials
  , wkr_arity <= mAX_CPR_SIZE
  , args `lengthIs` wkr_arity
  -- , pprTrace "cprTransformDataConWorkSig" (ppr con <+> ppr wkr_arity <+> ppr args) True
  = abstractCprTyNTimes wkr_arity $ conCprType con args
  | otherwise
  = topCprType
  where
    wkr_arity = dataConRepArity con
    -- Note how we don't say 'MightDiverge' for returned CprType, although
    -- evaluation of unlifted args might in theory diverge. But
    --   * That's OK by the let/app invariant, which specifies that the
    --     things we forget to force are ok for speculation, so we should find
    --     that each argument 'Terminates' anyway.
    --   * Data constructor workers are in fact lazy! Evaluation is done by the
    --     (now inlined) wrapper.

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
-- See Note [Rapid termination for strict binders] in CprAnal
cprTransformSig str_sig (CprSig sig_ty) arg_tys
  | dmds <- argDmdsFromStrictSig str_sig
  , dmds `leLength` arg_tys
  , arg_tys `lengthIs` ct_arty sig_ty
  -- See Note [Rapid termination for strict binders]
  -- NB: 'dmds' doesn't account for strict fields, see
  -- Note [Add demands for strict constructors]. We don't have to, either: All
  -- forcing will be done by the call to the DataCon wrapper.
  , (tf, _) <- runTerminationM $ zipWithM_ (idIfLazy forceCprTyM) (dmds ++ repeat topDmd) arg_tys
  = -- pprTrace "cprTransformSig:ok" (ppr str_sig <+> ppr sig_ty <+> ppr arg_tys <+> ppr tf)
    sig_ty `bothCprType` tf
  | otherwise
  = -- pprTrace "cprTransformSig:topSig" (ppr str_sig <+> ppr sig_ty <+> ppr arg_tys)
    topCprType

-- | We have to be sure that 'cprTransformSig' and 'argCprTypesFromStrictSig'
-- agree in how they compute the 'Demand's for which the 'CprSig' is computed.
-- This function encodes the common (trivial) logic.
argDmdsFromStrictSig :: StrictSig -> [Demand]
argDmdsFromStrictSig = fst . splitStrictSig

-- | Produces 'CprType's the termination info of which match the given
-- strictness signature. Examples:
--
--   - A head-strict demand `S` would translate to `#`
--   - A tuple demand `S(S,L)` would translate to `#(#,*)`
--   - A call demand `C(S)` would translate to `. -> #(#,*)`
argCprTypesFromStrictSig :: UnboxingStrategy Demand -> [Type] -> StrictSig -> [CprType]
argCprTypesFromStrictSig want_to_unbox arg_tys sig
  = zipWith go arg_tys (argDmdsFromStrictSig sig)
  where
    go ty dmd
      | Unbox (DataConPatContext { dcpc_dc = dc, dcpc_tc_args = tc_args }) dmds
          <- want_to_unbox ty dmd
      -- No existentials; see Note [Which types are unboxed?])
      -- Otherwise we'd need to call dataConRepInstPat here and thread a UniqSupply
      , null (dataConExTyCoVars dc)
      , let arg_tys = map scaledThing (dataConInstArgTys dc tc_args)
      = conCprType dc (zipWith go arg_tys dmds)
      | otherwise
      = snd $ forceCprTy dmd topCprType

---------------
-- * Outputable

pprFields :: Outputable r => [r] -> SDoc
pprFields fs = parens (pprWithCommas ppr fs)

instance Outputable TerminationFlag where
  ppr MightDiverge = char '*'
  ppr Terminates   = char '#'

instance Outputable Termination where
  ppr (Term tf l) = ppr tf <> case l of
    TopSh -> empty
    BotSh -> text "(#..)"
    ConSh t fs -> ppr t <> pprFields fs

instance Outputable Cpr where
  -- I like it better without the special case
  ppr (Cpr l) = case l of
    TopSh -> empty
    BotSh -> char 'b' -- Maybe just 'c'?
    ConSh t fs -> char 'c' <> ppr t <> pprFields fs

instance Outputable CAT where
  ppr (CAT t c) = parens (ppr t <> comma <> ppr c)

instance Outputable CprType where
  ppr (CprType arty cat) =
    ppr arty <> text "->" <> ppr cat

-- | Only print the CPR result
instance Outputable CprSig where
  ppr (CprSig ty) = ppr (ct_cat ty)

-----------
-- * Binary

instance Binary r => Binary (KnownShape r) where
  put_ bh BotSh   = putByte bh 0
  put_ bh (ConSh t fs) = putByte bh 1 *> putULEB128 bh t *> put_ bh fs
  put_ bh TopSh   = putByte bh 2
  get  bh = do
    h <- getByte bh
    case h of
      0 -> return BotSh
      1 -> ConSh <$> getULEB128 bh <*> get bh
      2 -> return TopSh
      _ -> pprPanic "Binary KnownShape: Invalid tag" (int (fromIntegral h))

instance Binary TerminationFlag where
  put_ bh Terminates   = put_ bh True
  put_ bh MightDiverge = put_ bh False
  get  bh = do
    b <- get bh
    if b
      then pure Terminates
      else pure MightDiverge

instance Binary Termination where
  put_ bh (Term tf l) = put_ bh tf >> put_ bh l
  get  bh = Term <$> get bh <*> get bh

instance Binary Cpr where
  put_ bh (Cpr sh) = put_ bh sh
  get  bh = Cpr <$> get bh

instance Binary CAT where
  put_ bh (CAT t c) = put_ bh t *> put_ bh c
  get  bh = CAT <$> get bh <*> get bh

instance Binary CprType where
  put_ bh (CprType args cat) = put_ bh args *> put_ bh cat
  get  bh = CprType <$> get bh <*> get bh
