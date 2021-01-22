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
    TermFlag (Terminates),
    Term, botTerm, topTerm, lubTerm, whnfTerm, divergeTerm, pruneDeepTerm, expandConFieldsTerm, applyTerm, lamTerm, bothTerm, forceTerm, markDivergingTerm,
    termTransformDataConWork, termTransformSig, argTermsFromStrictSig,
    Cpr, botCpr, topCpr, lubCpr, asConCpr, pruneDeepCpr, expandConFieldsCpr, applyCpr, lamCpr, trimCprToTerm, dropNonBotCpr,
    cprTransformDataConWork, cprTransformSig, argCprsFromStrictSig,
    Sig (..),
    CprSig, topCprSig, mkCprSigForResCpr, mkCprSigForFunRHS, seqCprSig,
    TermSig, topTermSig, mkTermSigForResTerm, mkTermSigForFunRHS, moreTermThanArity, seqTermSig
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
import GHC.Data.Maybe

import Data.Coerce
import qualified Data.Semigroup as Semigroup
import Control.Monad.Trans.Writer.CPS
import Control.Monad (zipWithM)

import GHC.Driver.Ppr

_ = pprTrace -- Tired of commenting out GHC.Driver.Ppr

---------------
-- * CloShape

-- | The runtime shape of a heap closure.
data CloShape r
  = BotSh
  | LamSh !r
  | ConSh !ConTag [r]
  | TopSh
  deriving Eq

seqCloShape :: (r -> ()) -> CloShape r -> ()
seqCloShape seq_r (ConSh _ args) = foldr (seq . seq_r) () args
seqCloShape seq_r (LamSh r)      = seq_r r
seqCloShape _     _              = ()

lubCloShape :: (r -> r -> r) -> CloShape r -> CloShape r -> CloShape r
lubCloShape _     BotSh            sh               = sh
lubCloShape _     sh               BotSh            = sh
lubCloShape lub_r (LamSh r1) (LamSh r2)             = LamSh (lub_r r1 r2)
lubCloShape lub_r (ConSh t1 args1) (ConSh t2 args2)
  | t1 == t2, args1 `equalLength` args2
  = (ConSh t1 (zipWith lub_r args1 args2))
lubCloShape _     _                _                = TopSh

-- | Prune the nesting depth of data structures
pruneCloShape :: (Int -> r -> r) -> Int -> CloShape r -> CloShape r
pruneCloShape _       0     _              = TopSh
pruneCloShape prune_r depth (ConSh t args) = ConSh t (map (prune_r (depth - 1)) args)
pruneCloShape prune_r depth (LamSh r)      = LamSh (prune_r depth r) -- only apply depth to data structures!
pruneCloShape _       _     sh             = sh

-- | Return fields for a 'ConSh' with the given 'ConTag' and arity, the
-- approriate number of 'bot's for a 'BotSh', or 'Nothing'.
splitConSh :: r -> ConTag -> Arity -> CloShape r -> Maybe [r]
splitConSh bot _   arty BotSh = Just $! replicate arty bot
splitConSh _   tag arty (ConSh t fields)
  | tag == t
  , length fields == arty -- See Note [CPR types and unsafeCoerce]
  = Just fields
splitConSh _   _   _    _     = Nothing

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

data TermFlag
  = Terminates
  | MightDiverge
  deriving Eq

lubTermFlag :: TermFlag -> TermFlag -> TermFlag
lubTermFlag MightDiverge _            = MightDiverge
lubTermFlag _            MightDiverge = MightDiverge
lubTermFlag Terminates   Terminates   = Terminates

data Term
  = Term_ !TermFlag !(CloShape Term)
  -- ^ Don't use 'Term_', use 'Term' instead! Otherwise the derived Eq instance
  -- is broken.
  deriving Eq

-- | Normalises the nested termination info according to
-- > TopSh === ConSh t [topTerm..]
-- > BotSh === ConSh t [botTerm..]
-- > TopSh === LamSh topTerm
-- > BotSh === LamSh botTerm
-- (Note that this identity doesn't hold for CPR!):
normTermShape :: CloShape Term -> CloShape Term
normTermShape (ConSh _ fields)
  | all (== topTerm) fields = TopSh
  | all (== botTerm) fields = BotSh
normTermShape (LamSh t)
  | t == topTerm            = TopSh
  | t == botTerm            = BotSh
normTermShape sh            = sh

pattern Term :: TermFlag -> CloShape Term -> Term
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

topTerm :: Term
topTerm = Term_ MightDiverge TopSh

botTerm :: Term
botTerm = Term_ Terminates BotSh

whnfTerm :: Term
whnfTerm = Term_ Terminates TopSh

-- | Used as
--
--   * The initial 'Term' of a recursive function in fixed-point iteration
--   * The 'Term' of 'undefined'/'error'/other sources of divergence.
--
-- We assume that evaluation to WHNF surely diverges (so 'MightDiverge'), but
-- are optimistic about nested termination information. I.e., we
-- assume that returned tuple components terminate rapidly.
divergeTerm :: Term
divergeTerm = Term_ MightDiverge BotSh

-- | Mark the given 'Term' as 'MightDiverge' at the given 'Arity'.
-- Recursive functions are marked like this, so we never consider them
-- terminating.
markDivergingTerm :: Arity -> Term -> Term
markDivergingTerm 0 (Term _ sh) = Term MightDiverge sh
markDivergingTerm n t           = case splitLamTerm t of
  (tf, t') -> Term tf (LamSh (markDivergingTerm (n-1) t'))

conTerm :: DataCon -> [Term] -> Term
conTerm dc terms  = Term Terminates (ConSh (dataConTag dc) terms')
  where
    -- See Note [No unboxed tuple for single, unlifted transit var]
    terms' = zipWithEqual "conTerm" add terms strs
    strs = dataConRepStrictness dc
    add t str | isMarkedStrict str = snd $ forceTerm seqSubDmd t
              | otherwise          = t

lubTerm :: Term -> Term -> Term
lubTerm (Term tf1 sh1) (Term tf2 sh2) =
  Term (lubTermFlag tf1 tf2) (lubCloShape lubTerm sh1 sh2)

pruneDeepTerm :: Int -> Term -> Term
pruneDeepTerm depth (Term tf sh) =
  Term tf (pruneCloShape pruneDeepTerm depth sh)

-- | Return fields of a 'ConSh' of the given 'ConTag' and arity, and make
-- up approprate field 'Term's for the other cases ('BotSh' -> 'botTerm',
-- all others 'topTerm'). Also return outer 'TermFlag'.
splitConTerm :: ConTag -> Arity -> Term -> (TermFlag, [Term])
splitConTerm tag arty (Term tf sh) =
  (,) tf $! splitConSh botTerm tag arty sh `orElse` replicate arty topTerm

expandConFieldsTerm :: DataCon -> Term -> [Term]
expandConFieldsTerm dc t = snd $ splitConTerm (dataConTag dc) (dataConRepArity dc) t

splitLamTerm :: Term -> (TermFlag, Term)
splitLamTerm (Term tf BotSh)     = (tf, botTerm)
splitLamTerm (Term tf (LamSh t)) = (tf, t)
splitLamTerm (Term tf _)         = (tf, topTerm)

applyTerm :: Term -> Term
applyTerm term@(Term tf sh)
  | term == botTerm = botTerm
  | LamSh t <- sh   = t `bothTerm` tf
  | otherwise       = topTerm

lamTerm :: Term -> Term
lamTerm t = Term Terminates (LamSh t)

-- | 'lubTerm's the given outer 'TermFlag' onto the 'Term'.
bothTerm :: Term -> TermFlag -> Term
-- If tf = Terminates, it's just 'id'.
-- If tf = MightDiverge, it will only set the WHNF layer to MightDiverge,
-- leaving nested termination info (e.g. on product components) intact.
bothTerm term        Terminates   = term
bothTerm (Term _ sh) MightDiverge = Term MightDiverge sh

-- | Makes sure there are exactly arity many successive, terminating 'LamSh's,
-- pushing intermittent 'MightDiverge's inwards.
trimTermToArity :: Arity -> Term -> Term
trimTermToArity n (Term tf sh) = go n tf sh
  where
    go 0 tf (LamSh _            ) = Term tf TopSh
    go 0 tf sh                    = Term tf sh
    go n _  TopSh                 = lam_go n MightDiverge TopSh
    go n tf ConSh{}               = go     n tf           TopSh
    go n tf (LamSh (Term tf' sh)) = lam_go n (lubTermFlag tf tf') sh
    go n tf BotSh                 = lam_go n tf           BotSh
    lam_go n tf sh = Term Terminates (LamSh (go (n-1) tf sh))

seqTerm :: Term -> ()
seqTerm (Term _ l) = seqCloShape seqTerm l

-- Reasons for design:
--  * We want to share between Cpr and Term, so CloShape
--  * Cpr is different from Term in that we give up once one result
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
  = Cpr_ (CloShape Cpr)
  deriving Eq

-- | Normalises the nested CPR info according to
-- > TopSh === LamSh topCpr
-- Because CPR only cares if ultimately we see a data constructor.
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

----------------------------------------
-- * Forcing 'Term' with 'Demand'
--
-- See Note [Rapid termination for strict binders]

-- | Abusing the Monoid instance of 'Semigroup.Any' to track a
-- 'TermFlag'.
newtype TerminationM a = TerminationM (Writer Semigroup.Any a)
  deriving (Functor, Applicative, Monad)

runTerminationM :: TerminationM a -> (TermFlag, a)
runTerminationM (TerminationM act) = case runWriter act of
  (a, Semigroup.Any True)  -> (MightDiverge, a)
  (a, Semigroup.Any False) -> (Terminates, a)

noteTermFlag :: TermFlag -> TerminationM ()
noteTermFlag MightDiverge = TerminationM (tell (Semigroup.Any True))
noteTermFlag Terminates   = TerminationM (tell (Semigroup.Any False))

-- | Lifts a 'TerminaionM' action from 'SubDemand's to 'Demand's by returning
-- the original argument if the 'Demand' is not strict.
-- Follows the rule "Don't force if lazy!".
idIfLazy :: (SubDemand -> cpr -> TerminationM cpr) -> Demand -> cpr -> TerminationM cpr
idIfLazy k (n :* sd) cpr
  | isStrict n = k sd cpr
  | otherwise  = return cpr

-- | Forces possibly deep 'Term' info according to incoming 'ArgStr'.
-- If there's any possibility that this 'MightDiverge', return that.
-- See Note [Rapid termination for strict binders]
forceTerm :: SubDemand -> Term -> (TermFlag, Term)
forceTerm sd term = runTerminationM (forceTermM sd term)

forceTermM :: SubDemand -> Term -> TerminationM Term
forceTermM sd (Term tf sh) = do
  -- 1. discharge head strictness by noting the term flag
  noteTermFlag tf
  -- 2. discharge *nested* strictness on available nested info
  sh' <- case (sh, sd) of
    (BotSh, _) ->
      return BotSh
    (LamSh t, viewCall -> Just (n, sd)) | isStrict n ->
      LamSh <$> forceTermM sd t
    (TopSh, Call n sd) | isStrict n ->
      LamSh <$> forceTermM sd topTerm
    (ConSh t fields, viewProd (length fields) -> Just ds) | t == fIRST_TAG -> do
      fields' <- zipWithM (idIfLazy forceTermM) ds fields
      return (ConSh fIRST_TAG fields')
    (TopSh, Prod ds) -> do
      fields' <- mapM (flip (idIfLazy forceTermM) topTerm) ds
      return (ConSh fIRST_TAG fields')
    _ ->
      return sh
  return (Term Terminates sh')

-----------
-- * CprSig

-- | The arity of the wrapped thing is the arity at which it is safe to unleash.
-- See Note [Understanding DmdType and StrictSig] in "GHC.Types.Demand"
newtype Sig l = Sig { getSig :: l }
  deriving (Eq, Outputable, Binary)

type TermSig = Sig Term

type CprSig = Sig Cpr

mkTermSigForResTerm :: Arity -> Term -> TermSig
mkTermSigForResTerm arty res_cpr = Sig $ iterate lamTerm res_cpr !! arty

mkCprSigForResCpr :: Arity -> Cpr -> CprSig
mkCprSigForResCpr arty res_cpr = Sig $ iterate lamCpr res_cpr !! arty

mkTermSigForFunRHS :: Term -> TermSig
mkTermSigForFunRHS = Sig

moreTermThanArity :: TermSig -> Arity -> Bool
moreTermThanArity (Sig term) n = iterate applyTerm term !! n /= topTerm

-- | Turns a 'Cpr' of a function body into a signature that is unleashable
-- at call sites of the particular 'Arity' and minimum call 'Demand'.
--
-- See Note [Trimming CPR signatures according to Term]
-- and Note [Improving CPR by considering strictness demand from call sites],
-- as well as Note [Arity trimming for CPR signatures],
-- all in "GHC.Core.Opt.CprAnal".
mkCprSigForFunRHS :: Arity -> Demand -> Term -> Cpr -> CprSig
mkCprSigForFunRHS arty fun_demand rhs_term rhs_cpr =
  -- pprTrace "mkCprSigForFunRHS" (vcat [ppr arty, ppr fun_demand, ppr call_sd, ppr rhs_term, ppr rhs_cpr, ppr demanded_term, ppr trimmed_term, ppr final_cpr]) $
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

topCprSig :: CprSig
topCprSig = Sig topCpr

topTermSig :: TermSig
topTermSig = Sig topTerm

seqSig :: (l -> ()) -> Sig l -> ()
seqSig seq_l (Sig sig) = seq_l sig `seq` ()

seqCprSig :: CprSig -> ()
seqCprSig = seqSig seqCpr

seqTermSig :: TermSig -> ()
seqTermSig = seqSig seqTerm

-- | Get a 'Term' for a 'DataCon', given 'Term's for its fields.
termTransformDataConWork :: DataCon -> [Term] -> Term
-- What about DataCon *wrappers*? See Note [CPR for DataCon wrappers]
-- NB: Evaluation of the worker always terminates, because all fields are lazy.
-- Evaluation of the arguments is done by the DataCon wrapper.
termTransformDataConWork con args
  | wkr_arity <= mAX_CPR_SIZE
  = -- pprTrace "termTransformDataConWork" (ppr con <+> ppr wkr_arity <+> ppr args) $
    getSig $ mkTermSigForResTerm wkr_arity (conTerm con args')
  -- We do not record termination info for components of big tuples because it
  -- leads to bloated interface files, and because most of the fields generally
  -- diverge anyway.
  | otherwise
  = topTerm
  where
    wkr_arity = dataConRepArity con
    args' = take wkr_arity $ args ++ repeat topTerm

-- | Get a 'Cpr' for a 'DataCon', given 'Cpr's for its fields.
cprTransformDataConWork :: DataCon -> [Cpr] -> Cpr
-- What about DataCon *wrappers*? See Note [CPR for DataCon wrappers]
cprTransformDataConWork con args
  | null (dataConExTyCoVars con)  -- No existentials
  , wkr_arity <= mAX_CPR_SIZE
  , args `lengthIs` wkr_arity
  -- , pprTrace "cprTransformDataConWork" (ppr con <+> ppr wkr_arity <+> ppr args) True
  = getSig $ mkCprSigForResCpr wkr_arity (conCpr con args)
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

termTransformSig :: StrictSig -> TermSig -> [Term] -> Term
-- See Note [Rapid termination for strict binders] in CprAnal
termTransformSig str_sig (Sig sig_term) arg_terms
  | dmds <- argDmdsFromStrictSig str_sig
  , (tf, _) <- forceArgTerms dmds (arg_terms ++ repeat topTerm)
  , sig_term' <- apply_term_flag (length dmds) tf
  = -- pprTrace "termTransformSig" (ppr str_sig <+> ppr sig_term <+> ppr arg_terms <+> ppr tf <+> ppr sig_term') $
    sig_term'
  where
    apply_term_flag _     Terminates   = sig_term
    apply_term_flag arity MightDiverge = markDivergingTerm arity sig_term

cprTransformSig :: CprSig -> Cpr
-- Nothing higher-order to worry about here
cprTransformSig (Sig sig_cpr) = sig_cpr

-- | We have to be sure that 'termTransformSig' and 'argTermsFromStrictSig'
-- agree in how they compute the 'Demand's for which the 'CprSig' is computed.
-- This function encodes the common (trivial) logic.
argDmdsFromStrictSig :: StrictSig -> [Demand]
-- NB: Doesn't need to account for strict fields, as in
-- Note [Add demands for strict constructors].
argDmdsFromStrictSig = fst . splitStrictSig

-- | Produces 'Term's that match the given strictness signature. Examples:
--
--   * A head-strict demand `S` would translate to `#`
--   * A tuple demand `S(S,L)` would translate to `#1(#,*)`
--   * A call demand `C(S)` would translate to `#L(#)`
argTermsFromStrictSig :: StrictSig -> [Term]
argTermsFromStrictSig sig =
  snd $ forceArgTerms (argDmdsFromStrictSig sig) (repeat topTerm)

-- | Force the terms by the given demands and return them.
-- Also returns the 'TermFlag' that accounts for whether forcing of any of the
-- 'Term's might have diverged.
-- See Note [Rapid termination for strict binders]
forceArgTerms :: [Demand] -> [Term] -> (TermFlag, [Term])
forceArgTerms dmds terms =
  runTerminationM (zipWithM (idIfLazy forceTermM) dmds terms)

-- | Produces 'Cpr's according to how strict argument types will be unboxed.
-- Examples:
--
--   * A head-strict demand `S` on `Int` would translate to `c1(*)`
--   * A tuple demand `S(S,L)` on `(Int, Bool)` would translate to `c1(c1(*),*)`
--   * A tuple demand `S(S,L)` on `(a , Bool)` would translate to `c1(*,*)`,
--     because the unboxing strategy would not unbox the `a`.
argCprsFromStrictSig :: UnboxingStrategy Demand -> [Type] -> StrictSig -> [Cpr]
argCprsFromStrictSig want_to_unbox arg_tys sig
  = zipWith go arg_tys (argDmdsFromStrictSig sig)
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

---------------
-- * Outputable

pprCloShape :: Outputable r => SDoc -> ([r] -> Bool) -> CloShape r -> SDoc
pprCloShape bot should_print_fields sh = case sh of
  BotSh                      -> bot
  TopSh                      -> empty
  LamSh sh                   -> char 'L' <> ppr sh
  ConSh t fs
    | should_print_fields fs -> int t <> parens (pprWithCommas ppr fs)
    | otherwise              -> int t

instance Outputable TermFlag where
  ppr MightDiverge = char '*'
  ppr Terminates   = char '#'

instance Outputable Term where
  ppr (Term tf l) = ppr tf <> pprCloShape (text "(#..)") (const True) l

instance Outputable Cpr where
  ppr (Cpr l) = pprCloShape (char 'b') (any (/= topCpr)) l

-----------
-- * Binary

instance Binary r => Binary (CloShape r) where
  put_ bh BotSh   = putByte bh 0
  put_ bh (LamSh r) = putByte bh 1 *> put_ bh r
  put_ bh (ConSh t fs) = putByte bh 2 *> putULEB128 bh t *> put_ bh fs
  put_ bh TopSh   = putByte bh 3
  get  bh = do
    h <- getByte bh
    case h of
      0 -> return BotSh
      1 -> LamSh <$> get bh
      2 -> ConSh <$> getULEB128 bh <*> get bh
      3 -> return TopSh
      _ -> pprPanic "Binary CloShape: Invalid tag" (int (fromIntegral h))

instance Binary TermFlag where
  put_ bh Terminates   = put_ bh True
  put_ bh MightDiverge = put_ bh False
  get  bh = do
    b <- get bh
    if b
      then pure Terminates
      else pure MightDiverge

instance Binary Term where
  put_ bh (Term tf l) = put_ bh tf >> put_ bh l
  get  bh = Term <$> get bh <*> get bh

instance Binary Cpr where
  put_ bh (Cpr sh) = put_ bh sh
  get  bh = Cpr <$> get bh
