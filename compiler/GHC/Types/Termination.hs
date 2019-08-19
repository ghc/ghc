{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}

-- | Types for the lattice that the termination analyis in
-- "GHC.Core.Opt.CprAnal" operates on. The resulting information is stored in
-- 'GHC.Types.Id.idTermInfo'.
module GHC.Types.Termination (
    -- * Closure shape
    CloShape (..), lubCloShape, splitConSh, splitLamSh, pruneCloShape,
    seqCloShape, pprCloShape,
    -- * TermFlag
    TermFlag (Terminates),
    -- * Termination lattice
    Term(Term), botTerm, topTerm, lubTerm, bothTerm, whnfTerm, divergeTerm,
    appTerm, appsTerm, lamTerm, forceTerm, whnfTerminatesRapidly, pruneDeepTerm,
    splitConTerm, splitLamTerm, expandConFieldsTerm, trimTermToArity,
    -- * Termination signatures
    Sig (..), TermSig, topTermSig, whnfTermSig, divergeTermSig, mkTermSig,
    termTransformDataConWork, termTransformSig, argTermsFromStrictSig,
    seqTermSig
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Types.Basic
import GHC.Types.Demand
import GHC.Core.DataCon
import GHC.Utils.Binary
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.Maybe

import Data.Coerce
import qualified Data.Semigroup as Semigroup
import Data.Functor.Identity
import Control.Applicative (Const (..))
import Control.Monad.Trans.Writer.CPS
import Control.Monad (zipWithM)

import GHC.Driver.Ppr
_ = pprTrace -- Tired of commenting out GHC.Driver.Ppr

-------------
-- * CloShape

-- | Abstracts the runtime shape of a heap closure.
data CloShape r
  = BotSh
  | LamSh !r
  | ConSh !ConTag [r]
  | TopSh
  deriving Eq

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

splitLamSh :: r -> CloShape r -> Maybe r
splitLamSh bot BotSh     = Just bot
splitLamSh _   (LamSh r) = Just r
splitLamSh _   _         = Nothing

seqCloShape :: (r -> ()) -> CloShape r -> ()
seqCloShape seq_r (ConSh _ args) = foldr (seq . seq_r) () args
seqCloShape seq_r (LamSh r)      = seq_r r
seqCloShape _     _              = ()

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
lubTermFlag Terminates   Terminates   = Terminates
lubTermFlag _            MightDiverge = MightDiverge
lubTermFlag MightDiverge _            = MightDiverge

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
    -- 'normTermShape' is the main point of this synonym. The first 4 case alts
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

-- | Used as the 'Term' of 'undefined'/'error'/other sources of divergence.
--
-- We assume that evaluation to WHNF surely diverges (so 'MightDiverge'), but
-- are optimistic about nested termination information. I.e., we
-- assume that returned tuple components terminate rapidly.
divergeTerm :: Term
divergeTerm = Term_ MightDiverge BotSh

conTerm :: DataCon -> [Term] -> Term
conTerm dc terms  = Term Terminates (ConSh (dataConTag dc) terms')
  where
    -- See Note [No unboxed tuple for single, unlifted transit var]
    terms' = zipWithEqual "conTerm" add terms strs
    strs = dataConRepStrictness dc
    add t str | isMarkedStrict str = getForcedTerm $ forceTermM topSubDmd t
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
splitLamTerm (Term tf sh) =
  (,) tf $! splitLamSh botTerm sh `orElse` topTerm

-- | Applies a 'LamSh', accounting the outer 'TermFlag' to the inner one.
-- >>> appTerm (Term MightDiverge (LamSh (Term Terminates BotSh)))
-- Term MightDiverge BotSh
appTerm :: Term -> Term
appTerm (splitLamTerm -> (tf, term)) = term `bothTerm` tf

-- | `appsTerm n` is n iterations of 'appTerm'.
appsTerm :: Arity -> Term -> Term
appsTerm arty term = iterate appTerm term !! arty

lamTerm :: Term -> Term
lamTerm t = Term Terminates (LamSh t)

-- | `lamsTerm n` is n iterations of 'lamTerm'.
lamsTerm :: Arity -> Term -> Term
lamsTerm arty term = iterate lamTerm term !! arty

-- | 'lubTerm's the given outer 'TermFlag' onto the 'Term'.
bothTerm :: Term -> TermFlag -> Term
-- If tf = Terminates, it's just 'id'.
-- If tf = MightDiverge, it will only set the WHNF layer to MightDiverge,
-- leaving nested termination info (e.g. on product components) intact.
bothTerm term        Terminates   = term
bothTerm (Term _ sh) MightDiverge = Term MightDiverge sh

-- | Makes sure there are exactly arity many successive, terminating 'LamSh's,
-- pushing intermittent 'MightDiverge's inwards.
-- It's like eta-expansion on 'Term' in that it pushes work (MD) under lambdas.
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

---------------------------------
-- * Forcing 'Term' with 'Demand'
--
-- See Note [Rapid termination for strict binders]

termFlag2Any :: TermFlag -> Semigroup.Any
termFlag2Any MightDiverge = Semigroup.Any True
termFlag2Any Terminates   = Semigroup.Any False

any2TermFlag :: Semigroup.Any -> TermFlag
any2TermFlag (Semigroup.Any True)  = MightDiverge
any2TermFlag (Semigroup.Any False) = Terminates

class Applicative f => ApplicativeTermination f where
  noteTermFlag :: TermFlag -> f ()

-- | For extracting a 'TermFlag' from 'forceTermM'
newtype GetTermFlagM a = GetTermFlagM (Const Semigroup.Any a)
  deriving (Functor, Applicative)
-- | For extracting the forced 'Term' from 'forceTermM'
newtype GetTermM a = GetTermM (Identity a)
  deriving (Functor, Applicative)
-- | For extracting both the 'TermFlag' and the forced 'Term' from 'forceTermM'
newtype TerminationM a = TerminationM (Writer Semigroup.Any a)
  deriving (Functor, Applicative)

instance ApplicativeTermination GetTermFlagM where
  noteTermFlag = GetTermFlagM . Const . termFlag2Any
instance ApplicativeTermination GetTermM where
  noteTermFlag _ = pure ()
instance ApplicativeTermination TerminationM where
  noteTermFlag = TerminationM . tell . termFlag2Any

getTermFlag :: GetTermFlagM a -> TermFlag
getTermFlag (GetTermFlagM f) = any2TermFlag $ getConst f

getForcedTerm :: GetTermM a -> a
getForcedTerm (GetTermM f) = runIdentity f

getTermFlagAndForcedTerm :: TerminationM a -> (TermFlag, a)
getTermFlagAndForcedTerm (TerminationM act) = case runWriter act of
  (!a, !m) -> (any2TermFlag m, a)

-- | Lifts a 'TerminationM' action from 'SubDemand's to 'Demand's by returning
-- the original argument if the 'Demand' is not strict.
-- Follows the rule "Don't force if lazy!".
idIfLazy :: Applicative f => (SubDemand -> cpr -> f cpr) -> Demand -> cpr -> f cpr
idIfLazy k (n :* sd) cpr
  | isStrict n = k sd cpr
  | otherwise  = pure cpr

-- | Forces possibly deep 'Term' info according to incoming 'SubDemand'.
-- If there's any possibility that this 'MightDiverge', return that.
-- See Note [Rapid termination for strict binders]
forceTerm :: SubDemand -> Term -> (TermFlag, Term)
forceTerm sd term = getTermFlagAndForcedTerm (forceTermM sd term)

forceTermM :: ApplicativeTermination f => SubDemand -> Term -> f Term
forceTermM sd (Term tf sh) = do
  -- 1. discharge head strictness by noting the term flag
  noteTermFlag tf
  -- 2. discharge *nested* strictness on available nested info
  sh' <- case (sh, sd) of
    (BotSh, _) -> pure BotSh
    (LamSh t, viewCall -> Just (n, sd))
      | isStrict n
      -> LamSh <$> forceTermM sd t
    (TopSh, Call n sd)
      | isStrict n
      -> LamSh <$> forceTermM sd topTerm
    (ConSh t fields, Prod ds)
      | t == fIRST_TAG
      , length ds <= mAX_TERM_CON_SIZE
      -> ConSh fIRST_TAG <$> forceTermsM ds fields
    (TopSh, Prod ds)
      | length ds <= mAX_TERM_CON_SIZE
      -> ConSh fIRST_TAG <$> traverse (flip (idIfLazy forceTermM) topTerm) ds
    _ -> pure sh
  pure (Term Terminates sh')

forceTermsM :: ApplicativeTermination f => [Demand] -> [Term] -> f [Term]
forceTermsM = zipWithM (idIfLazy forceTermM)
{-# INLINE forceTermsM #-}

whnfTerminatesRapidly :: Term -> Bool
whnfTerminatesRapidly term =
  getTermFlag (forceTermM topSubDmd term) == Terminates

-- | A signature of `l`, which is attached to 'Id's and unleashed at use sites.
--
-- Why is this necessary? Answer: The wrapped `l` will lack 'idArity' many
-- 'LamSh's, simply for redundancy and efficiency reasons (repeated
-- wrapping/unwrapping, but also serialisation/deserialisation to
-- interface files).
--
-- For a related distinction, see Note [Understanding DmdType and StrictSig] in
-- "GHC.Types.Demand".
newtype Sig l = Sig { getSig :: l }
  deriving (Eq, Outputable, Binary)

-- | Signatures need to be unleashed through 'termTransformSig' to account for
-- strict arguments and 'idArity' missing 'LamSh's, hence this is not just
-- 'Term'.
-- See Note [Rapid termination for strict binders] in "GHC.Core.Opt.CprAnal".
type TermSig = Sig Term

topTermSig :: TermSig
topTermSig = Sig topTerm

whnfTermSig :: TermSig
whnfTermSig = Sig whnfTerm

divergeTermSig :: TermSig
divergeTermSig = Sig divergeTerm

seqTermSig :: TermSig -> ()
seqTermSig = coerce seqTerm

-- | Turns a 'Term' of a function RHS into a signature that is unleashable
-- at call sites of the particular 'Arity'.
mkTermSig :: Arity -> Term -> TermSig
-- Strip the arity many (and thus boring) LamSh's
mkTermSig arity rhs_term = Sig $ appsTerm arity rhs_term

termTransformSig :: Arity -> StrictSig -> TermSig -> [Term] -> Term
-- See Note [Rapid termination for strict binders] in CprAnal
termTransformSig arity str_sig (Sig body_term) arg_terms
  | dmds <- argDmdsFromStrictSig str_sig
  , tf <- getTermFlag $ forceTermsM dmds (arg_terms ++ repeat topTerm)
  , body_term' <- body_term `bothTerm` tf
  = -- pprTrace "termTransformSig" (ppr str_sig <+> ppr sig_term <+> ppr arg_terms <+> ppr tf <+> ppr sig_term') $
    lamsTerm arity body_term'

-- | We have to be sure that 'termTransformSig' and 'argTermsFromStrictSig'
-- agree in how they compute the 'Demand's for which the 'TermSig' is computed.
-- This function encodes the common (trivial) logic, making sure it doesn't go
-- out of sync in the future.
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
  getForcedTerm $ forceTermsM (argDmdsFromStrictSig sig) (repeat topTerm)

-- | Get a 'Term' for a 'DataCon', given 'Term's for its fields.
termTransformDataConWork :: DataCon -> [Term] -> Term
-- What about DataCon *wrappers*? See Note [CPR for DataCon wrappers]
-- NB: Evaluation of the worker always terminates, because all fields are lazy.
-- Evaluation of the arguments is done by the DataCon wrapper.
termTransformDataConWork con args
  | wkr_arity <= mAX_TERM_CON_SIZE
  = -- pprTrace "termTransformDataConWork" (ppr con <+> ppr wkr_arity <+> ppr args) $
    lamsTerm wkr_arity (conTerm con args')
  | otherwise -- We do not record termination info for components of big tuples
  = topTerm   -- because it leads to bloated interface files, and because most
  where       -- of the fields generally diverge anyway.
    wkr_arity = dataConRepArity con
    args' = take wkr_arity $ args ++ repeat topTerm

mAX_TERM_CON_SIZE :: Arity
mAX_TERM_CON_SIZE = 10

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

-----------
-- * Binary

instance Binary r => Binary (CloShape r) where
  put_ bh BotSh        = putByte bh 0
  put_ bh (LamSh r)    = putByte bh 1 *> put_ bh r
  put_ bh (ConSh t fs) = putByte bh 2 *> put_ bh t *> put_ bh fs
  put_ bh TopSh        = putByte bh 3
  get  bh = do
    h <- getByte bh
    case h of
      0 -> return BotSh
      1 -> LamSh <$> get bh
      2 -> ConSh <$> get bh <*> get bh
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
