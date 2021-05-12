
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>
        Sebastian Graf <sgraf1337@gmail.com>
-}

-- | Types used through-out pattern match checking. This module is mostly there
-- to be imported from "GHC.HsToCore.Types". The exposed API is that of
-- "GHC.HsToCore.Pmc".
--
-- These types model the paper
-- [Lower Your Guards: A Compositional Pattern-Match Coverage Checker"](https://dl.acm.org/doi/abs/10.1145/3408989).
module GHC.HsToCore.Pmc.Types (
        -- * LYG syntax

        -- ** Guard language
        SrcInfo(..), PmGrd(..), GrdVec(..),

        -- ** Guard tree language
        PmMatchGroup(..), PmMatch(..), PmGRHSs(..), PmGRHS(..), PmPatBind(..), PmEmptyCase(..),

        -- * Coverage Checking types
        RedSets (..), Precision (..), CheckResult (..),

        -- * Pre and post coverage checking synonyms
        Pre, Post,

        -- * Normalised refinement types
        module GHC.HsToCore.Pmc.Solver.Types

    ) where

import GHC.Prelude

import GHC.HsToCore.Pmc.Solver.Types

import GHC.Data.OrdList
import GHC.Types.Id
import GHC.Types.Var (EvVar)
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Core.Type
import GHC.Core

import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as Semi

--
-- * Guard language
--

-- | A very simple language for pattern guards. Let bindings, bang patterns,
-- and matching variables against flat constructor patterns.
-- The LYG guard language.
data PmGrd
  = -- | @PmCon x K dicts args@ corresponds to a @K dicts args <- x@ guard.
    -- The @args@ are bound in this construct, the @x@ is just a use.
    -- For the arguments' meaning see 'GHC.Hs.Pat.ConPatOut'.
    PmCon {
      pm_id          :: !Id,
      pm_con_con     :: !PmAltCon,
      pm_con_tvs     :: ![TyVar],
      pm_con_dicts   :: ![EvVar],
      pm_con_args    :: ![Id]
    }

    -- | @PmBang x@ corresponds to a @seq x True@ guard.
    -- If the extra 'SrcInfo' is present, the bang guard came from a source
    -- bang pattern, in which case we might want to report it as redundant.
    -- See Note [Dead bang patterns] in GHC.HsToCore.Pmc.Check.
  | PmBang {
      pm_id   :: !Id,
      _pm_loc :: !(Maybe SrcInfo)
    }

    -- | @PmLet x expr@ corresponds to a @let x = expr@ guard. This actually
    -- /binds/ @x@.
  | PmLet {
      pm_id        :: !Id,
      _pm_let_expr :: !CoreExpr
    }

-- | Should not be user-facing.
instance Outputable PmGrd where
  ppr (PmCon x alt _tvs _con_dicts con_args)
    = hsep [ppr alt, hsep (map ppr con_args), text "<-", ppr x]
  ppr (PmBang x _loc) = char '!' <> ppr x
  ppr (PmLet x expr) = hsep [text "let", ppr x, text "=", ppr expr]

--
-- * Guard tree language
--

-- | Means by which we identify a source construct for later pretty-printing in
-- a warning message. 'SDoc' for the equation to show, 'Located' for the
-- location.
newtype SrcInfo = SrcInfo (Located SDoc)

-- | A sequence of 'PmGrd's.
newtype GrdVec = GrdVec [PmGrd]

-- | A guard tree denoting 'MatchGroup'.
newtype PmMatchGroup p = PmMatchGroup (NonEmpty (PmMatch p))

-- | A guard tree denoting 'Match': A payload describing the pats and a bunch of
-- GRHS.
data PmMatch p = PmMatch { pm_pats :: !p, pm_grhss :: !(PmGRHSs p) }

-- | A guard tree denoting 'GRHSs': A bunch of 'PmLet' guards for local
-- bindings from the 'GRHSs's @where@ clauses and the actual list of 'GRHS'.
-- See Note [Long-distance information for HsLocalBinds] in
-- "GHC.HsToCore.Pmc.Desugar".
data PmGRHSs p = PmGRHSs { pgs_lcls :: !p, pgs_grhss :: !(NonEmpty (PmGRHS p))}

-- | A guard tree denoting 'GRHS': A payload describing the grds and a 'SrcInfo'
-- useful for printing out in warnings messages.
data PmGRHS p = PmGRHS { pg_grds :: !p, pg_rhs :: !SrcInfo }

-- | A guard tree denoting an -XEmptyCase.
newtype PmEmptyCase = PmEmptyCase { pe_var :: Id }

-- | A guard tree denoting a pattern binding.
newtype PmPatBind p =
  -- just reuse GrdGRHS and pretend its @SrcInfo@ is info on the /pattern/,
  -- rather than on the pattern bindings.
  PmPatBind (PmGRHS p)

instance Outputable SrcInfo where
  ppr (SrcInfo (L (RealSrcSpan rss _) _)) = ppr (srcSpanStartLine rss)
  ppr (SrcInfo (L s                   _)) = ppr s

-- | Format LYG guards as @| True <- x, let x = 42, !z@
instance Outputable GrdVec where
  ppr (GrdVec [])     = empty
  ppr (GrdVec (g:gs)) = fsep (char '|' <+> ppr g : map ((comma <+>) . ppr) gs)

-- | Format a LYG sequence (e.g. 'Match'es of a 'MatchGroup' or 'GRHSs') as
-- @{ <first alt>; ...; <last alt> }@
pprLygSequence :: Outputable a => NonEmpty a -> SDoc
pprLygSequence (NE.toList -> as) =
  braces (space <> fsep (punctuate semi (map ppr as)) <> space)

instance Outputable p => Outputable (PmMatchGroup p) where
  ppr (PmMatchGroup matches) = pprLygSequence matches

instance Outputable p => Outputable (PmMatch p) where
  ppr (PmMatch { pm_pats = grds, pm_grhss = grhss }) =
    ppr grds <+> ppr grhss

instance Outputable p => Outputable (PmGRHSs p) where
  ppr (PmGRHSs { pgs_lcls = _lcls, pgs_grhss = grhss }) =
    ppr grhss

instance Outputable p => Outputable (PmGRHS p) where
  ppr (PmGRHS { pg_grds = grds, pg_rhs = rhs }) =
    ppr grds <+> text "->" <+> ppr rhs

instance Outputable p => Outputable (PmPatBind p) where
  ppr (PmPatBind PmGRHS { pg_grds = grds, pg_rhs = bind }) =
    ppr bind <+> ppr grds <+> text "=" <+> text "..."

instance Outputable PmEmptyCase where
  ppr (PmEmptyCase { pe_var = var }) =
    text "<empty case on " <> ppr var <> text ">"

data Precision = Approximate | Precise
  deriving (Eq, Show)

instance Outputable Precision where
  ppr = text . show

instance Semi.Semigroup Precision where
  Precise <> Precise = Precise
  _       <> _       = Approximate

instance Monoid Precision where
  mempty = Precise
  mappend = (Semi.<>)

-- | Redundancy sets, used to determine redundancy of RHSs and bang patterns
-- (later digested into a 'CIRB').
data RedSets
  = RedSets
  { rs_cov :: !Nablas
  -- ^ The /Covered/ set; the set of values reaching a particular program
  -- point.
  , rs_div :: !Nablas
  -- ^ The /Diverging/ set; empty if no match can lead to divergence.
  --   If it wasn't empty, we have to turn redundancy warnings into
  --   inaccessibility warnings for any subclauses.
  , rs_bangs :: !(OrdList (Nablas, SrcInfo))
  -- ^ If any of the 'Nablas' is empty, the corresponding 'SrcInfo' pin-points
  -- a bang pattern in source that is redundant. See Note [Dead bang patterns].
  }

instance Outputable RedSets where
  ppr RedSets { rs_cov = _cov, rs_div = _div, rs_bangs = _bangs }
    -- It's useful to change this definition for different verbosity levels in
    -- printf-debugging
    = empty

-- | Pattern-match coverage check result
data CheckResult a
  = CheckResult
  { cr_ret :: !a
  -- ^ A hole for redundancy info and covered sets.
  , cr_uncov   :: !Nablas
  -- ^ The set of uncovered values falling out at the bottom.
  --   (for -Wincomplete-patterns, but also important state for the algorithm)
  , cr_approx  :: !Precision
  -- ^ A flag saying whether we ran into the 'maxPmCheckModels' limit for the
  -- purpose of suggesting to crank it up in the warning message. Writer state.
  } deriving Functor

instance Outputable a => Outputable (CheckResult a) where
  ppr (CheckResult c unc pc)
    = text "CheckResult" <+> ppr_precision pc <+> braces (fsep
        [ field "ret" c <> comma
        , field "uncov" unc])
    where
      ppr_precision Precise     = empty
      ppr_precision Approximate = text "(Approximate)"
      field name value = text name <+> equals <+> ppr value

--
-- * Pre and post coverage checking synonyms
--

-- | Used as tree payload pre-checking. The LYG guards to check.
type Pre = GrdVec

-- | Used as tree payload post-checking. The redundancy info we elaborated.
type Post = RedSets
