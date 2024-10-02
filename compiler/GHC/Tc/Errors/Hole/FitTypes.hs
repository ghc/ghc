{-# LANGUAGE ExistentialQuantification #-}
module GHC.Tc.Errors.Hole.FitTypes (
  TypedHole (..), HoleFit (..), TcHoleFit(..), HoleFitCandidate (..),
  hfIsLcl, pprHoleFitCand
  ) where

import GHC.Prelude

import GHC.Tc.Types.Constraint
import GHC.Tc.Utils.TcType

import GHC.Types.Name.Reader

import GHC.Hs.Doc
import GHC.Types.Id

import GHC.Utils.Outputable
import GHC.Types.Name

import GHC.Data.Bag

import Data.Function ( on )

data TypedHole = TypedHole { th_relevant_cts :: Bag CtEvidence
                           -- ^ Any relevant Cts to the hole
                           , th_implics :: [Implication]
                           -- ^ The nested implications of the hole with the
                           --   innermost implication first.
                           , th_hole :: Maybe Hole
                           -- ^ The hole itself, if available.
                           }

instance Outputable TypedHole where
  ppr (TypedHole { th_relevant_cts = rels
                 , th_implics      = implics
                 , th_hole         = hole })
    = hang (text "TypedHole") 2
        (ppr rels $+$ ppr implics $+$ ppr hole)

-- | HoleFitCandidates are passed to hole fit plugins and then
-- checked whether they fit a given typed-hole.
data HoleFitCandidate = IdHFCand Id             -- An id, like locals.
                      | NameHFCand Name         -- A name, like built-in syntax.
                      | GreHFCand GlobalRdrElt  -- A global, like imported ids.

instance Eq HoleFitCandidate where
  IdHFCand i1 == IdHFCand i2 = i1 == i2
  NameHFCand n1 == NameHFCand n2 = n1 == n2
  GreHFCand gre1 == GreHFCand gre2 = greName gre1 == greName gre2
  _ == _ = False

instance Outputable HoleFitCandidate where
  ppr = pprHoleFitCand

pprHoleFitCand :: HoleFitCandidate -> SDoc
pprHoleFitCand (IdHFCand cid) = text "Id HFC: " <> ppr cid
pprHoleFitCand (NameHFCand cname) = text "Name HFC: " <> ppr cname
pprHoleFitCand (GreHFCand cgre) = text "Gre HFC: " <> ppr cgre

instance NamedThing HoleFitCandidate where
  getName hfc = case hfc of
                     IdHFCand cid -> idName cid
                     NameHFCand cname -> cname
                     GreHFCand cgre -> greName cgre
  getOccName hfc = case hfc of
                     IdHFCand cid -> occName cid
                     NameHFCand cname -> occName cname
                     GreHFCand cgre -> occName $ greName cgre

instance HasOccName HoleFitCandidate where
  occName = getOccName

instance Ord HoleFitCandidate where
  compare = compare `on` getName

-- | HoleFit is the type we use for valid hole fits. It contains the
-- element that was checked, the Id of that element as found by `tcLookup`,
-- and the refinement level of the fit, which is the number of extra argument
-- holes that this fit uses (e.g. if hfRefLvl is 2, the fit is for `Id _ _`).
data TcHoleFit =
  HoleFit { hfId   :: Id       -- ^ The elements id in the TcM
          , hfCand :: HoleFitCandidate  -- ^ The candidate that was checked.
          , hfType :: TcType -- ^ The type of the id, possibly zonked.
          , hfRefLvl :: Int  -- ^ The number of holes in this fit.
          , hfWrap :: [TcType] -- ^ The wrapper for the match.
          , hfMatches :: [TcType]
          -- ^ What the refinement variables got matched with, if anything
          , hfDoc :: Maybe [HsDocString]
          -- ^ Documentation of this HoleFit, if available.
          }

data HoleFit
  = TcHoleFit  TcHoleFit
  | RawHoleFit SDoc
 -- ^ A fit that is just displayed as is. Here so that HoleFitPlugins
 --   can inject any fit they want.

-- We define an Eq and Ord instance to be able to build a graph.
instance Eq TcHoleFit where
   (==) = (==) `on` hfId

instance Outputable HoleFit where
  ppr (TcHoleFit hf)  = ppr hf
  ppr (RawHoleFit sd) = sd

instance Outputable TcHoleFit where
  ppr (HoleFit _ cand ty _ _ mtchs _) =
    hang (name <+> holes) 2 (text "where" <+> name <+> dcolon <+> (ppr ty))
    where name = ppr $ getName cand
          holes = sep $ map (parens . (text "_" <+> dcolon <+>) . ppr) mtchs

-- We compare HoleFits by their name instead of their Id, since we don't
-- want our tests to be affected by the non-determinism of `nonDetCmpVar`,
-- which is used to compare Ids. When comparing, we want HoleFits with a lower
-- refinement level to come first.
instance Ord TcHoleFit where
--  compare (RawHoleFit _) (RawHoleFit _) = EQ
--  compare (RawHoleFit _) _ = LT
--  compare _ (RawHoleFit _) = GT
  compare a@(HoleFit {}) b@(HoleFit {}) = cmp a b
    where cmp  = if hfRefLvl a == hfRefLvl b
                 then compare `on` (getName . hfCand)
                 else compare `on` hfRefLvl

hfIsLcl :: TcHoleFit -> Bool
hfIsLcl hf@(HoleFit {}) = case hfCand hf of
                            IdHFCand _    -> True
                            NameHFCand _  -> False
                            GreHFCand gre -> gre_lcl gre


