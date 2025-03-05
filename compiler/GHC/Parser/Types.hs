{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module GHC.Parser.Types
   ( SumOrTuple(..)
   , pprSumOrTuple
   , PatBuilder(..)
   , DataConBuilder(..)
   )
where

import GHC.Prelude
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader
import GHC.Hs.Extension
import GHC.Hs.Lit
import GHC.Hs.Pat
import GHC.Hs.Type
import GHC.Utils.Outputable as Outputable
import GHC.Data.OrdList

import Data.Foldable
import GHC.Parser.Annotation
import Language.Haskell.Syntax

data SumOrTuple b
  = Sum ConTag Arity (LocatedA b) [EpToken "|"] [EpToken "|"]
  -- ^ Last two are the locations of the '|' before and after the payload
  | Tuple [Either (EpAnn Bool) (LocatedA b)]

pprSumOrTuple :: Outputable b => Boxity -> SumOrTuple b -> SDoc
pprSumOrTuple boxity = \case
    Sum alt arity e _ _ ->
      parOpen <+> ppr_bars (alt - 1) <+> ppr e <+> ppr_bars (arity - alt)
              <+> parClose
    Tuple xs ->
      parOpen <> (fcat . punctuate comma $ map ppr_tup xs)
              <> parClose
  where
    ppr_tup (Left _)  = empty
    ppr_tup (Right e) = ppr e

    ppr_bars n = hsep (replicate n (Outputable.char '|'))
    (parOpen, parClose) =
      case boxity of
        Boxed -> (text "(", text ")")
        Unboxed -> (text "(#", text "#)")


-- | See Note [Ambiguous syntactic categories] and Note [PatBuilder]
data PatBuilder p
  = PatBuilderPat (Pat p)
  | PatBuilderPar (EpToken "(") (LocatedA (PatBuilder p)) (EpToken ")")
  | PatBuilderApp (LocatedA (PatBuilder p)) (LocatedA (PatBuilder p))
  | PatBuilderAppType (LocatedA (PatBuilder p)) (EpToken "@") (HsTyPat GhcPs)
  | PatBuilderOpApp (LocatedA (PatBuilder p)) (LocatedN RdrName)
                    (LocatedA (PatBuilder p)) ([EpToken "("], [EpToken ")"])
  | PatBuilderVar (LocatedN RdrName)
  | PatBuilderOverLit (HsOverLit GhcPs)

-- These instances are here so that they are not orphans
type instance Anno (GRHS GhcPs (LocatedA (PatBuilder GhcPs)))             = EpAnnCO
type instance Anno (MatchGroup GhcPs (LocatedA (PatBuilder GhcPs)))       = SrcSpanAnnLW
type instance Anno (Match GhcPs (LocatedA (PatBuilder GhcPs)))            = SrcSpanAnnA
type instance Anno (StmtLR GhcPs GhcPs (LocatedA (PatBuilder GhcPs)))     = SrcSpanAnnA

instance Outputable (PatBuilder GhcPs) where
  ppr (PatBuilderPat p) = ppr p
  ppr (PatBuilderPar _ (L _ p) _) = parens (ppr p)
  ppr (PatBuilderApp (L _ p1) (L _ p2)) = ppr p1 <+> ppr p2
  ppr (PatBuilderAppType (L _ p) _ t) = ppr p <+> text "@" <> ppr t
  ppr (PatBuilderOpApp (L _ p1) op (L _ p2) _) = ppr p1 <+> ppr op <+> ppr p2
  ppr (PatBuilderVar v) = ppr v
  ppr (PatBuilderOverLit l) = ppr l

-- | An accumulator to build a prefix data constructor,
--   e.g. when parsing @MkT A B C@, the accumulator will evolve as follows:
--
--  @
--  1. PrefixDataConBuilder []        MkT
--  2. PrefixDataConBuilder [A]       MkT
--  3. PrefixDataConBuilder [A, B]    MkT
--  4. PrefixDataConBuilder [A, B, C] MkT
--  @
--
--  There are two reasons we have a separate builder type instead of using
--  @HsConDeclDetails GhcPs@ directly:
--
--  1. It's faster, because 'OrdList' gives us constant-time snoc.
--  2. Having a separate type helps ensure that we don't forget to finalize a
--     'RecTy' into a 'RecCon' (we do that in 'dataConBuilderDetails').
--
--  See Note [PatBuilder] for another builder type used in the parser.
--  Here the technique is similar, but the motivation is different.
data DataConBuilder
  = PrefixDataConBuilder
      (OrdList (LHsType GhcPs))  -- Data constructor fields
      (LocatedN RdrName)         -- Data constructor name
  | InfixDataConBuilder
      (LHsType GhcPs)    -- LHS field
      (LocatedN RdrName) -- Data constructor name
      (LHsType GhcPs)    -- RHS field

instance Outputable DataConBuilder where
  ppr (PrefixDataConBuilder flds data_con) =
    hang (ppr data_con) 2 (sep (map ppr (toList flds)))
  ppr (InfixDataConBuilder lhs data_con rhs) =
    ppr lhs <+> ppr data_con <+> ppr rhs

type instance Anno [LocatedA (StmtLR GhcPs GhcPs (LocatedA (PatBuilder GhcPs)))] = SrcSpanAnnLW
