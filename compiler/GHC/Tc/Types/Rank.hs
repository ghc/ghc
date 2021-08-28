module GHC.Tc.Types.Rank (Rank(..))  where

import GHC.Base (Bool)
import GHC.Utils.Outputable (Outputable, (<+>), parens, ppr, text)

{-
Note [Higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~
Technically
            Int -> forall a. a->a
is still a rank-1 type, but it's not Haskell 98 (#5957).  So the
validity checker allow a forall after an arrow only if we allow it
before -- that is, with Rank2Types or RankNTypes
-}

data Rank = ArbitraryRank -- Any rank ok

          | LimitedRank   -- Note [Higher rank types]
                 Bool     -- Forall ok at top
                 Rank     -- Use for function arguments

          -- Monotypes that could be a polytype through an extension
          | MonoTypeRankZero   -- RankNTypes
          | MonoTypeTyConArg   -- ImpredicativeTypes
          | MonoTypeSynArg     -- LiberalTypeSynonyms
          | MonoTypeConstraint -- QuantifiedConstraints
          --

          | MustBeMonoType  -- Monotype regardless of flags

instance Outputable Rank where
  ppr ArbitraryRank      = text "ArbitraryRank"
  ppr (LimitedRank top_forall_ok r)
                         = text "LimitedRank" <+> ppr top_forall_ok
                                              <+> parens (ppr r)
  ppr MonoTypeRankZero   = text "MonoTypeRankZero"
  ppr MonoTypeTyConArg   = text "MonoTypeTyConArg"
  ppr MonoTypeSynArg     = text "MonoTypeSynArg"
  ppr MonoTypeConstraint = text "MonoTypeConstraint"
  ppr MustBeMonoType     = text "MustBeMonoType"
