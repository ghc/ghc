{-# LANGUAGE TypeFamilies, DataKinds, GADTs, FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

-- This module declares some basic types used by GHC.Stg.Lift
-- We can import this module into GHC.Stg.Syntax, where the
-- type instance declartions for BinderP etc live

module GHC.Stg.Lift.Types(
   Skeleton(..),
   bothSk, altSk, rhsSk,

   BinderInfo(..),
   binderInfoBndr, binderInfoOccursAsArg
   ) where

import GHC.Prelude

import GHC.Types.Id
import GHC.Types.Demand
import GHC.Types.Var.Set

import GHC.Utils.Outputable

-- | Captures details of the syntax tree relevant to the cost model, such as
-- closures, multi-shot lambdas and case expressions.
data Skeleton
  = ClosureSk !Id !DIdSet {- ^ free vars -} !Skeleton
  | RhsSk !Card {- ^ how often the RHS was entered -} !Skeleton
  | AltSk !Skeleton !Skeleton
  | BothSk !Skeleton !Skeleton
  | NilSk

bothSk :: Skeleton -> Skeleton -> Skeleton
bothSk NilSk b = b
bothSk a NilSk = a
bothSk a b     = BothSk a b

altSk :: Skeleton -> Skeleton -> Skeleton
altSk NilSk b = b
altSk a NilSk = a
altSk a b     = AltSk a b

rhsSk :: Card -> Skeleton -> Skeleton
rhsSk _        NilSk = NilSk
rhsSk body_dmd skel  = RhsSk body_dmd skel

-- | The type used in binder positions in 'GenStgExpr's.
data BinderInfo
  = BindsClosure !Id !Bool -- ^ Let(-no-escape)-bound thing with a flag
                           --   indicating whether it occurs as an argument
                           --   or in a nullary application
                           --   (see "GHC.Stg.Lift.Analysis#arg_occs").
  | BoringBinder !Id       -- ^ Every other kind of binder

-- | Gets the bound 'Id' out a 'BinderInfo'.
binderInfoBndr :: BinderInfo -> Id
binderInfoBndr (BoringBinder bndr)   = bndr
binderInfoBndr (BindsClosure bndr _) = bndr

-- | Returns 'Nothing' for 'BoringBinder's and 'Just' the flag indicating
-- occurrences as argument or in a nullary applications otherwise.
binderInfoOccursAsArg :: BinderInfo -> Maybe Bool
binderInfoOccursAsArg BoringBinder{}     = Nothing
binderInfoOccursAsArg (BindsClosure _ b) = Just b

instance Outputable Skeleton where
  ppr NilSk = text ""
  ppr (AltSk l r) = vcat
    [ text "{ " <+> ppr l
    , text "ALT"
    , text "  " <+> ppr r
    , text "}"
    ]
  ppr (BothSk l r) = ppr l $$ ppr r
  ppr (ClosureSk f fvs body) = ppr f <+> ppr fvs $$ nest 2 (ppr body)
  ppr (RhsSk card body) = hcat
    [ lambda
    , ppr card
    , dot
    , ppr body
    ]

instance Outputable BinderInfo where
  ppr = ppr . binderInfoBndr

instance OutputableBndr BinderInfo where
  pprBndr b = pprBndr b . binderInfoBndr
  pprPrefixOcc = pprPrefixOcc . binderInfoBndr
  pprInfixOcc = pprInfixOcc . binderInfoBndr
  bndrIsJoin_maybe = bndrIsJoin_maybe . binderInfoBndr

