{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods -Wno-unused-matches #-}

module LocalGivenEqs where

-- See Note [When does an implication have given equalities?] in GHC.Tc.Solver.Monad;
-- this tests custom treatment for LocalGivenEqs

import Data.Kind ( Type )
import GHC.Exts ( Any )

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)
newtype HappyWrap201 = HappyWrap201 (ECP)
newtype HappyWrap205 = HappyWrap205 (([Located Token],Bool))

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
type HappyAny = Any

newtype ECP =
  ECP { unECP :: forall b. DisambECP b => PV (Located b) }

data PV a
data P a
data GhcPs
data Token
data Located a
data AnnKeywordId = AnnIf | AnnThen | AnnElse | AnnSemi
data AddAnn
data SrcSpan
type LHsExpr a = Located (HsExpr a)
data HsExpr a

class b ~ (Body b) GhcPs => DisambECP b where
  type Body b :: Type -> Type
  mkHsIfPV :: SrcSpan
         -> LHsExpr GhcPs
         -> Bool  -- semicolon?
         -> Located b
         -> Bool  -- semicolon?
         -> Located b
         -> PV (Located b)

instance DisambECP (HsExpr GhcPs) where
  type Body (HsExpr GhcPs) = HsExpr
  mkHsIfPV = undefined

instance Functor P
instance Applicative P
instance Monad P

instance Functor PV
instance Applicative PV
instance Monad PV

mj :: AnnKeywordId -> Located e -> AddAnn
mj = undefined

amms :: Monad m => m (Located a) -> [AddAnn] -> m (Located a)
amms = undefined

happyIn208 :: ECP -> HappyAbsSyn
happyIn208 = undefined

happyReturn :: () => a -> P a
happyReturn = (return)

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (>>=)

comb2 :: Located a -> Located b -> SrcSpan
comb2 = undefined

runPV :: PV a -> P a
runPV = undefined

happyOutTok :: HappyAbsSyn -> Located Token
happyOutTok = undefined

happyOut201 :: HappyAbsSyn -> HappyWrap201
happyOut201 = undefined

happyOut205 :: HappyAbsSyn -> HappyWrap205
happyOut205 = undefined

happyReduction_508 (happy_x_8 `HappyStk`
        happy_x_7 `HappyStk`
        happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest) tk
         = happyThen ((case happyOutTok happy_x_1 of { happy_var_1 ->
        case happyOut201 happy_x_2 of { (HappyWrap201 happy_var_2) ->
        case happyOut205 happy_x_3 of { (HappyWrap205 happy_var_3) ->
        case happyOutTok happy_x_4 of { happy_var_4 ->
        case happyOut201 happy_x_5 of { (HappyWrap201 happy_var_5) ->
        case happyOut205 happy_x_6 of { (HappyWrap205 happy_var_6) ->
        case happyOutTok happy_x_7 of { happy_var_7 ->
        case happyOut201 happy_x_8 of { (HappyWrap201 happy_var_8) ->
        ( runPV (unECP happy_var_2) >>= \ happy_var_2 ->
                            return $ ECP $
                              unECP happy_var_5 >>= \ happy_var_5 ->
                              unECP happy_var_8 >>= \ happy_var_8 ->
                              amms (mkHsIfPV (comb2 happy_var_1 happy_var_8) happy_var_2 (snd happy_var_3) happy_var_5 (snd happy_var_6) happy_var_8)
                                  (mj AnnIf happy_var_1:mj AnnThen happy_var_4
                                     :mj AnnElse happy_var_7
                                     :(map (\l -> mj AnnSemi l) (fst happy_var_3))
                                    ++(map (\l -> mj AnnSemi l) (fst happy_var_6))))}}}}}}}})
        ) (\r -> happyReturn (happyIn208 r))
