{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module T26395_Plugin where

-- base
import Prelude hiding ( (<>) )
import qualified Data.Semigroup as S
import Data.List ( partition )
import Data.Maybe
import GHC.TypeNats

-- ghc
import GHC.Builtin.Types.Literals
import GHC.Core.Predicate
import GHC.Core.TyCo.Rep
import GHC.Plugins
import GHC.Tc.Plugin
import GHC.Tc.Types
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence
import GHC.Tc.Utils.TcType
import GHC.Types.Unique.Map

--------------------------------------------------------------------------------

plugin :: Plugin
plugin =
  defaultPlugin
    { pluginRecompile = purePlugin
    , tcPlugin = \ _-> Just $
        TcPlugin
          { tcPluginInit    = pure ()
          , tcPluginSolve   = \ _ -> solve
          , tcPluginRewrite = \ _ -> emptyUFM
          , tcPluginStop    = \ _ -> pure ()
          }
    }

solve :: EvBindsVar -> [Ct] -> [Ct] -> TcPluginM TcPluginSolveResult
solve _ givens wanteds
  -- This plugin only reports inconsistencies among Given constraints.
  | not $ null wanteds
  = pure $ TcPluginOk [] []
  | otherwise
  = do { let givenLinearExprs = mapMaybe linearExprCt_maybe givens
             sols = solutions givenLinearExprs

        ; tcPluginTrace "solveLinearExprs" $
            vcat [ text "givens:" <+> ppr givens
                 , text "linExprs:" <+> ppr givenLinearExprs
                 , text "sols:" <+> ppr (take 1 sols)
                 ]
        ; return $
            if null sols
            then TcPluginContradiction givens
            else TcPluginOk [] []
       }

data LinearExpr =
  LinearExpr
    { constant :: Integer
    , coeffs   :: UniqMap TyVar Integer
    }
instance Semigroup LinearExpr where
  LinearExpr c xs <> LinearExpr d ys =
    LinearExpr ( c + d ) ( plusMaybeUniqMap_C comb xs ys )
    where
      comb a1 a2 =
        let a = a1 + a2
        in if a == 0
           then Nothing
           else Just a

instance Monoid LinearExpr where
  mempty = LinearExpr 0 emptyUniqMap

mapLinearExpr :: (Integer -> Integer) -> LinearExpr -> LinearExpr
mapLinearExpr f (LinearExpr c xs) = LinearExpr (f c) (mapUniqMap f xs)

minusLinearExpr :: LinearExpr -> LinearExpr -> LinearExpr
minusLinearExpr a b = a S.<> mapLinearExpr negate b

instance Outputable LinearExpr where
  ppr ( LinearExpr c xs ) =
    hcat $ punctuate ( text " + " ) $
      ( ppr c : map ppr_var ( nonDetUniqMapToList xs ) )
    where
      ppr_var ( tv, i )
        | i == 1
        = ppr tv
        | i < 0
        = parens ( text "-" <> ppr (abs i) ) <> text "*" <> ppr tv
        | otherwise
        = ppr i <> text "*" <> ppr tv

maxCoeff :: LinearExpr -> Double
maxCoeff ( LinearExpr c xs ) =
  maximum ( map fromInteger ( c : nonDetEltsUniqMap xs ) )


linearExprCt_maybe :: Ct -> Maybe LinearExpr
linearExprCt_maybe ct =
  case classifyPredType (ctPred ct) of
    EqPred NomEq lhs rhs
      | all isNaturalTy [ typeKind lhs, typeKind rhs ]
      , Just e1 <- linearExprTy_maybe lhs
      , Just e2 <- linearExprTy_maybe rhs
      -> Just $ e1 `minusLinearExpr` e2
    _ -> Nothing

isNat :: Type -> Maybe Integer
isNat ty
  | Just (NumTyLit n) <- isLitTy ty
  = Just n
  | otherwise
  = Nothing

linearExprTy_maybe :: Type -> Maybe LinearExpr
linearExprTy_maybe ty
  | Just n <- isNat ty
  = Just $ LinearExpr n emptyUniqMap
  | Just (tc, args) <- splitTyConApp_maybe ty
  = if | tc == typeNatAddTyCon
       , [x, y] <- args
       , Just e1 <- linearExprTy_maybe x
       , Just e2 <- linearExprTy_maybe y
       -> Just $ e1 S.<> e2
       | tc == typeNatSubTyCon
       , [x,y] <- args
       , Just e1 <- linearExprTy_maybe x
       , Just e2 <- linearExprTy_maybe y
       -> Just $ e1 `minusLinearExpr` e2
       | tc == typeNatMulTyCon
       , [x, y] <- args
       ->
        if | Just ( LinearExpr n xs ) <- linearExprTy_maybe x
           , isNullUniqMap xs
           , Just e <- linearExprTy_maybe y
           -> Just $
                if n == 0
                then mempty
                else mapLinearExpr (n *) e
           | Just ( LinearExpr n ys ) <- linearExprTy_maybe y
           , isNullUniqMap ys
           , Just e <- linearExprTy_maybe x
           -> Just $
                if n == 0
                then mempty
                else mapLinearExpr (fromIntegral n *) e
           | otherwise
           -> Nothing
       | otherwise
       -> Nothing
  | Just tv <- getTyVar_maybe ty
  = Just $ LinearExpr 0 ( unitUniqMap tv 1 )
  | otherwise
  = Nothing

-- Brute force algorithm to check whether a system of Diophantine
-- linear equations is solvable in natural numbers.
solutions :: [ LinearExpr ] -> [ UniqMap TyVar Natural ]
solutions eqs =
  let
    (constEqs, realEqs) = partition (isNullUniqMap . coeffs) eqs
    d   = length realEqs
    fvs = nonDetKeysUniqMap $ plusUniqMapList ( map coeffs realEqs )
  in
    if | any ( ( /= 0 ) . evalLinearExpr emptyUniqMap ) constEqs
       -> []
       | d == 0
       -> [ emptyUniqMap ]
       | otherwise
       ->
          let
            m = maximum $ map maxCoeff realEqs
            hadamardBound = sqrt ( fromIntegral $ d ^ d ) * m ^ d
            tests = mkAssignments ( floor hadamardBound ) fvs
          in
            filter ( \ test -> isSolution test realEqs ) tests


mkAssignments :: Natural -> [ TyVar ] -> [ UniqMap TyVar Natural ]
mkAssignments _ [] = [ emptyUniqMap ]
mkAssignments b (v : vs) =
  [ addToUniqMap rest v n
  | n <- [ 0 .. b ]
  , rest <- mkAssignments b vs
  ]

isSolution :: UniqMap TyVar Natural -> [ LinearExpr ] -> Bool
isSolution assig =
  all ( \ expr -> evalLinearExpr assig expr == 0 )

evalLinearExpr :: UniqMap TyVar Natural -> LinearExpr -> Integer
evalLinearExpr vals ( LinearExpr c xs ) = nonDetFoldUniqMap aux c xs
  where
    aux ( tv, coeff ) !acc = acc + coeff * val
      where
        val :: Integer
        val = case lookupUniqMap vals tv of
                 Nothing -> pprPanic "evalLinearExpr: missing tv" (ppr tv)
                 Just v  -> fromIntegral v
