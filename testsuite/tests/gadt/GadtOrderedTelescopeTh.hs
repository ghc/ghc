{-# LANGUAGE GADTs, TemplateHaskell, TypeAbstractions, TypeApplications #-}
module Main where

import qualified Language.Haskell.TH as TH
import GadtOrderedTelescopeProvider

$(do
    let rhs = TH.mkName "Rebuilt"
    info <- TH.reify ''Mixed
    case info of
      TH.TyConI (TH.DataD [] _ vars kind
        [TH.RetainedGadtC [_] prefix
          (TH.ForallMatchT ma (TH.ForallT tvs []
            (TH.ForallT [] [predicate]
              (TH.AppT arrow (TH.AppT (TH.ConT familyName) v)))))] [])
        | familyName == ''Mixed ->
          pure [TH.DataD [] rhs vars kind [TH.RetainedGadtC [rhs] prefix
            (TH.ForallMatchT ma (TH.ForallT tvs []
              (TH.ForallT [] [predicate]
                (TH.AppT arrow (TH.AppT (TH.ConT rhs) v)))))] []]
      _ -> fail (show info))

$([d| data Quoted a where
        Quoted :: !Int -> forall a. Eq a => a -> Quoted a
   |])

equalRebuilt :: Rebuilt a -> Bool
equalRebuilt (Rebuilt _ @a x) = x == x

equalQuoted :: Quoted a -> Bool
equalQuoted (Quoted _ @a x) = x == x

$(do
    let local = TH.mkName "RebuiltF2"
    info <- TH.reify ''F2
    case info of
      TH.TyConI (TH.DataD [] _ vars kind
        [TH.ForallMatchC ma (TH.ForallC tvs [ord@(TH.AppT (TH.ConT cls1) (TH.VarT early))]
          (TH.RetainedGadtC [_]
            [field@(TH.GadtValueArg _ _ (TH.VarT fieldName))]
            (TH.ForallT [] [eq@(TH.AppT (TH.ConT cls2) (TH.VarT late))]
              (TH.AppT (TH.ConT parent) (TH.VarT result)))))] [])
        | cls1 == ''Ord, cls2 == ''Eq, parent == ''F2,
          all (== early) [fieldName,late,result] ->
            pure [TH.DataD [] local vars kind
                [TH.ForallMatchC ma (TH.ForallC tvs [ord]
                    (TH.RetainedGadtC [local] [field] (TH.ForallT [] [eq]
                       (TH.AppT (TH.ConT local) (TH.VarT result)))))] []]
      _ -> fail (show info))

$([d| data QuotedF2 a where
        QuotedF2 :: Ord a => a -> Eq a => QuotedF2 a
   |])

alias :: $(TH.reifyType 'MkF2)
alias = MkF2

$(do
    let local = TH.mkName "RebuiltInferredPair"
    info <- TH.reify ''InferredPair
    case info of
      TH.TyConI (TH.DataD [] _ [] kind
        [TH.RetainedGadtC [_] fields
          (TH.ForallMatchT ma
            (TH.ForallT tvs@[TH.KindedTV _ TH.InferredSpec _,
                            TH.KindedTV _ TH.SpecifiedSpec _] []
              (TH.ForallT [] context (TH.AppT field (TH.ConT result)))))] [])
        | result == ''InferredPair ->
            pure [TH.DataD [] local [] kind
              [TH.RetainedGadtC [local] fields
                (TH.ForallMatchT ma (TH.ForallT tvs []
                  (TH.ForallT [] context (TH.AppT field (TH.ConT local)))))] []]
      _ -> fail (show info))

$([d| data QuotedInferredPair where
        QuotedInferredPair :: Int -> forall {a} b. Show b => (a, b) -> QuotedInferredPair
   |])

main :: IO ()
main = do
  print (equalRebuilt (Rebuilt 1 @Bool True),
              equalQuoted (Quoted 2 @Double (0/0)))
  print (case alias @Int 7 of MkF2 x -> x == x,
         case RebuiltF2 @Bool False of RebuiltF2 x -> compare x x,
         case QuotedF2 @Char 't' of QuotedF2 x -> x /= x)
  print (case RebuiltInferredPair 5 @Bool ('x', True) of RebuiltInferredPair _ @b (_, x) -> show @b x,
         case QuotedInferredPair 7 @Char (False, 'y') of QuotedInferredPair _ @b (_, x) -> show @b x)
