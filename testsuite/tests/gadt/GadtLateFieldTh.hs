{-# LANGUAGE ExplicitNamespaces, GADTs, RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell, TypeApplications #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import qualified Language.Haskell.TH as TH
import GadtLateFieldProvider

$(do
    info <- TH.reify ''Late
    let local = TH.mkName "Rebuilt"
    case info of
      TH.TyConI (TH.DataD [] _ [] kind [TH.RetainedGadtC [_]
        args@[TH.GadtValueArg _ _ (TH.ConT int),
             TH.GadtRequiredArgs _ [TH.KindedTV a () _],
             TH.GadtValueArg (TH.Bang _ TH.SourceStrict) _ (TH.VarT field)]
        (TH.ConT result)] [])
        | int == ''Int, a == field, result == ''Late ->
          pure [TH.DataD [] local [] kind
                  [TH.RetainedGadtC [local] args (TH.ConT local)] []]
      _ -> fail (show info))

$(do
    decs <- [d| data Quoted where
                  Quoted :: Int -> forall a -> !a -> Quoted
                  Context :: Int -> forall {a} b. Show b => !(a, b) -> Quoted
             |]
    case decs of
      [TH.DataD [] _ [] _
        [TH.RetainedGadtC [_]
           [TH.GadtValueArg _ _ _,
            TH.GadtRequiredArgs _ [TH.PlainTV t ()],
            TH.GadtValueArg (TH.Bang _ TH.SourceStrict) _ (TH.VarT field)] _,
         TH.RetainedGadtC [_]
           [TH.GadtValueArg _ _ _,
            TH.GadtInvisibleArgs _ [TH.PlainTV a TH.InferredSpec, TH.PlainTV b TH.SpecifiedSpec],
            TH.GadtContextArgs [TH.AppT (TH.ConT cls) (TH.VarT predicate)],
            TH.GadtValueArg (TH.Bang _ TH.SourceStrict) _
              (TH.AppT (TH.AppT (TH.TupleT 2) (TH.VarT left)) (TH.VarT right))] _] []]
        | t == field, cls == ''Show, a == left, b == predicate, b == right -> pure decs
      _ -> fail (show decs))

throws :: a -> IO Bool
throws x = either (const True) (const False) <$> (try (evaluate x >> pure ()) :: IO (Either ErrorCall ()))

main :: IO ()
main = do
  partial <- throws (Late (error "prefix") (type Bool))
  prefix <- throws (Late (error "prefix") (type Bool) True)
  late <- throws (Late 0 (type Bool) (error "late"))
  rebuilt <- throws (Rebuilt 0 (type Bool) (error "rebuilt"))
  quoted <- throws (Quoted 0 (type Bool) (error "quoted"))
  context <- throws (Context 0 @Bool (error "context"))
  print (partial, prefix, late, rebuilt, quoted, context)
