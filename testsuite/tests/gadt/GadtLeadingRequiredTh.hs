{-# LANGUAGE ExplicitNamespaces, GADTs, RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell, TypeAbstractions, TypeApplications, ViewPatterns #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import GadtLeadingRequiredThProvider
import qualified Language.Haskell.TH as TH

$(do
    let binderName (TH.PlainTV n _) = n
        binderName (TH.KindedTV n _ _) = n
        specificity (TH.PlainTV _ s) = s
        specificity (TH.KindedTV _ s _) = s

        check args = case args of
          [TH.GadtRequiredArgs _ [a],
           TH.GadtContextArgs [TH.AppT (TH.ConT eq) (TH.VarT eqArg)],
           TH.GadtInvisibleArgs _ [b, c],
           TH.GadtContextArgs [TH.AppT (TH.ConT showClass) (TH.VarT showArg)],
           TH.GadtValueArg _ _ (TH.VarT field),
           TH.GadtValueArg (TH.Bang _ TH.SourceStrict) _
             (TH.AppT (TH.AppT (TH.TupleT 2) (TH.VarT left)) (TH.VarT right))]
            | eq == ''Eq, showClass == ''Show
            , specificity b == TH.InferredSpec, specificity c == TH.SpecifiedSpec
            , binderName a == eqArg, binderName a == field
            , binderName b == left, binderName c == right, binderName c == showArg
            -> pure ()
          _ -> fail (show args)

    quoted <- [d|
      data Quoted where
        Quoted :: forall a -> Eq a => forall {b} c. Show c => a -> !(b, c) -> Quoted
      |]
    case quoted of
      [TH.DataD [] name [] _ [TH.RetainedGadtC [_] args (TH.ConT result)] []]
        | name == result -> check args
      _ -> fail (show quoted)

    info <- TH.reify ''T
    case info of
      TH.TyConI (TH.DataD [] _ [] kind [TH.RetainedGadtC [_] args (TH.ConT result)] [])
        | result == ''T -> do
          check args
          let local = TH.mkName "Rebuilt"
          pure (TH.DataD [] local [] kind [TH.RetainedGadtC [local] args (TH.ConT local)] [] : quoted)
      _ -> fail (show info))

view :: Rebuilt -> (Bool, String)
view (Rebuilt (type a) @c ((\x -> x == x) -> same) ((\(_, x) -> show x) -> shown)) =
  (same, shown)

throws :: a -> IO Bool
throws x = either (const True) (const False)
         <$> (try (evaluate x >> pure ()) :: IO (Either ErrorCall ()))

main :: IO ()
main = do
  partial <- throws (Quoted (type Int) @Bool 0)
  quoted <- throws (Quoted (type Int) @Bool 0 (error "quoted"))
  rebuilt <- throws (Rebuilt (type Int) @Bool 0 (error "rebuilt"))
  print (partial, quoted, rebuilt, view (Rebuilt (type Int) @Bool 0 ('x', True)))
