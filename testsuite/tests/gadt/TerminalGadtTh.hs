{-# LANGUAGE DataKinds, ExplicitNamespaces, GADTs, LinearTypes, RequiredTypeArguments, TemplateHaskell, TypeApplications #-}
module Main where

import Control.Exception (SomeException, evaluate, try)
import Data.Kind (Type)
import GHC.Exts (Multiplicity(Many))
import qualified Language.Haskell.TH as TH
import qualified TerminalGadtThProvider as P

$([d|
  data Quoted (a :: Type) (b :: Type) (c :: Type) where
    QuoteAfter :: forall a. Eq a => (Show a => !a %Many -> forall b c -> Quoted a b c)
  |])

$(do
  let copyName = TH.mkName "Copied"
      rename ty = case ty of
        TH.AppT fun arg -> TH.AppT (rename fun) arg
        TH.ConT n | n == ''P.Original -> TH.ConT copyName
        other -> other
      tvName (TH.PlainTV n _) = n
      tvName (TH.KindedTV n _ _) = n
      copy (TH.ForallMatchC ma con) = TH.ForallMatchC ma <$> copy con
      copy (TH.ForallC binders ctx con) = TH.ForallC binders ctx <$> copy con
      copy (TH.RetainedGadtC [n] fields@[
              TH.GadtValueArg (TH.Bang _ TH.SourceStrict) (TH.PromotedT mult) (TH.VarT a)]
              (TH.ForallMatchT ma (TH.ForallVisT tvs@[b, c] result)))
        | TH.nameBase n == "After", mult == 'Many
        , TH.AppT (TH.AppT (TH.AppT (TH.ConT parent) (TH.VarT a')) (TH.VarT b')) (TH.VarT c') <- result
        , parent == ''P.Original, a' == a, tvName b == b', tvName c == c' =
            pure (TH.RetainedGadtC [TH.mkName "CopyAfter"] fields
                  (TH.ForallMatchT ma (TH.ForallVisT tvs (rename result))))
      copy con = fail (show con)
  TH.reify ''P.Original >>= \info -> case info of
    TH.TyConI (TH.DataD cxt _ tvs kind cons derivs) -> do
      cons' <- traverse copy cons
      pure [TH.DataD cxt copyName tvs kind cons' derivs]
    _ -> fail (show info))

reifiedUse :: $(TH.reifyType 'P.After)
reifiedUse = P.After

dupOriginal :: P.Original a b c %1 -> (a, a)
dupOriginal (P.After x (type _) (type _)) = (x, x)

dupQuoted :: Quoted a b c %1 -> (a, a)
dupQuoted (QuoteAfter x (type _) (type _)) = (x, x)

dupCopied :: Copied a b c %1 -> (a, a)
dupCopied (CopyAfter x (type _) (type _)) = (x, x)

strictWHNF :: a -> IO Bool
strictWHNF x = either (const True) (const False) <$> try @SomeException (evaluate x)

main :: IO ()
main = do
  print (dupOriginal (reifiedUse @Int 11 (type Char) (type Bool)))
  print (dupQuoted (QuoteAfter @Int 23 (type Char) (type Bool)))
  print (dupCopied (CopyAfter @Int 37 (type Char) (type Bool)))
  let bottom = error "strict field" :: Int
  a <- strictWHNF (P.After @Int bottom (type Char) (type Bool))
  b <- strictWHNF (QuoteAfter @Int bottom (type Char) (type Bool))
  c <- strictWHNF (CopyAfter @Int bottom (type Char) (type Bool))
  print (a, b, c)
