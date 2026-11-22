{-# LANGUAGE DataKinds, ExplicitNamespaces, NoListTuplePuns, PolyKinds, RequiredTypeArguments, TemplateHaskell #-}
module Main where

import qualified Forward as Ty (type Key)
import qualified Forward as Val (data Key)
import Data.Tuple.Experimental (Unit, Tuple2)
import Language.Haskell.TH (AnnLookup(..), reifyAnnotations)
import Language.Haskell.TH.Syntax (lift)

keep :: forall a -> a -> a
keep (type a) x = x

data Witness a = MkWitness

promoted :: Witness Val.Key
promoted = MkWitness

annotations :: Tuple2 String String
annotations = $(do
    ty <- reifyAnnotations (AnnLookupName ''Ty.Key)
    val <- reifyAnnotations (AnnLookupName 'Val.Key)
    case (ty, val) of
      ([t], [v]) -> lift (t :: String, v :: String)
      _ -> fail "expected one annotation in each namespace")

main :: IO Unit
main = case (keep (type Ty.Key) Val.Key, promoted) of
    (Val.Key, MkWitness) -> print annotations
