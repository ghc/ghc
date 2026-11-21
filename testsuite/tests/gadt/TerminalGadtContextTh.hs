{-# LANGUAGE ExplicitNamespaces, GADTs, NoExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments, TemplateHaskell, TypeApplications #-}
module Main where

import Data.Kind (Type)
import qualified Language.Haskell.TH as TH
import qualified TerminalGadtContextProvider as P
import Type.Reflection (Typeable, typeRep)

$([d|
    data Quoted a where
      Quoted :: Int -> forall (a :: Type) -> Typeable a => Quoted a
  |])

$(do
    original <- TH.reify ''P.IndexedTag
    let rebuilt = TH.mkName "Rebuilt"
    case original of
      TH.TyConI (TH.DataD [] parent vars kind
        [TH.RetainedGadtC [_] args
          (TH.ForallMatchT matchability (TH.ForallVisT binders@[_]
            (TH.ForallT [] context@[_] (TH.AppT (TH.ConT result) index))))] [])
        | result == parent ->
            pure [TH.DataD [] rebuilt vars kind
              [TH.RetainedGadtC [rebuilt] args
                (TH.ForallMatchT matchability (TH.ForallVisT binders
                  (TH.ForallT [] context (TH.AppT (TH.ConT rebuilt) index))))] []]
      _ -> fail (show original))

alias :: $(TH.reifyType 'P.IndexedTag)
alias = P.IndexedTag

main :: IO ()
main = do
  print $ case alias 23 (type Char) of
    P.IndexedTag n (type a) -> (n, show (typeRep @a))
  print $ case Quoted 29 (type Bool) of
    Quoted n (type a) -> (n, show (typeRep @a))
  print $ case Rebuilt 31 (type Ordering) of
    Rebuilt n (type a) -> (n, show (typeRep @a))
