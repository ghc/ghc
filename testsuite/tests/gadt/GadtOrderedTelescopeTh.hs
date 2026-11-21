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

main :: IO ()
main = print (equalRebuilt (Rebuilt 1 @Bool True),
              equalQuoted (Quoted 2 @Double (0/0)))
