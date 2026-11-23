{-# LANGUAGE DataKinds, ExplicitNamespaces, GADTs, PolyKinds, TypeOperators #-}
{-# LANGUAGE RequiredTypeArguments, TemplateHaskell, TypeData #-}
module Main where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GadtOrderedTypeDataProvider
import qualified Language.Haskell.TH as TH

$( [d| type data Quoted b where
         MakeQuoted :: Bool -> forall b -> Bool -> Quoted b
     |] )

$( do declaration <- TH.reify ''U
      let ordered (TH.ForallMatchC _ con) = ordered con
          ordered (TH.ForallC _ [] con) = ordered con
          ordered (TH.RetainedGadtC [_] [TH.GadtValueArg{}, TH.GadtRequiredArgs{}, TH.GadtValueArg{}] _) = True
          ordered _ = False
      case declaration of
        TH.TyConI (TH.TypeDataD _ _ _ [constructor]) | ordered constructor -> pure []
        _ -> fail (show declaration) )

invisible :: Proxy (K @Type Int @Bool)
invisible = Proxy

required :: Proxy (R @Type Int Bool 'True)
required = Proxy

equation :: Proxy (Q 'True @Int @Bool)
equation = Proxy

symbolic :: Proxy (('True :& 'False) :: H Int)
symbolic = Proxy

fieldless :: Proxy (F0 @Int @Bool)
fieldless = Proxy

quoted :: Proxy (MakeQuoted 'True Int 'False)
quoted = Proxy

main :: IO ()
main = case (invisible, required, equation, symbolic, fieldless, quoted) of
  (Proxy, Proxy, Proxy, Proxy, Proxy, Proxy) -> print True
