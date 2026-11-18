{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Main (main) where

import Data.Char (isDigit, toUpper)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeLits (charVal)
import qualified NativeClosedCharLib as Lib

upper :: Data.Char.toUpper 'x' :~: 'X'
upper = Refl

letterIsNotDigit :: Data.Char.isDigit 'x' :~: False
letterIsNotDigit = Refl

digit :: Data.Char.isDigit '7' :~: True
digit = Refl

unicode :: Data.Char.toUpper '\xE9' :~: '\xC9'
unicode = Refl

escaped :: Data.Char.toUpper '\n' :~: '\n'
escaped = Refl

code :: Lib.code 'A' :~: 65
code = Refl

nonempty :: Lib.nonempty '[1, 2] :~: True
nonempty = Refl

shadow :: forall (toUpper :: Bool -> Bool).
          Proxy (toUpper 'True) -> Proxy (toUpper 'True)
shadow x = x

visibleIdentity :: forall a. a -> Proxy a
visibleIdentity _ = Proxy @(Prelude.id a)

main :: IO ()
main = case (upper, letterIsNotDigit, digit, unicode, escaped, code, nonempty) of
    (Refl, Refl, Refl, Refl, Refl, Refl, Refl) -> print (charVal (Proxy @(Data.Char.toUpper 'x')))
