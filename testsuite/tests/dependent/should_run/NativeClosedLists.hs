{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
{-# LANGUAGE NamedWildCards, PartialTypeSignatures, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
module Main (main) where

import Prelude
import qualified Data.Char
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeLits (natVal)
import Numeric.Natural (Natural)
import qualified NativeClosedListsLib as Lib

reversed :: Prelude.reverse '[2, 3, 5] :~: '[5, 3, 2]
reversed = Refl

emptyInput :: Prelude.reverse '[] :~: '[]
emptyInput = Refl

emptyResult :: Lib.empty '[2, 3, 5] :~: '[]
emptyResult = Refl

bools :: Lib.flags '[2, 3, 5] :~: '[False, True, True]
bools = Refl

emptyBools :: Lib.flags '[] :~: ('[] :: [Bool])
emptyBools = Refl

chars :: Lib.characters '\n' :~: '[ '"', '\\', '\n', '\SO', 'H', 'λ']
chars = Refl

data Same (xs :: [Natural]) (ys :: [Natural]) where
    Same :: Same xs xs

-- Replay learns the input from the pattern, then the reversed list from Same.
chain :: Proxy _input -> (Proxy (Lib.first _reversed), Same (Lib.reverseNaturals _input) _reversed)
chain (_ :: Proxy '[2, 3, 5]) = (Proxy @5, Same)

-- The inner result determines the specialization of the outer call.
nested :: ( Prelude.sum (Prelude.reverse '[2, 3, 5]) :~: 10
          , Lib.first (Prelude.reverse '[2, 3, 5]) :~: 5
          , Data.Char.toUpper (Data.Char.toLower 'Z') :~: 'Z' )
nested = (Refl, Refl, Refl)

main :: IO ()
main = case (reversed, emptyInput, emptyResult, bools, emptyBools, chars) of
    (Refl, Refl, Refl, Refl, Refl, Refl)
        | show [5, 3, 2 :: Natural] == "[2,3,5]" ->
            case nested of
                (Refl, Refl, Refl) -> print (natVal (fst (chain (Proxy @'[2, 3, 5]))))
        | otherwise -> fail "ordinary Show did not select the overlapping list instance"
