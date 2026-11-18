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

emptyInput :: Prelude.reverse '[] :~: ('[] :: [Natural])
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
nested :: forall. ( sum (reverse [2, 3, 5]) :~: 10
          , Lib.first (Prelude.reverse '[2, 3, 5]) :~: 5
          , Data.Char.toUpper (Data.Char.toLower 'Z') :~: 'Z' )
nested = (Refl, Refl, Refl)

-- The kind of a checked result determines its runtime representation.
computed :: ( Prelude.odd (Prelude.sum '[2, 3, 5]) :~: 'False
            , Prelude.id (Prelude.sum '[18446744073709551616, 1]) :~: 18446744073709551617
            , Prelude.not (Data.Char.isDigit 'x') :~: 'True
            , Prelude.and (Lib.flags '[1, 2]) :~: 'False
            , Prelude.and (Lib.flags '[]) :~: 'True
            , Prelude.reverse (Lib.characters '\n') :~: '[ 'λ', 'H', '\SO', '\n', '\\', '"'] )
computed = (Refl, Refl, Refl, Refl, Refl, Refl)

source :: ( Prelude.odd 5 :~: 'True
          , Prelude.not 'False :~: 'True
          , Prelude.and '[ 'True, 'False] :~: 'False
          , Prelude.and '[] :~: 'True
          , Prelude.reverse '[ 'λ', '\n'] :~: '[ '\n', 'λ'] )
source = (Refl, Refl, Refl, Refl, Refl)

main :: IO ()
main = case (reversed, emptyInput, emptyResult, bools, emptyBools, chars) of
    (Refl, Refl, Refl, Refl, Refl, Refl)
        | show [5, 3, 2 :: Natural] == "[2,3,5]" ->
            case (nested, computed, source) of
                ((Refl, Refl, Refl), (Refl, Refl, Refl, Refl, Refl, Refl), (Refl, Refl, Refl, Refl, Refl)) ->
                    print (natVal (fst (chain (Proxy @'[2, 3, 5]))))
        | otherwise -> fail "ordinary Show did not select the overlapping list instance"
