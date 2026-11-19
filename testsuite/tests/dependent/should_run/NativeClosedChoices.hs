{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FlexibleInstances, TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import qualified Data.Either
import qualified Data.List
import qualified Data.Maybe
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
import qualified NativeClosedChoiceLib as P

instance {-# OVERLAPPING #-} Show (Maybe Natural) where
    show _ = "Nothing"

instance {-# OVERLAPPING #-} Show (Either String (Maybe (Natural, Bool))) where
    show _ = "Right Nothing"

optional :: ( Data.Maybe.listToMaybe '[5,7] :~: 'Just 5
            , Data.Maybe.listToMaybe '[] :~: ('Nothing :: Maybe Natural)
            , Data.Maybe.fromMaybe 9 (Data.Maybe.listToMaybe '[]) :~: 9
            , Data.List.uncons '[2,3,5] :~: 'Just '(2, '[3,5]) )
optional = (Refl, Refl, Refl, Refl)

alternatives :: ( P.classify 0 :~: 'Left '[ 'a', '\n', '"', 'λ']
                , P.classify 1 :~: 'Right 'Nothing
                , P.classify 3 :~: 'Right ('Just '(3, 'True))
                , P.score (P.classify 0) :~: 10
                , P.score (P.classify 1) :~: 20
                , P.score (P.classify 3) :~: 33
                , P.score (P.classify 18446744073709551617) :~: 18446744073709551647
                , Data.Either.isLeft (P.classify 0) :~: 'True )
alternatives = (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl)

main :: IO ()
main = case (optional, alternatives) of
    ((Refl, Refl, Refl, Refl), (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl)) ->
        print (show (Just (5 :: Natural)), show (P.classify 0))
