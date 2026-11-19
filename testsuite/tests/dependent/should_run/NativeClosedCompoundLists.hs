{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FlexibleInstances, KindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Type.Equality ((:~:)(Refl))
import qualified Data.Either
import qualified Data.List
import qualified Data.Maybe
import Numeric.Natural (Natural)
import qualified NativeClosedCompoundListsLib as P

instance {-# OVERLAPPING #-} Show [[Natural]] where
  show _ = "[[99]]"

instance {-# OVERLAPPING #-} Show [Either Char Natural] where
  show _ = "[]"

suffixes :: Data.List.tails '[1, 2, 3] :~: '[ '[1, 2, 3], '[2, 3], '[3], '[]]
suffixes = Refl

flattened :: Prelude.concat (Data.List.tails '[1, 2, 3]) :~: '[1, 2, 3, 2, 3, 3]
flattened = Refl

maybes :: P.maybes 2 :~: '[ 'Nothing, 'Just 2, 'Nothing, 'Just 3]
maybes = Refl

eitherValues :: P.alternatives 2 :~: '[ 'Right 2, 'Left '\n', 'Right 3, 'Left 'é']
eitherValues = Refl

lefts :: Data.Either.lefts (P.alternatives 2) :~: '[ '\n', 'é']
lefts = Refl

nestedAlternatives
  :: Data.Either.lefts (Prelude.concat (Data.List.tails (P.alternatives 2)))
       :~: '[ '\n', 'é', '\n', 'é', 'é', 'é']
nestedAlternatives = Refl

empty :: P.emptyCompound 1 :~: ('[] :: [Maybe Natural])
empty = Refl

literals :: ( Prelude.concat '[ '[], '[ 'x'], '[]] :~: '[ 'x']
            , Data.Maybe.catMaybes '[ 'Nothing, 'Just 3] :~: '[3]
            , Data.Either.lefts '[ 'Left 'x', 'Right 3] :~: '[ 'x']
            , P.readPair '( 'Just 3, 'Right 'x') :~: 4
            , P.selected ('Right 'x') 'True 'False :~: 'False
            , P.emptyWith '[] ('Just 'x') :~: 'True
            , P.ignoreCompound '[] :~: 7 )
literals = (Refl, Refl, Refl, Refl, Refl, Refl, Refl)

ignoredElements :: P.ignoreCompound (P.bottomElement 1) :~: 7
ignoredElements = Refl

main :: IO ()
main = print (show ([[1, 2], []] :: [[Natural]]), show (P.alternatives 2))
