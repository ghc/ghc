{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables #-}
module ScopedClassMethodClient (twoDictionaries, ordinaryChoices) where

import ScopedClassMethod
import OrdinaryScopedChoice (readChoice)

instance Choose Bool where
    choose x = x

newtype Inverted = Inverted Bool

instance Choose Inverted where
    choose (Inverted x) = not x

selectBoth :: forall a b. (Choose b, Choose a)
           => foreach (x :: a) -> foreach (y :: b)
           -> (Tagged (choose x), Tagged (choose y))
selectBoth x y = (chooseTagged x, chooseTagged y)

-- | Read the two imported selections with different dictionaries.
twoDictionaries :: Bool -> (Bool, Bool)
twoDictionaries x =
    let inverted = Inverted x
    in case selectBoth x inverted of
        (Tagged lhs, Tagged rhs) -> (lhs, rhs)

-- | Apply the Haskell 2010 reader to two distinct instances.
ordinaryChoices :: Bool -> (Bool, Bool)
ordinaryChoices x = (readChoice x, readChoice (Inverted x))
