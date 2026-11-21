{-# LANGUAGE GHC2024, NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fplugin=DeferredHeterogeneousPlugin #-}
module Main where

import Data.Coerce (Coercible, coerce)
import GHC.Exts (TYPE)
import HomogeneousSelectorGivensProvider

nominal :: forall k l (a :: k) (b :: l).
           Premise a ~ Premise b => Result a -> Result b
nominal x = x

nominalFromCoercible :: forall k l (a :: k) (b :: l).
                       Coercible (Premise a) (Premise b) => Result a -> Result b
nominalFromCoercible x = x

representational :: forall k l (a :: k) (b :: l).
                    Coercible (RepPremise a) (RepPremise b) => RepResult a -> RepResult b
representational = coerce

functionArgument :: forall r s (a :: TYPE r) (b :: TYPE s).
                    Coercible (a -> ()) (b -> ()) => RepResult a -> RepResult b
functionArgument = coerce

main :: IO ()
main = print ( nominal @_ @_ @'True @'True (Result 11)
             , nominalFromCoercible @_ @_ @Maybe @Maybe (Result 13)
             , representational @_ @_ @Age @Int (RepResult 17)
             , functionArgument @_ @_ @Age @Int (RepResult 19) )
