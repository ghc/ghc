{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module T24621_th where

import Data.Function

foo :: a -> a
foo x = x

$( [d| {-# RULES "" forall a b c. a * c + b * c = (a + b) * c :: Int #-} |] )
$( [d| {-# RULES "." forall a b. (.) a b foo = a (b foo) #-} |] )
$( [d| {-# RULES "foo" forall a b. foo a b = a b #-} |] )
