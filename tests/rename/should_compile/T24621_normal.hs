{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module T24621_normal where

import Data.Function

foo :: a -> a
foo x = x

{-# RULES "" forall a b c. a * c + b * c = (a + b) * c :: Int #-}
{-# RULES "." forall f g. (f . g) foo = f (g foo) #-}
{-# RULES "foo" forall a b. (foo a) b = a b #-}
{-# RULES "on" forall a b. (flip compare `on` foo) a b = compare b a #-}
