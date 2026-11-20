{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -fno-ignore-interface-pragmas #-}
module ConsRuleMapProof (negateHead, lazySpine, twiceHead, composedHead) where

import qualified ConsRuleMap as M
import ConsRuleMapSource (NegateAll, NegateTwice)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))

-- | Check the head equation while rewrite rules are enabled.
negateHead :: Proxy xs -> NegateAll ('True ': xs) :~: ('False ': NegateAll xs)
negateHead _ = Refl

-- | Observe the cons without forcing the callback or either field.
lazySpine :: Bool
lazySpine = case M.mapper (undefined :: Bool -> Bool) (undefined : undefined) of
    _ : _ -> True
    [] -> False

-- | Check that two negations preserve a True head with rules enabled.
twiceHead :: Proxy xs -> NegateTwice ('True ': xs) :~: ('True ': NegateTwice xs)
twiceHead _ = Refl

-- | Observe the head when the outer callback ignores the intermediate value.
{-# NOINLINE composedHead #-}
composedHead :: [Bool] -> Bool
composedHead xs = case M.mapper (const True) (M.mapper (undefined :: Bool -> Bool) xs) of
    x : _ -> x
    [] -> False
