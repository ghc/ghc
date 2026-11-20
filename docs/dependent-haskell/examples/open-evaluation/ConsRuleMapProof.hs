{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -fno-ignore-interface-pragmas #-}
module ConsRuleMapProof (negateHead, lazySpine) where

import qualified ConsRuleMap as M
import ConsRuleMapSource (NegateAll)
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
