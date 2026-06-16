{-# LANGUAGE ExistentialQuantification #-}

-- Bindings that are normal-form taggable (small-family BoxedRep) values,
-- exported for use from another module.  Each is OPAQUE so the simplifier
-- cannot inline the constructor application into the use site: the only way a
-- consumer can scrutinise these without entering an untagged closure is if the
-- constructor's pointer tag is conveyed across the module boundary via the
-- serialized LambdaFormInfo.
module T24136A (E(..), Box(..), theE0, theE1, theE2, aBox) where

data E = E0 | E1 Int | E2

data Box = forall a. Show a => Box a

{-# OPAQUE theE0 #-}
theE0 :: E
theE0 = E0

{-# OPAQUE theE1 #-}
theE1 :: E
theE1 = E1 7

{-# OPAQUE theE2 #-}
theE2 :: E
theE2 = E2

-- An existential package: scrutinising the Box enters the wrapped Show
-- dictionary, which is itself a taggable value.
{-# OPAQUE aBox #-}
aBox :: Box
aBox = Box (42 :: Int)
