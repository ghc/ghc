{-# LANGUAGE OverloadedLabels, TransformListComp, MonadComprehensions, MagicHash #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}

module T25065a where
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Exts

data A = A
instance IsLabel "x" (A -> Int) where fromLabel A = 1

blah :: [A] -> [(Int, [A])]
blah as = [(the i, a) | a <- as, let i = #x a, then group by i using groupWith]

