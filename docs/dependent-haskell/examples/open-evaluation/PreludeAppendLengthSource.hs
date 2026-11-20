{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module PreludeAppendLengthSource (Appended, Plus, plus) where

import qualified Prelude as P
import PreludeMapLengthSize (Size(..))

-- | Apply the installed append function to two lists.
type Appended :: forall a. [a] -> [a] -> [a]
type Appended xs ys = (P.++) xs ys

-- | Add two sizes by recursion on the first.
plus :: Size -> Size -> Size
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

-- | Use the checked equations of size addition.
type Plus :: Size -> Size -> Size
type Plus m n = plus m n
