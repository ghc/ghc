{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module ZipMinimum where

import Data.Type.Equality ((:~:)(Refl))

-- | An unbounded count of list cells.
data Size = Zero | Succ Size

-- | Count the cells without inspecting the elements.
size :: [a] -> Size
size []       = Zero
size (_ : xs) = Succ (size xs)

-- | Return the smaller count.
minSize :: Size -> Size -> Size
minSize Zero     _        = Zero
minSize (Succ _) Zero     = Zero
minSize (Succ m) (Succ n) = Succ (minSize m n)

-- | Pairing two lists produces as many pairs as the shorter list has cells.
lengthZip :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
          -> size (zip xs ys) :~: minSize (size xs) (size ys)
lengthZip []       _        = Refl
lengthZip (_ : _)  []       = Refl
lengthZip (_ : xs) (_ : ys) =
  case lengthZip xs ys of
    Refl -> Refl
