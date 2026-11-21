{-# LANGUAGE EmptyCase, ExperimentalDependentTypes, GADTs, LinearTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, UndecidableInstances #-}

-- | Store an ordinary list with a position checked against that list.
module ListCursor (Cursor, Selection, cursor, prepare, withIndex) where

import GHC.Sigma (Sigma(Pair))
import PackedVecLibrary (sourceLength)
import ParameterizedIndexLibrary (Fin(..), indexGeneric, mkFinGeneric)

-- | The position is bounded by the list in the same pair.
type Cursor a = some (xs :: [a]) | Fin (sourceLength xs)

-- | Hide the representation of a checked position with its reader.
type Selection a = exists state. (state, state %1 -> a)

-- | Check the offset once before storing the list and its position together.
cursor :: [a] -> Int -> Maybe (Cursor a)
cursor xs n = case mkFinGeneric xs n of
    Nothing -> Nothing
    Just i  -> Just (Pair xs i)

-- | Consume a checked position to select the list element.
readCursor :: Cursor a %1 -> a
readCursor (Pair [] i) = case i of {}
readCursor (Pair (x : _) FZ) = x
readCursor (Pair (_ : xs) (FS i)) = indexGeneric xs i

-- | Package the cursor for a consumer with no knowledge of its representation.
prepare :: forall a. Cursor a -> Selection a
prepare c = (c, readCursor @a)

-- | Consume the bounded position and invoke the supplied continuation once.
withIndex :: forall a b. foreach (xs :: [a]) -> Fin (sourceLength xs) %1 -> (a -> b) %1 -> b
withIndex (x : _) FZ visit = visit x
withIndex (_ : rest) (FS i) visit = withIndex rest i visit
