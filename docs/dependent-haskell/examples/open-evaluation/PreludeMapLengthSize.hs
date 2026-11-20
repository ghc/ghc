{-# LANGUAGE ExperimentalDependentTypes #-}
module PreludeMapLengthSize (Size(..), size) where

-- | Count a finite spine in successors.
data Size = Zero | Succ Size
    deriving Show

-- | Count the list constructors without inspecting the elements.
size :: [a] -> Size
size [] = Zero
size (_ : xs) = Succ (size xs)
