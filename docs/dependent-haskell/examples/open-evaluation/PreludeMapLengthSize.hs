{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module PreludeMapLengthSize (Size(..), size, minSize, eqSize) where

-- | Count a finite spine in successors.
data Size = Zero | Succ Size
    deriving Show

-- | Count the list constructors without inspecting the elements.
size :: [a] -> Size
size [] = Zero
size (_ : xs) = Succ (size xs)

-- | Take the smaller count. A zero on the left determines the answer.
minSize :: Size -> Size -> Size
minSize Zero _ = Zero
minSize (Succ _) Zero = Zero
minSize (Succ m) (Succ n) = Succ (minSize m n)

-- | Compare two sizes by inspecting their constructors.
eqSize :: Size -> Size -> Bool
eqSize Zero Zero = True
eqSize Zero (Succ _) = False
eqSize (Succ _) Zero = False
eqSize (Succ m) (Succ n) = eqSize m n
