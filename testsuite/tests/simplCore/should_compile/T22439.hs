{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

-- This test caused a Lint error because Exitify didn't pin on a
-- bottoming signature when floating.

module Bug where

import qualified Data.Text.Lazy as T
import GHC.Exts (IsList(..))
import Prelude (Bool, Char, otherwise, type (~) )

class (IsList full, item ~ Item full) => ListLike full item | full -> item where
    empty :: full
    cons :: item -> full -> full
    head :: full -> item
    tail :: full -> full
    null :: full -> Bool

    deleteBy :: (item -> item -> Bool) -> item -> full -> full
    deleteBy func i l
        | null l = empty
        | otherwise =
            if func i (head l)
               then tail l
               else cons (head l) (deleteBy func i (tail l))

instance ListLike T.Text Char where
    empty = T.empty
    cons = T.cons
    head = T.head
    tail = T.tail
    null = T.null
