{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror #-}

module T5892a where

import Data.Tree ( Tree( Node, rootLabel ))
-- Not importing field 'subForest'

foo :: Tree [Int] -> Tree [Int]
foo (Node {..}) -- Pattern match does not bind 'subForest'
  = let rootLabel = []
    in Node {..}   -- Hence warning here
