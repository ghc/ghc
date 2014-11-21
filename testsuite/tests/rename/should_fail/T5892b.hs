{-# LANGUAGE RecordWildCards #-}
module T5892b where

import Data.Tree ( Tree( Node, rootLabel ))
-- Not importing field 'subForest'

Node{..} = Node [] []
-- Binds 'rootLabel' only

foo = T5892b.rootLabel
bar = T5892b.subForest
