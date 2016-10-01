{-# LANGUAGE NamedFieldPuns, PatternSynonyms, RecordWildCards #-}
module T11987 where

import T11987a

-- works
namedFieldPuns :: (Int,Int)
namedFieldPuns = let { x = 1; y = 2 } in Point { x, y }

-- error: Pattern synonym ‘Point’ used as a data constructor
recordWildCards :: (Int,Int)
recordWildCards = let { x = 1; y = 2 } in Point { .. }
