module P where

import qualified Map
import qualified Set

foo = do
    let x = Map.insert 0 "foo"
          . Map.insert (6 :: Int) "foo"
          $ Map.empty
    print (Map.lookup 1 x)
    print (Set.size (Map.keysSet x))
    return x
