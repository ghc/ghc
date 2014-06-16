Some extra functions to extend Data.Map

\begin{code}
module FiniteMap (
        insertList,
        insertListWith,
        deleteList,
        foldRight, foldRightWithKey
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

insertList :: Ord key => [(key,elt)] -> Map key elt -> Map key elt
insertList xs m = foldl (\m (k, v) -> Map.insert k v m) m xs

insertListWith :: Ord key
               => (elt -> elt -> elt)
               -> [(key,elt)]
               -> Map key elt
               -> Map key elt
insertListWith f xs m0 = foldl (\m (k, v) -> Map.insertWith f k v m) m0 xs

deleteList :: Ord key => [key] -> Map key elt -> Map key elt
deleteList ks m = foldl (flip Map.delete) m ks

foldRight        :: (elt -> a -> a) -> a -> Map key elt -> a
foldRight        = Map.fold
foldRightWithKey :: (key -> elt -> a -> a) -> a -> Map key elt -> a
foldRightWithKey = Map.foldrWithKey
\end{code}

