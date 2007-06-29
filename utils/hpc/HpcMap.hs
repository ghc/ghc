module HpcMap ( module HpcMap ) where

#if __GLASGOW_HASKELL__ < 604
import qualified Data.FiniteMap as Map
#else
import qualified Data.Map as Map
#endif


lookup :: Ord key => key -> Map key elt -> Maybe elt
fromList :: Ord key => [(key,elt)] -> Map key elt


#if __GLASGOW_HASKELL__ < 604
type Map key elt = Map.FiniteMap key elt

lookup = flip Map.lookupFM
fromList = Map.listToFM 

#else

type Map key elt = Map.Map key elt

lookup = Map.lookup
fromList = Map.fromList

#endif
