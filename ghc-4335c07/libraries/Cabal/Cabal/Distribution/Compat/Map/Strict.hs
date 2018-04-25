{-# LANGUAGE CPP #-}

-- For bootstrapping GHC
#ifdef MIN_VERSION_containers
#if MIN_VERSION_containers(0,5,0)
#define HAVE_containers_050
#endif
#endif

module Distribution.Compat.Map.Strict
    ( module X
#ifdef HAVE_containers_050
#else
    , insertWith
    , fromSet
#endif
    ) where

#ifdef HAVE_containers_050
import Data.Map.Strict as X
#else
import Data.Map as X hiding (insertWith, insertWith')
import qualified Data.Map
import qualified Data.Set

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith = Data.Map.insertWith'

fromSet :: (k -> a) -> Data.Set.Set k -> Map k a
fromSet f = Data.Map.fromDistinctAscList . Prelude.map (\k -> (k, f k)) . Data.Set.toList
#endif
