{-# LANGUAGE CPP #-}

#include "containers.h"

-- | This module simply holds deprecated copies of functions from
-- Data.Map.Internal.Debug.
module Data.Map.Internal.DeprecatedShowTree where

import qualified Data.Map.Internal.Debug as Debug
import Data.Map.Internal (Map)

-- | /O(n)/. Show the tree that implements the map. The tree is shown
-- in a compressed, hanging format. See 'showTreeWith'.
{-# DEPRECATED showTree "'showTree' is now in \"Data.Map.Internal.Debug\"" #-}
showTree :: (Show k,Show a) => Map k a -> String
showTree = Debug.showTree

{- | /O(n)/. The expression (@'showTreeWith' showelem hang wide map@) shows
 the tree that implements the map. Elements are shown using the @showElem@ function. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.

>  Map> let t = fromDistinctAscList [(x,()) | x <- [1..5]]
>  Map> putStrLn $ showTreeWith (\k x -> show (k,x)) True False t
>  (4,())
>  +--(2,())
>  |  +--(1,())
>  |  +--(3,())
>  +--(5,())
>
>  Map> putStrLn $ showTreeWith (\k x -> show (k,x)) True True t
>  (4,())
>  |
>  +--(2,())
>  |  |
>  |  +--(1,())
>  |  |
>  |  +--(3,())
>  |
>  +--(5,())
>
>  Map> putStrLn $ showTreeWith (\k x -> show (k,x)) False True t
>  +--(5,())
>  |
>  (4,())
>  |
>  |  +--(3,())
>  |  |
>  +--(2,())
>     |
>     +--(1,())

-}
{-# DEPRECATED showTreeWith "'showTreeWith' is now in \"Data.Map.Internal.Debug\"" #-}
showTreeWith :: (k -> a -> String) -> Bool -> Bool -> Map k a -> String
showTreeWith = Debug.showTreeWith
