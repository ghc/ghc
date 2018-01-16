module Data.IntMap.Internal.DeprecatedDebug where
import qualified Data.IntMap.Internal as IM
import Data.IntMap.Internal (IntMap)

{-# DEPRECATED showTree, showTreeWith
    "These debugging functions will be removed from this module. They are available from Data.IntMap.Internal.Debug."
    #-}

-- | /O(n)/. Show the tree that implements the map. The tree is shown
-- in a compressed, hanging format.
showTree :: Show a => IntMap a -> String
showTree = IM.showTree

{- | /O(n)/. The expression (@'showTreeWith' hang wide map@) shows
 the tree that implements the map. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.
-}
showTreeWith :: Show a => Bool -> Bool -> IntMap a -> String
showTreeWith = IM.showTreeWith
