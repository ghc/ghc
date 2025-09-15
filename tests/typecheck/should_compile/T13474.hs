module T13474 where

import qualified Data.Map as M

class Default a where
  def :: a

foo :: Default a => b -> a
foo x = def

mapdef :: Default v => M.Map k v -> M.Map k v
mapdef = M.map foo

