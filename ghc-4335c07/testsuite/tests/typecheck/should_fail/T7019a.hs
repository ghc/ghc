{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes           #-}

module T7019a where

class Context c where
  func1 :: c -> String

-- Illegal forall in context
class (forall b. Context (Associated a b)) => Class a where
  data Associated a :: * -> *


