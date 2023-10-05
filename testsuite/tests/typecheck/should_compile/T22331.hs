{-# LANGUAGE TypeFamilies #-}

module T22331 where

import Data.Coerce

data family Fool a

-- This works
joe :: Coercible (Fool a) (Fool b) => Fool a -> Fool b
joe = coerce

-- This does not
bob :: Coercible (Fool a) (Fool b) => Fool b -> Fool a
bob = coerce
