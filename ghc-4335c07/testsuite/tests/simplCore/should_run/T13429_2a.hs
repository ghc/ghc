{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module T13429_2a where

data D

data Array r ix e = Array { _size :: ix
                          , _index :: ix -> e }

class Show ix => Index ix

instance Index Int

class Index ix => Massiv r ix e where
  size :: Array r ix e -> ix
  makeArray :: ix -> (ix -> e) -> Array r ix e
  index :: Array r ix e -> ix -> e


instance Massiv r ix e => Show (Array r ix e) where
  show arr = "<Array " ++ show (size arr) ++ ">"


instance Index ix => Massiv D ix e where
  size = _size
  makeArray = Array
  index = _index


-- | Map a function over an array (restricted return type)
map :: Massiv r' ix e' => (e' -> e) -> Array r' ix e' -> Array D ix e
map = mapG
{-# INLINE map #-}

-- | Map a function over an array (general)
mapG :: (Massiv r' ix e', Massiv r ix e) => (e' -> e) -> Array r' ix e' -> Array r ix e
mapG f arr = makeArray (size arr) (f . index arr)
