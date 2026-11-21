module MappingTags (Tagged(..), exchange) where

import MappingWorker (MapLike(..))

data Tagged a = Old a | New a

instance MapLike Tagged where
    mapLike = exchange

-- | Apply the callback and change the tag.
exchange :: (a -> b) -> Tagged a -> Tagged b
exchange f (Old x) = New (f x)
exchange f (New x) = Old (f x)
