module MappingWorker (MapLike(..), mapMaybeValue) where

-- | Map the elements of an input selected by its type constructor.
class MapLike t where
    mapLike :: (a -> b) -> t a -> t b

instance MapLike Maybe where
    mapLike = mapMaybeValue

-- | Preserve the constructor and apply the callback to a present value.
mapMaybeValue :: (a -> b) -> Maybe a -> Maybe b
mapMaybeValue _ Nothing = Nothing
mapMaybeValue f (Just x) = Just (f x)
