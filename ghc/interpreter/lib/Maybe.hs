-----------------------------------------------------------------------------
-- Standard Library: Operations on the Maybe datatype
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------
module Maybe(
    isJust, fromJust, fromMaybe, listToMaybe, maybeToList,
    catMaybes, mapMaybe,

    -- ... and what the Prelude exports
    Maybe(Nothing, Just),
    maybe
    ) where

isJust		      :: Maybe a -> Bool
isJust (Just a)        = True
isJust Nothing         = False

fromJust              :: Maybe a -> a
fromJust (Just a)      = a
fromJust Nothing       = error "Maybe.fromJust: Nothing"

fromMaybe             :: a -> Maybe a -> a
fromMaybe d Nothing    = d
fromMaybe d (Just a)   = a

maybeToList           :: Maybe a -> [a]
maybeToList Nothing    = []
maybeToList (Just a)   = [a]

listToMaybe           :: [a] -> Maybe a
listToMaybe []         = Nothing
listToMaybe (a:as)     = Just a
 
catMaybes             :: [Maybe a] -> [a]
catMaybes ms           = [ m | Just m <- ms ]

mapMaybe              :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f             = catMaybes . map f

-----------------------------------------------------------------------------
