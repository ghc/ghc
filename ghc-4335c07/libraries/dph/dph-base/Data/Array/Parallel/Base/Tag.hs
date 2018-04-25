
-- | Data constructor tags.
module Data.Array.Parallel.Base.Tag 
        ( Tag
        , tagToInt, intToTag
        , fromBool, toBool)
where


-- | Given a value of an algebraic type, the tag tells us what
--   data constructor was used to create it.
type Tag = Int


-- | Convert a `Tag` to an `Int`. This is identity at the value level.
tagToInt :: Tag -> Int
tagToInt = id
{-# INLINE tagToInt #-}


-- | Convert an `Int` to a `Tag`. This is identity at the value level.
intToTag :: Int -> Tag
intToTag = id
{-# INLINE intToTag #-}


-- | Get the `Tag` of a `Bool` value. `False` is 0, `True` is 1.
fromBool :: Bool -> Tag
fromBool False = 0
fromBool True  = 1
{-# INLINE fromBool #-}


-- | Convert a `Tag` to a `Bool` value.
toBool :: Tag -> Bool
toBool n | n == 0    = False
         | otherwise = True
{-# INLINE toBool #-}



