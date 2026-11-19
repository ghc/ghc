{-# LANGUAGE BangPatterns, NoExperimentalDependentTypes #-}
module SpineWorker
    ( Spine(..), firstMaybe, firstViaHelper, firstViaWrap, Marked(..), firstViaMark, secondOrFirst, erase, fill
    , boxedFirst, strictFirst, Secret, privateFirst, StrictResult(..), strictResult
    ) where

-- | A lazy outer cell and its remaining spine.
data Spine a = Empty | More a (Spine a)

-- | Return the first field without inspecting the tail.
firstMaybe :: Spine a -> Maybe a
firstMaybe Empty = Nothing
firstMaybe (More x _) = Just x

-- | Preserve a checked helper call in the result's lazy field.
firstViaHelper :: Spine a -> Maybe a
firstViaHelper Empty = Nothing
firstViaHelper (More x _) = Just (helper x)

{-# NOINLINE helper #-}
helper :: a -> a
helper x = x

-- | Construct both result constructors, one in a checked helper.
firstViaWrap :: Spine a -> Maybe (Maybe a)
firstViaWrap Empty = Nothing
firstViaWrap (More x _) = Just (wrap x)

{-# NOINLINE wrap #-}
wrap :: a -> Maybe a
wrap x = Just x

-- | A public result constructor distinct from the built-in types.
data Marked a = Unmarked | Marked a

-- | Construct a public user-defined result through a private helper.
firstViaMark :: Spine a -> Maybe (Marked a)
firstViaMark Empty = Nothing
firstViaMark (More x _) = Just (mark x)

{-# NOINLINE mark #-}
mark :: a -> Marked a
mark x = markResult x

{-# NOINLINE markResult #-}
markResult :: a -> Marked a
markResult x = markField x

{-# NOINLINE markField #-}
markField :: a -> Marked a
markField x = Marked x

-- | Return the second element of a nonempty spine, or its only element.
secondOrFirst :: Spine a -> Maybe a
secondOrFirst Empty = Nothing
secondOrFirst (More x xs) = Just (headOr xs x)

-- | Select a head without inspecting the fallback or the remaining tail.
{-# NOINLINE headOr #-}
headOr :: Spine a -> a -> a
headOr Empty fallback = fallback
headOr (More y _) _ = y

-- | Replace elements with True while keeping the tail lazy.
erase :: Spine a -> Spine Bool
erase Empty = Empty
erase (More _ xs) = More True (erase xs)

-- | Replace each element with the supplied value.
fill :: Spine a -> b -> Spine b
fill Empty _ = Empty
fill (More _ xs) tag = More tag (fill xs tag)

-- | Force the tail before returning the first field.
strictFirst :: Spine a -> Maybe a
strictFirst Empty = Nothing
strictFirst (More x !_) = Just x

-- | Keep the constructors private to this module.
data Secret a = NoSecret | Secret a

-- | Return the first field inside a private constructor.
privateFirst :: Spine a -> Secret a
privateFirst Empty = NoSecret
privateFirst (More x _) = Secret x

-- | Build two result constructors from a single field.
boxedFirst :: Spine a -> Maybe (Maybe a)
boxedFirst Empty = Nothing
boxedFirst (More x _) = Just (Just x)

-- | Force the field of a present result.
data StrictResult a = Absent | Present !a

-- | Use a result constructor that demands its field.
strictResult :: Spine a -> StrictResult a
strictResult Empty = Absent
strictResult (More x _) = Present x
