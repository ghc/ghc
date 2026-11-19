{-# LANGUAGE BangPatterns, NoExperimentalDependentTypes #-}
module SpineWorker (Spine(..), firstMaybe, boxedFirst, strictFirst, Secret, privateFirst, StrictResult(..), strictResult) where

-- | A lazy outer cell and its remaining spine.
data Spine a = Empty | More a (Spine a)

-- | Return the first field without inspecting the tail.
firstMaybe :: Spine a -> Maybe a
firstMaybe Empty = Nothing
firstMaybe (More x _) = Just x

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
