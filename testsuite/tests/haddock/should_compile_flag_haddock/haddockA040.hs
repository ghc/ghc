{-# LANGUAGE TypeFamilies #-}

module CommentsDataInstanceDeriving where

-- | Comment on the U data family
data family U a

-- | Comment on the U () data instance
data instance U () = UUnit
  deriving ( Eq    -- ^ Comment on the derived  Eq   (U ())  instance
           , Ord   -- ^ Comment on the derived  Ord  (U ())  instance
           , Show  -- ^ Comment on the derived  Show (U ())  instance
           )
