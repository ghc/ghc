{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Werror=redundant-constraints #-}

module EventThing where

class Monad m => Event m where
  thingsForEvent :: m [Int]

class Monad m => Thingy m where
  thingies :: m [Int]

-- Check that we don't get a redundant constraint warning for "Monad m".
-- See #19690.
instance (Monad m, Event m) => Thingy m where
  thingies = thingsForEvent
