{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}

module ExplicitForAllRules2 where

{-# RULES "new4" forall a. forall (x :: b). id @a (wk x) = (wk x) #-}
{-# RULES "new5" forall a. forall (x :: a). id @a y = y #-}
{-# RULES "new6" forall a. forall (x :: a). id @c x = x #-}

{-# NOINLINE wk #-}
wk :: forall b a. b -> a
wk _ = error ""
