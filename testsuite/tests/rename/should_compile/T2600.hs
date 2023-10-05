{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module T2600 where

-- ** See trac #10595 for why we're okay with this generating warnings! **

class T t where
    to :: [a] -> t a
    from :: t a -> [a]
    tmap :: (a -> a) -> t a -> t a

{-# RULES

"myrule" forall t a. forall f x.
     from (tmap f (to x :: t a)) = map f (from (to x :: t a))

  #-}
