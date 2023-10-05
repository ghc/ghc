{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
module T20189 where

y :: (t ~ (forall x . Show x => x -> IO ())) => t
y = _

