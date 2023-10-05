{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module T8428 where

import Control.Monad.ST

data IdentityT m a = IdentityT { runIdentityT :: m a }

runIdST :: IdentityT (forall s. ST s) a -> a
runIdST = runST . runIdentityT
-- Test formatting in the error message.
-- In fact this should be rejected as a kind error (#8388)
