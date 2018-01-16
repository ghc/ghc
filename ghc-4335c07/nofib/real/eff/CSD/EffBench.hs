{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables,
  MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

{-# OPTIONS -fexpose-all-unfoldings #-}



module EffBench where

import qualified Control.Monad.State.Strict as S


times :: Monad m => Int -> m a -> m ()
times n ma = go n where
  go 0 = pure ()
  go n = ma >> go (n - 1)
{-# inline times #-}


-- Classy chruch state
--------------------------------------------------------------------------------
--
-- This optimises well as it's the role of the specialiser to specialise
-- each `pure`, `get`, `put`.

class PureD r a where cpure :: a -> r
class GetD  s r where cget  :: (s -> r) -> r
class PutD  s r where cput  :: s -> r -> r

csmodify :: (GetD s r, PutD s r, PureD r ()) => (s -> s) -> r
csmodify f = cget $ \s -> let !s' = f s in cput s' $ cpure ()

cstimes :: forall r. (GetD Int r, PutD Int r, PureD r ()) => Int -> r
cstimes 0 = cpure ()
cstimes n = (cget :: (Int -> r) -> r) $ \s -> let !s' = s + 1 in (cput :: (Int -> r -> r)) s' $ cstimes (n - 1)

instance PureD (s -> (a, s)) a where
  cpure a s = (a, s)
  {-# inline cpure #-}
instance GetD  s (s -> (a, s)) where
  cget got s = got s s
  {-# inline cget #-}
instance PutD s (s -> (a, s)) where
  cput s' r _ = r s'
  {-# inline cput #-}


