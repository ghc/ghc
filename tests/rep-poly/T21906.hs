

{-# LANGUAGE DataKinds, MagicHash, NoImplicitPrelude, UnboxedTuples #-}

module T21906 where

import GHC.Exts

-- These should be rejected because representation-polymorphism
-- in a negative position is not supported.

test1 :: forall {r} (a :: TYPE (BoxedRep Lifted)) (b :: TYPE r)
      .  a -> State# RealWorld -> ( State# RealWorld -> b ) -> b
test1 val s f = keepAlive# val s f

test2 :: forall {r} (a :: TYPE r) (b :: TYPE (BoxedRep Lifted))
      .  ( State# RealWorld -> (# State# RealWorld, a #) )
      -> ( b -> State# RealWorld -> (# State# RealWorld, a #) )
      -> State# RealWorld
      -> (# State# RealWorld, a #)
test2 action handle s = catch# action handle s

test3 :: forall {r} a (b :: TYPE r)
      .  PromptTag# a
      -> (((State# RealWorld -> (# State# RealWorld, b #))
           -> State# RealWorld -> (# State# RealWorld, a #))
          -> State# RealWorld -> (# State# RealWorld, a #))
      -> State# RealWorld
      -> (# State# RealWorld, b #)
test3 tag f s = control0# tag f s

test4 :: forall {r} (a :: TYPE r)
      . (State# RealWorld -> (# State# RealWorld, a #))
      -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
test4 f s = fork# f s
