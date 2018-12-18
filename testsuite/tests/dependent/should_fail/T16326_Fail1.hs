{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module T16326_Fail1 where

id1 :: forall a -> a -> a
id1 _ x = x

type Foo = forall a -> a -> a
id2 :: Foo
id2 _ x = x
