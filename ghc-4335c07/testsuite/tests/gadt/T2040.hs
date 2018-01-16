{-# LANGUAGE GADTs, ScopedTypeVariables, FlexibleContexts,
             MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module T2040 where

data Teq a b where Teq :: Teq a a

class C a b where proof :: Teq a b

data S a = S a

data W b where
    -- This would make every version of GHC happy
    -- W :: (C a c , c ~ S b) => W a -> W c
    W :: C a (S b) => W a -> W (S b)

foo :: W (S ()) -> W (S ()) -> ()
foo (W (_ :: W a1)) (W (_ :: W a2)) =
  case proof :: Teq a1 (S ()) of
    Teq -> ()

foo2 :: W (S ()) -> W (S ()) -> ()
foo2 (W (_ :: W a1)) (W (_ :: W a2)) =
  case proof :: Teq a1 (S ()) of
    Teq -> case proof :: Teq a2 (S ()) of
             Teq ->  ()