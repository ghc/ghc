{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall #-}

module T10931 ( BugC(..) ) where

data IdT f a = IdC (f a)

class ( m ~ Outer m (Inner m) ) => BugC (m :: * -> *) where
    type Inner m :: * -> *
    type Outer m :: (* -> *) -> * -> *

    bug :: ( forall n. ( n ~ Outer n (Inner n)
                       , Outer n ~ Outer m
                       )
            => Inner n a)
        -> m a

instance BugC (IdT m) where
    type Inner (IdT m) = m
    type Outer (IdT m) = IdT

    bug f = IdC f
