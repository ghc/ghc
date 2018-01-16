{-# LANGUAGE TypeFamilies, RankNTypes #-}

module T10899 where

class C a where
  type F a
  type F a = forall m. m a
