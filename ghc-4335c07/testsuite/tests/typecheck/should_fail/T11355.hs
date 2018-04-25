{-# LANGUAGE TypeApplications, RankNTypes #-}

module T11355 where

foo = const @_ @((forall a. a) -> forall a. a) () (id @(forall a. a))
