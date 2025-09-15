{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes, ImpredicativeTypes #-}

module ICFP20 where

import Control.Monad.ST( ST, runST )

type SId = forall a. a->a

choose :: a -> a -> a
choose x y = x

single :: a -> [a]
single x = [x]

auto'2 :: SId -> b -> b
auto'2 x = id @SId x

-- Fails, and should fail
auto'1 :: SId -> b -> b
auto'1 = id

-- Fails, and should do so
a6 = id auto'2

-- Fails, and should do so
a8 = choose id auto'2

-- Fails, and should do so
st2 :: forall a. (forall s. ST s a) -> a
st2 x = id runST x
