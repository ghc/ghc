{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -flate-specialise -O2 #-}

module Main (main) where

import qualified Control.Monad.State.Strict as S
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Kind
import           Data.Monoid
import           Data.Tuple

main :: IO ()
main = print $ badCore 100

badCore :: Int -> Int
badCore n  = getSum $ fst $ run  $ runState mempty $ for_ [0..n] $ \i ->   modify (<> Sum i)

data Union (r :: [Type -> Type]) a where
  Union :: e a -> Union '[e] a

decomp :: Union (e ': r) a -> e a
decomp (Union a) = a
{-# INLINE decomp #-}

absurdU :: Union '[] a -> b
absurdU = absurdU

newtype Semantic r a = Semantic
  { runSemantic
        :: forall m
         . Monad m
        => (forall x. Union r x -> m x)
        -> m a
  }

instance Functor (Semantic f) where
  fmap f (Semantic m) = Semantic $ \k -> fmap f $ m k
  {-# INLINE fmap #-}

instance Applicative (Semantic f) where
  pure a = Semantic (\x -> const (pure a) x)
  {-# INLINE pure #-}
  Semantic f <*> Semantic a = Semantic $ \k -> f k <*> a k
  {-# INLINE (<*>) #-}

instance Monad (Semantic f) where
  return = pure
  {-# INLINE return #-}
  Semantic ma >>= f = Semantic $ \k -> do
    z <- ma k
    runSemantic (f z) k
  {-# INLINE (>>=) #-}

data State s a
  = Get (s -> a)
  | Put s a
  deriving Functor

get :: Semantic '[State s] s
get = Semantic $ \k -> k $ Union $ Get id
{-# INLINE get #-}

put :: s -> Semantic '[State s] ()
put !s = Semantic $ \k -> k $ Union $! Put s ()
{-# INLINE put #-}

modify :: (s -> s) -> Semantic '[State s] ()
modify f = do
  !s <- get
  put $! f s
{-# INLINE modify #-}

runState :: s -> Semantic (State s ': r) a -> Semantic r (s, a)
runState = interpretInStateT $ \case
  Get k   -> fmap k S.get
  Put s k -> S.put s >> pure k
{-# INLINE[3] runState #-}

run :: Semantic '[] a -> a
run (Semantic m) = runIdentity $ m absurdU
{-# INLINE run #-}

interpretInStateT
    :: (forall x. e x -> S.StateT s (Semantic r) x)
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
interpretInStateT f s (Semantic m) = Semantic $ \k ->
  fmap swap $ flip S.runStateT s $ m $ \u ->
    S.mapStateT (\z -> runSemantic z k) $ f $ decomp u
{-# INLINE interpretInStateT #-}

