{- 
    Copyright 2009 Mario Blazevic

    This file is part of the Streaming Component Combinators (SCC) project.

    The SCC project is free software: you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    SCC is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
    or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
    License for more details.

    You should have received a copy of the GNU General Public License
    along with SCC.  If not, see <http://www.gnu.org/licenses/>.
-}

-- | Module "Trampoline" defines the pipe computations and their basic building blocks.

{-# LANGUAGE ScopedTypeVariables, Rank2Types, MultiParamTypeClasses,
             TypeFamilies, KindSignatures, FlexibleContexts,
             FlexibleInstances, OverlappingInstances, UndecidableInstances
 #-}

{-   Somewhere we get:

  Wanted: AncestorFunctor (EitherFunctor a (TryYield a)) d
  This should not reduce because of overlapping instances

  If it (erroneously) does reduce, via dfun2 we get
  Wanted: Functor (EitherFunctor a (TryYield a)
          Functor d'
          Functor d
          d ~ EitherFunctor d' s
          AncestorFunctor (EitherFunctor a (TryYield a) d'


  And that gives an infinite loop in the type checker!
-}


module Main where

import Control.Monad (liftM, liftM2, when)
-- import Control.Monad.Identity

import Debug.Trace (trace)


-------------
class (Functor a, Functor d) => AncestorFunctor a d where
   liftFunctor :: a x -> d x

-- dfun 1
instance Functor a => AncestorFunctor a a where
   liftFunctor = trace "liftFunctor id" . id

-- dfun 2
instance ( Functor a
         , Functor d'
         , Functor d
         , d ~ EitherFunctor d' s
         , AncestorFunctor a d')
      => AncestorFunctor a d where
   liftFunctor = LeftF . (trace "liftFunctor other" . liftFunctor :: a x -> d' x)




-------------
newtype Identity a = Identity { runIdentity :: a }
instance Monad Identity where
    return a = Identity a
    m >>= k  = k (runIdentity m)

newtype Trampoline m s r = Trampoline {bounce :: m (TrampolineState m s r)}
data TrampolineState m s r = Done r | Suspend! (s (Trampoline m s r))

instance (Monad m, Functor s) => Monad (Trampoline m s) where
   return x = Trampoline (return (Done x))
   t >>= f = Trampoline (bounce t >>= apply f)
      where apply f (Done x) = bounce (f x)
            apply f (Suspend s) = return (Suspend (fmap (>>= f) s))

data Yield x y = Yield! x y
instance Functor (Yield x) where
   fmap f (Yield x y) = trace "fmap yield" $ Yield x (f y)

data Await x y = Await! (x -> y)
instance Functor (Await x) where
   fmap f (Await g) = trace "fmap await" $ Await (f . g)

data EitherFunctor l r x = LeftF (l x) | RightF (r x)
instance (Functor l, Functor r) => Functor (EitherFunctor l r) where
   fmap f v = trace "fmap Either" $ 
              case v of
                LeftF l  -> trace "fmap LeftF" $ LeftF (fmap f l)
                RightF r -> trace "fmap RightF" $ RightF (fmap f r)

type TryYield x = EitherFunctor (Yield x) (Await Bool)

suspend :: (Monad m, Functor s) => s (Trampoline m s x) -> Trampoline m s x
suspend s = Trampoline (return (Suspend s))

yield :: forall m x. Monad m => x -> Trampoline m (Yield x) ()
yield x = suspend (Yield x (return ()))

await :: forall m x. Monad m => Trampoline m (Await x) x
await = suspend (Await return)

tryYield :: forall m x. Monad m => x -> Trampoline m (TryYield x) Bool
tryYield x = suspend (LeftF (Yield x (suspend (RightF (Await return)))))

canYield :: forall m x. Monad m => Trampoline m (TryYield x) Bool
canYield = suspend (RightF (Await return))

liftBounce :: Monad m => m x -> Trampoline m s x
liftBounce = Trampoline . liftM Done

fromTrampoline :: Monad m => Trampoline m s x -> m x
fromTrampoline t = bounce t >>= \(Done x)-> return x

runTrampoline :: Monad m => Trampoline m Maybe x -> m x
runTrampoline = fromTrampoline

coupleNestedFinite :: (Functor s, Monad m) =>
                      Trampoline m (EitherFunctor s (TryYield a)) x
                   -> Trampoline m (EitherFunctor s (Await (Maybe a))) y -> Trampoline m s (x, y)
coupleNestedFinite t1 t2 =
   trace "bounce start" $
   liftBounce (liftM2 (,) (bounce t1) (bounce t2))
   >>= \(s1, s2)-> trace "bounce end" $
                   case (s1, s2)
                   of (Done x, Done y) -> return (x, y)
                      (Done x, Suspend (RightF (Await c2))) -> coupleNestedFinite (return x) (c2 Nothing)
                      (Suspend (RightF (LeftF (Yield _ c1))), Done y) -> coupleNestedFinite c1 (return y)
                      (Suspend (RightF (LeftF (Yield x c1))), Suspend (RightF (Await c2))) -> coupleNestedFinite c1 (c2 $ Just x)
                      (Suspend (RightF (RightF (Await c1))), Suspend s2@(RightF Await{})) -> coupleNestedFinite (c1 True) (suspend s2)
                      (Suspend (RightF (RightF (Await c1))), Done y) -> coupleNestedFinite (c1 False) (return y)
                      (Suspend (LeftF s), Done y) -> suspend (fmap (flip coupleNestedFinite (return y)) s)
                      (Done x, Suspend (LeftF s)) -> suspend (fmap (coupleNestedFinite (return x)) s)
                      (Suspend (LeftF s1), Suspend (LeftF s2)) -> suspend (fmap (coupleNestedFinite $ suspend $ LeftF s1) s2)
                      (Suspend (LeftF s1), Suspend (RightF s2)) -> suspend (fmap (flip coupleNestedFinite (suspend $ RightF s2)) s1)
                      (Suspend (RightF s1), Suspend (LeftF s2)) -> suspend (fmap (coupleNestedFinite (suspend $ RightF s1)) s2)

local :: forall m l r x. (Monad m, Functor r) => Trampoline m r x -> Trampoline m (EitherFunctor l r) x
local (Trampoline mr) = Trampoline (liftM inject mr)
   where inject :: TrampolineState m r x -> TrampolineState m (EitherFunctor l r) x
         inject (Done x) = Done x
         inject (Suspend r) = Suspend (RightF $ fmap local r)

out :: forall m l r x. (Monad m, Functor l) => Trampoline m l x -> Trampoline m (EitherFunctor l r) x
out (Trampoline ml) = Trampoline (liftM inject ml)
   where inject :: TrampolineState m l x -> TrampolineState m (EitherFunctor l r) x
         inject (Done x) = Done x
         inject (Suspend l) = Suspend (LeftF $ fmap out l)

liftOut :: forall m a d x. (Monad m, Functor a, AncestorFunctor a d) => Trampoline m a x -> Trampoline m d x
liftOut (Trampoline ma) = trace "liftOut" $ Trampoline (liftM inject ma)
   where inject :: TrampolineState m a x -> TrampolineState m d x
         inject (Done x) = Done x
         inject (Suspend a) = trace "inject suspend" $ Suspend (liftFunctor $ trace "calling fmap" $ 
                              fmap liftOut (trace "poking a" a))

data Sink (m :: * -> *) a x =
   Sink   {put :: forall d. (AncestorFunctor (EitherFunctor a (TryYield x)) d) => x -> Trampoline m d Bool,
           canPut :: forall d. (AncestorFunctor (EitherFunctor a (TryYield x)) d) => Trampoline m d Bool}
newtype Source (m :: * -> *) a x =
   Source {get :: forall d. (AncestorFunctor (EitherFunctor a (Await (Maybe x))) d) => Trampoline m d (Maybe x)}

pipe :: forall m a x r1 r2. (Monad m, Functor a) =>
        (Sink m a x -> Trampoline m (EitherFunctor a (TryYield x)) r1)
     -> (Source m a x -> Trampoline m (EitherFunctor a (Await (Maybe x))) r2) -> Trampoline m a (r1, r2)
pipe producer consumer = coupleNestedFinite (producer sink) (consumer source) where
   sink = Sink {put= liftOut . (local . tryYield :: x -> Trampoline m (EitherFunctor a (TryYield x)) Bool),
                canPut= liftOut (local canYield :: Trampoline m (EitherFunctor a (TryYield x)) Bool)} :: Sink m a x
   source = Source (liftOut (local await :: Trampoline m (EitherFunctor a (Await (Maybe x))) (Maybe x))) :: Source m a x

pipeProducer sink = do put sink 1
                       (c, d) <- pipe
                                    (\sink'-> do put sink' 2
                                                 put sink 3
                                                 put sink' 4
                                                 return 5)
                                    (\source'-> do Just n <- get source'
                                                   put sink n
                                                   put sink 6
                                                   return n)
                       put sink c
                       put sink d
                       return (c, d)

testPipe = print $
           runIdentity $
           runTrampoline $
           do (a, b) <- pipe
                           pipeProducer
                           (\source-> do Just n1 <- get source
                                         Just n2 <- get source
                                         Just n3 <- get source
                                         return (n1, n2, n3))
              return (a, b)

main = testPipe
