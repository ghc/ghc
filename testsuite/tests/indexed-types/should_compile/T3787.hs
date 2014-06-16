{- 
    Copyright 2009 Mario Blazevic

    This file is part of the Streaming Component Combinators (SCC) project.

    The SCC project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
    License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
    version.

    SCC is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with SCC.  If not, see
    <http://www.gnu.org/licenses/>.
-}

-- | Module "Trampoline" defines the trampoline computations and their basic building blocks.

{-# LANGUAGE ScopedTypeVariables, RankNTypes, MultiParamTypeClasses, TypeFamilies, KindSignatures,
             FlexibleContexts, FlexibleInstances, OverlappingInstances, UndecidableInstances
 #-}

module T3787 where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (liftM, liftM2, when)
import Control.Monad.Identity
import Control.Monad.Trans (MonadTrans(..))

import Data.Foldable (toList)
import Data.Maybe (maybe)
import Data.Sequence (Seq, viewl)

par, pseq :: a -> b -> b
par = error "urk"
pseq = error "urk"

-- | Class of monads that can perform two computations in parallel.
class Monad m => ParallelizableMonad m where
   -- | Combine two computations into a single parallel computation. Default implementation of `parallelize` is
   -- @liftM2 (,)@
   parallelize :: m a -> m b -> m (a, b)
   parallelize = liftM2 (,)

-- | Any monad that allows the result value to be extracted, such as `Identity` or `Maybe` monad, can implement
-- `parallelize` by using `par`.
instance ParallelizableMonad Identity where
   parallelize ma mb = let a = runIdentity ma
                           b = runIdentity mb
                       in  a `par` (b `pseq` a `pseq` Identity (a, b))

instance ParallelizableMonad Maybe where
   parallelize ma mb = case ma `par` (mb `pseq` (ma, mb))
                       of (Just a, Just b) -> Just (a, b)
                          _ -> Nothing

-- | IO is parallelizable by `forkIO`.
instance ParallelizableMonad IO where
   parallelize ma mb = do va <- newEmptyMVar
                          vb <- newEmptyMVar
                          forkIO (ma >>= putMVar va)
                          forkIO (mb >>= putMVar vb)
                          a <- takeMVar va
                          b <- takeMVar vb
                          return (a, b)

-- | Suspending monadic computations.
newtype Trampoline s m r = Trampoline {
   -- | Run the next step of a `Trampoline` computation.
   bounce :: m (TrampolineState s m r)
   }

data TrampolineState s m r =
   -- | Trampoline computation is finished with final value /r/.
   Done r
   -- | Computation is suspended, its remainder is embedded in the functor /s/.
 | Suspend! (s (Trampoline s m r))

instance (Functor s, Monad m) => Monad (Trampoline s m) where
   return x = Trampoline (return (Done x))
   t >>= f = Trampoline (bounce t >>= apply f)
      where apply f (Done x) = bounce (f x)
            apply f (Suspend s) = return (Suspend (fmap (>>= f) s))

instance (Functor s, ParallelizableMonad m) => ParallelizableMonad (Trampoline s m) where
   parallelize t1 t2 = Trampoline $ liftM combine $ parallelize (bounce t1) (bounce t2) where
      combine (Done x, Done y) = Done (x, y)
      combine (Suspend s, Done y) = Suspend (fmap (liftM $ \x-> (x, y)) s)
      combine (Done x, Suspend s) = Suspend (fmap (liftM $ (,) x) s)
      combine (Suspend s1, Suspend s2) = Suspend (fmap (parallelize $ suspend s1) s2)

instance Functor s => MonadTrans (Trampoline s) where
   lift = Trampoline . liftM Done

data Yield x y = Yield! x y
instance Functor (Yield x) where
   fmap f (Yield x y) = Yield x (f y)

data Await x y = Await! (x -> y)
instance Functor (Await x) where
   fmap f (Await g) = Await (f . g)

data EitherFunctor l r x = LeftF (l x) | RightF (r x)
instance (Functor l, Functor r) => Functor (EitherFunctor l r) where
   fmap f (LeftF l) = LeftF (fmap f l)
   fmap f (RightF r) = RightF (fmap f r)

newtype NestedFunctor l r x = NestedFunctor (l (r x))
instance (Functor l, Functor r) => Functor (NestedFunctor l r) where
   fmap f (NestedFunctor lr) = NestedFunctor ((fmap . fmap) f lr)

data SomeFunctor l r x = LeftSome (l x) | RightSome (r x) | Both (NestedFunctor l r x)
instance (Functor l, Functor r) => Functor (SomeFunctor l r) where
   fmap f (LeftSome l) = LeftSome (fmap f l)
   fmap f (RightSome r) = RightSome (fmap f r)
   fmap f (Both lr) = Both (fmap f lr)

type TryYield x = EitherFunctor (Yield x) (Await Bool)

suspend :: (Monad m, Functor s) => s (Trampoline s m x) -> Trampoline s m x
suspend s = Trampoline (return (Suspend s))

yield :: forall m x. Monad m => x -> Trampoline (Yield x) m ()
yield x = suspend (Yield x (return ()))

await :: forall m x. Monad m => Trampoline (Await x) m x
await = suspend (Await return)

tryYield :: forall m x. Monad m => x -> Trampoline (TryYield x) m Bool
tryYield x = suspend (LeftF (Yield x (suspend (RightF (Await return)))))

canYield :: forall m x. Monad m => Trampoline (TryYield x) m Bool
canYield = suspend (RightF (Await return))

fromTrampoline :: Monad m => Trampoline s m x -> m x
fromTrampoline t = bounce t >>= \(Done x)-> return x

runTrampoline :: Monad m => Trampoline Identity m x -> m x
runTrampoline = fromTrampoline

pogoStick :: (Functor s, Monad m) => (s (Trampoline s m x) -> Trampoline s m x) -> Trampoline s m x -> m x
pogoStick reveal t = bounce t
                     >>= \s-> case s 
                              of Done result -> return result
                                 Suspend c -> pogoStick reveal (reveal c)

pogoStickNested :: (Functor s1, Functor s2, Monad m) => 
                   (s2 (Trampoline (EitherFunctor s1 s2) m x) -> Trampoline (EitherFunctor s1 s2) m x)
                   -> Trampoline (EitherFunctor s1 s2) m x -> Trampoline s1 m x
pogoStickNested reveal t = 
   Trampoline{bounce= bounce t
                      >>= \s-> case s
                               of Done result -> return (Done result)
                                  Suspend (LeftF s) -> return (Suspend (fmap (pogoStickNested reveal) s))
                                  Suspend (RightF c) -> bounce (pogoStickNested reveal (reveal c))
             }

nest :: (Functor a, Functor b) => a x -> b y -> NestedFunctor a b (x, y)
nest a b = NestedFunctor $ fmap (\x-> fmap ((,) x) b) a

-- couple :: (Monad m, Functor s1, Functor s2) => 
--           Trampoline s1 m x -> Trampoline s2 m y -> Trampoline (NestedFunctor s1 s2) m (x, y)
-- couple t1 t2 = Trampoline{bounce= do ts1 <- bounce t1
--                                      ts2 <- bounce t2
--                                      case (ts1, ts2) of (Done x, Done y) -> return $ Done (x, y)
--                                                         (Suspend s1, Suspend s2) -> return $ Suspend $ 
--                                                                                     fmap (uncurry couple) (nest s1 s2)
--                          }

coupleAlternating :: (Monad m, Functor s1, Functor s2) => 
                     Trampoline s1 m x -> Trampoline s2 m y -> Trampoline (SomeFunctor s1 s2) m (x, y)
coupleAlternating t1 t2 = 
   Trampoline{bounce= do ts1 <- bounce t1
                         ts2 <- bounce t2
                         case (ts1, ts2) of (Done x, Done y) -> return $ Done (x, y)
                                            (Suspend s1, Suspend s2) ->
                                               return $ Suspend $ fmap (uncurry coupleAlternating) (Both $ nest s1 s2)
                                            (Done x, Suspend s2) ->
                                               return $ Suspend $ fmap (coupleAlternating (return x)) (RightSome s2)
                                            (Suspend s1, Done y) ->
                                               return $ Suspend $ fmap (flip coupleAlternating (return y)) (LeftSome s1)
             }

coupleParallel :: (ParallelizableMonad m, Functor s1, Functor s2) => 
                  Trampoline s1 m x -> Trampoline s2 m y -> Trampoline (SomeFunctor s1 s2) m (x, y)
coupleParallel t1 t2 = 
   Trampoline{bounce= parallelize (bounce t1) (bounce t2)
                      >>= \pair-> case pair
                                  of (Done x, Done y) -> return $ Done (x, y)
                                     (Suspend s1, Suspend s2) ->
                                        return $ Suspend $ fmap (uncurry coupleParallel) (Both $ nest s1 s2)
                                     (Done x, Suspend s2) ->
                                        return $ Suspend $ fmap (coupleParallel (return x)) (RightSome s2)
                                     (Suspend s1, Done y) ->
                                        return $ Suspend $ fmap (flip coupleParallel (return y)) (LeftSome s1)
             }

coupleNested :: (Monad m, Functor s0, Functor s1, Functor s2) => 
                Trampoline (EitherFunctor s0 s1) m x -> Trampoline (EitherFunctor s0 s2) m y -> 
                Trampoline (EitherFunctor s0 (SomeFunctor s1 s2)) m (x, y)
coupleNested t1 t2 = 
   Trampoline{bounce= do ts1 <- bounce t1
                         ts2 <- bounce t2
                         case (ts1, ts2) of (Done x, Done y) -> return $ Done (x, y)
                                            (Suspend (RightF s), Done y) -> 
                                               return $ Suspend $ RightF $ fmap (flip coupleNested (return y)) (LeftSome s)
                                            (Done x, Suspend (RightF s)) -> 
                                               return $ Suspend $ RightF $ fmap (coupleNested (return x)) (RightSome s)
                                            (Suspend (RightF s1), Suspend (RightF s2)) ->
                                               return $ Suspend $ RightF $ fmap (uncurry coupleNested) (Both $ nest s1 s2)
                                            (Suspend (LeftF s), Done y) ->
                                               return $ Suspend $ LeftF $ fmap (flip coupleNested (return y)) s
                                            (Done x, Suspend (LeftF s)) ->
                                               return $ Suspend $ LeftF $ fmap (coupleNested (return x)) s
                                            (Suspend (LeftF s1), Suspend (LeftF s2)) -> 
                                               return $ Suspend $ LeftF $ fmap (coupleNested $ suspend $ LeftF s1) s2
             }

seesaw :: (Monad m, Functor s1, Functor s2) => 
           (forall x y s t. (s ~ SomeFunctor s1 s2, t ~ Trampoline s m (x, y)) => s t -> t)
           -> Trampoline s1 m x -> Trampoline s2 m y -> m (x, y)
seesaw resolve t1 t2 = pogoStick resolve (coupleAlternating t1 t2)

seesawParallel :: (ParallelizableMonad m, Functor s1, Functor s2) => 
                  (forall x y s t. (s ~ SomeFunctor s1 s2, t ~ Trampoline s m (x, y)) => s t -> t)
                  -> Trampoline s1 m x -> Trampoline s2 m y -> m (x, y)
seesawParallel resolve t1 t2 = pogoStick resolve (coupleParallel t1 t2)

resolveProducerConsumer :: forall a s s0 t t' m x. 
                           (Functor s0, Monad m, s ~ SomeFunctor (TryYield a) (Await (Maybe a)), 
                            t ~ Trampoline (EitherFunctor s0 s) m x) => 
                           s t -> t
-- Arg :: s t
-- (LeftSome (LeftF ...)) : SomeFunctor (EitherFunctor .. ..) (...) t
resolveProducerConsumer (LeftSome (LeftF (Yield _ c))) = c
resolveProducerConsumer (LeftSome (RightF (Await c))) = c False
resolveProducerConsumer (RightSome (Await c)) = c Nothing
resolveProducerConsumer (Both (NestedFunctor (LeftF (Yield x (Await c))))) = c (Just x)
resolveProducerConsumer (Both (NestedFunctor (RightF (Await c)))) = suspend (RightF $ RightSome $ c True)

couplePC :: ParallelizableMonad m => Trampoline (Yield a) m x -> Trampoline (Await (Maybe a)) m y -> m (x, y)
couplePC t1 t2 = parallelize (bounce t1) (bounce t2)
                 >>= \(s1, s2)-> case (s1, s2)
                                 of (Done x, Done y) -> return (x, y)
                                    (Suspend (Yield x c1), Suspend (Await c2)) -> couplePC c1 (c2 $ Just x)
                                    (Suspend (Yield _ c1), Done y) -> couplePC c1 (return y)
                                    (Done x, Suspend (Await c2)) -> couplePC (return x) (c2 Nothing)

coupleFinite :: ParallelizableMonad m => Trampoline (TryYield a) m x -> Trampoline (Await (Maybe a)) m y -> m (x, y)
coupleFinite t1 t2 =
   parallelize (bounce t1) (bounce t2)
   >>= \(s1, s2)-> case (s1, s2)
                   of (Done x, Done y) -> return (x, y)
                      (Done x, Suspend (Await c2)) -> coupleFinite (return x) (c2 Nothing)
                      (Suspend (LeftF (Yield x c1)), Suspend (Await c2)) -> coupleFinite c1 (c2 $ Just x)
                      (Suspend (LeftF (Yield _ c1)), Done y) -> coupleFinite c1 (return y)
                      (Suspend (RightF (Await c1)), Suspend s2@Await{}) -> coupleFinite (c1 True) (suspend s2)
                      (Suspend (RightF (Await c1)), Done y) -> coupleFinite (c1 False) (return y)

coupleFiniteSequential :: Monad m => Trampoline (TryYield a) m x -> Trampoline (Await (Maybe a)) m y -> m (x, y)
coupleFiniteSequential t1 t2 =
   bounce t1
   >>= \s1-> bounce t2
             >>= \s2-> case (s1, s2)
                       of (Done x, Done y) -> return (x, y)
                          (Done x, Suspend (Await c2)) -> coupleFiniteSequential (return x) (c2 Nothing)
                          (Suspend (LeftF (Yield x c1)), Suspend (Await c2)) -> coupleFiniteSequential c1 (c2 $ Just x)
                          (Suspend (LeftF (Yield _ c1)), Done y) -> coupleFiniteSequential c1 (return y)
                          (Suspend (RightF (Await c1)), Suspend s2@Await{}) -> coupleFiniteSequential (c1 True) (suspend s2)
                          (Suspend (RightF (Await c1)), Done y) -> coupleFiniteSequential (c1 False) (return y)

-- coupleNested :: (Functor s, Monad m) =>
--                 Trampoline (EitherFunctor s (Yield a)) m x
--              -> Trampoline (EitherFunctor s (Await (Maybe a))) m y -> Trampoline s m (x, y)
             
-- coupleNested t1 t2 =
--    lift (liftM2 (,) (bounce t1) (bounce t2))
--    >>= \(s1, s2)-> case (s1, s2)
--                    of (Done x, Done y) -> return (x, y)
--                       (Suspend (RightF (Yield _ c1)), Done y) -> coupleNested c1 (return y)
--                       (Done x, Suspend (RightF (Await c2))) -> coupleNested (return x) (c2 Nothing)
--                       (Suspend (RightF (Yield x c1)), Suspend (RightF (Await c2))) -> coupleNested c1 (c2 $ Just x)
--                       (Suspend (LeftF s), Done y) -> suspend (fmap (flip coupleNested (return y)) s)
--                       (Done x, Suspend (LeftF s)) -> suspend (fmap (coupleNested (return x)) s)
--                       (Suspend (LeftF s1), Suspend (LeftF s2)) -> suspend (fmap (coupleNested $ suspend $ LeftF s1) s2)

coupleNestedFinite :: (Functor s, ParallelizableMonad m) =>
                      Trampoline (SinkFunctor s a) m x -> Trampoline (SourceFunctor s a) m y -> Trampoline s m (x, y)
coupleNestedFinite t1 t2 = lift (parallelize (bounce t1) (bounce t2))
                           >>= stepCouple coupleNestedFinite

coupleNestedFiniteSequential :: (Functor s, Monad m) =>
                                Trampoline (SinkFunctor s a) m x
                             -> Trampoline (SourceFunctor s a) m y
                             -> Trampoline s m (x, y)
coupleNestedFiniteSequential producer consumer = 
   pogoStickNested resolveProducerConsumer (coupleNested producer consumer)
-- coupleNestedFiniteSequential t1 t2 = lift (liftM2 (,) (bounce t1) (bounce t2))
--                                      >>= stepCouple coupleNestedFiniteSequential

stepCouple :: (Functor s, Monad m) =>
              (Trampoline (EitherFunctor s (TryYield a)) m x
                  -> Trampoline (EitherFunctor s (Await (Maybe a))) m y
                  -> Trampoline s m (x, y))
              -> (TrampolineState (EitherFunctor s (TryYield a)) m x,
                  TrampolineState (EitherFunctor s (Await (Maybe a))) m y)
              -> Trampoline s m (x, y)
stepCouple f couple = case couple
                      of (Done x, Done y) -> return (x, y)
                         (Done x, Suspend (RightF (Await c2))) -> f (return x) (c2 Nothing)
                         (Suspend (RightF (LeftF (Yield _ c1))), Done y) -> f c1 (return y)
                         (Suspend (RightF (LeftF (Yield x c1))), Suspend (RightF (Await c2))) -> f c1 (c2 $ Just x)
                         (Suspend (RightF (RightF (Await c1))), Suspend s2@(RightF Await{})) -> f (c1 True) (suspend s2)
                         (Suspend (RightF (RightF (Await c1))), Done y) -> f (c1 False) (return y)
                         (Suspend (LeftF s), Done y) -> suspend (fmap (flip f (return y)) s)
                         (Done x, Suspend (LeftF s)) -> suspend (fmap (f (return x)) s)
                         (Suspend (LeftF s1), Suspend (LeftF s2)) -> suspend (fmap (f $ suspend $ LeftF s1) s2)
                         (Suspend (LeftF s1), Suspend (RightF s2)) -> suspend (fmap (flip f (suspend $ RightF s2)) s1)
                         (Suspend (RightF s1), Suspend (LeftF s2)) -> suspend (fmap (f (suspend $ RightF s1)) s2)

local :: forall m l r x. (Functor r, Monad m) => Trampoline r m x -> Trampoline (EitherFunctor l r) m x
local (Trampoline mr) = Trampoline (liftM inject mr)
   where inject :: TrampolineState r m x -> TrampolineState (EitherFunctor l r) m x
         inject (Done x) = Done x
         inject (Suspend r) = Suspend (RightF $ fmap local r)

out :: forall m l r x. (Functor l, Monad m) => Trampoline l m x -> Trampoline (EitherFunctor l r) m x
out (Trampoline ml) = Trampoline (liftM inject ml)
   where inject :: TrampolineState l m x -> TrampolineState (EitherFunctor l r) m x
         inject (Done x) = Done x
         inject (Suspend l) = Suspend (LeftF $ fmap out l)

-- | Class of functors that can be lifted.
class (Functor a, Functor d) => AncestorFunctor a d where
   -- | Convert the ancestor functor into its descendant. The descendant functor typically contains the ancestor.
   liftFunctor :: a x -> d x

instance Functor a => AncestorFunctor a a where
   liftFunctor = id
instance (Functor a, Functor d', Functor d, d ~ EitherFunctor d' s, AncestorFunctor a d') => AncestorFunctor a d where
   liftFunctor = LeftF . (liftFunctor :: a x -> d' x)

liftOut :: forall m a d x. (Monad m, Functor a, AncestorFunctor a d) => Trampoline a m x -> Trampoline d m x
liftOut (Trampoline ma) = Trampoline (liftM inject ma)
   where inject :: TrampolineState a m x -> TrampolineState d m x
         inject (Done x) = Done x
         inject (Suspend a) = Suspend (liftFunctor $ fmap liftOut a)

type SourceFunctor a x = EitherFunctor a (Await (Maybe x))
type SinkFunctor a x = EitherFunctor a (TryYield x)

-- | A 'Sink' can be used to yield values from any nested `Trampoline` computation whose functor provably descends from
-- the functor /a/. It's the write-only end of a 'Pipe' communication channel.
data Sink (m :: * -> *) a x =
   Sink
   {
   -- | Function 'put' tries to put a value into the given `Sink`. The intervening 'Trampoline' computations suspend up
   -- to the 'pipe' invocation that has created the argument sink. The result of 'put' indicates whether the operation
   -- succeded.
   put :: forall d. (AncestorFunctor a d) => x -> Trampoline d m Bool,
   -- | Function 'canPut' checks if the argument `Sink` accepts values, i.e., whether a 'put' operation would succeed on
   -- the sink.
   canPut :: forall d. (AncestorFunctor a d) => Trampoline d m Bool
   }

-- | A 'Source' can be used to read values into any nested `Trampoline` computation whose functor provably descends from
-- the functor /a/. It's the read-only end of a 'Pipe' communication channel.
newtype Source (m :: * -> *) a x =
   Source
   {
   -- | Function 'get' tries to get a value from the given 'Source' argument. The intervening 'Trampoline' computations
   -- suspend all the way to the 'pipe' function invocation that created the source. The function returns 'Nothing' if
   -- the argument source is empty.
   get :: forall d. (AncestorFunctor a d) => Trampoline d m (Maybe x)
   }

-- | Converts a 'Sink' on the ancestor functor /a/ into a sink on the descendant functor /d/.
liftSink :: forall m a d x. (Monad m, AncestorFunctor a d) => Sink m a x -> Sink m d x
liftSink s = Sink {put= liftOut . (put s :: x -> Trampoline d m Bool),
                   canPut= liftOut (canPut s :: Trampoline d m Bool)}

-- | Converts a 'Source' on the ancestor functor /a/ into a source on the descendant functor /d/.
liftSource :: forall m a d x. (Monad m, AncestorFunctor a d) => Source m a x -> Source m d x
liftSource s = Source {get= liftOut (get s :: Trampoline d m (Maybe x))}

-- | The 'pipe' function splits the computation into two concurrent parts, /producer/ and /consumer/. The /producer/ is
-- given a 'Sink' to put values into, and /consumer/ a 'Source' to get those values from. Once producer and consumer
-- both complete, 'pipe' returns their paired results.
pipe :: forall m a a1 a2 x r1 r2. (Monad m, Functor a, a1 ~ SinkFunctor a x, a2 ~ SourceFunctor a x) =>
        (Sink m a1 x -> Trampoline a1 m r1) -> (Source m a2 x -> Trampoline a2 m r2) -> Trampoline a m (r1, r2)
pipe producer consumer = coupleNestedFiniteSequential (producer sink) (consumer source) where
   sink = Sink {put= liftOut . (local . tryYield :: x -> Trampoline a1 m Bool),
                canPut= liftOut (local canYield :: Trampoline a1 m Bool)} :: Sink m a1 x
   source = Source (liftOut (local await :: Trampoline a2 m (Maybe x))) :: Source m a2 x

-- | The 'pipeP' function is equivalent to 'pipe', except the /producer/ and /consumer/ are run in parallel.
pipeP :: forall m a a1 a2 x r1 r2. (ParallelizableMonad m, Functor a, a1 ~ SinkFunctor a x, a2 ~ SourceFunctor a x) =>
         (Sink m a1 x -> Trampoline a1 m r1) -> (Source m a2 x -> Trampoline a2 m r2) -> Trampoline a m (r1, r2)
pipeP producer consumer = coupleNestedFinite (producer sink) (consumer source) where
   sink = Sink {put= liftOut . (local . tryYield :: x -> Trampoline a1 m Bool),
                canPut= liftOut (local canYield :: Trampoline a1 m Bool)} :: Sink m a1 x
   source = Source (liftOut (local await :: Trampoline a2 m (Maybe x))) :: Source m a2 x

-- | The 'pipePS' function acts either as 'pipeP' or as 'pipe', depending on the argument /parallel/.
pipePS :: forall m a a1 a2 x r1 r2. (ParallelizableMonad m, Functor a, a1 ~ SinkFunctor a x, a2 ~ SourceFunctor a x) =>
          Bool -> (Sink m a1 x -> Trampoline a1 m r1) -> (Source m a2 x -> Trampoline a2 m r2) ->
          Trampoline a m (r1, r2)
pipePS parallel = if parallel then pipeP else pipe

getSuccess :: forall m a d x . (Monad m, AncestorFunctor a d)
              => Source m a x -> (x -> Trampoline d m ()) {- ^ Success continuation -} -> Trampoline d m ()
getSuccess source succeed = get source >>= maybe (return ()) succeed

-- | Function 'get'' assumes that the argument source is not empty and returns the value the source yields. If the
-- source is empty, the function throws an error.
get' :: forall m a d x . (Monad m, AncestorFunctor a d) => Source m a x -> Trampoline d m x
get' source = get source >>= maybe (error "get' failed") return

-- | 'pour' copies all data from the /source/ argument into the /sink/ argument, as long as there is anything to copy
-- and the sink accepts it.
pour :: forall m a1 a2 d x . (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d)
        => Source m a1 x -> Sink m a2 x -> Trampoline d m ()
pour source sink = fill'
   where fill' = canPut sink >>= flip when (getSuccess source (\x-> put sink x >> fill'))

-- | 'pourMap' is like 'pour' that applies the function /f/ to each argument before passing it into the /sink/.
pourMap :: forall m a1 a2 d x y . (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d)
           => (x -> y) -> Source m a1 x -> Sink m a2 y -> Trampoline d m ()
pourMap f source sink = loop
   where loop = canPut sink >>= flip when (get source >>= maybe (return ()) (\x-> put sink (f x) >> loop))

-- | 'pourMapMaybe' is to 'pourMap' like 'Data.Maybe.mapMaybe' is to 'Data.List.Map'.
pourMapMaybe :: forall m a1 a2 d x y . (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d)
                => (x -> Maybe y) -> Source m a1 x -> Sink m a2 y -> Trampoline d m ()
pourMapMaybe f source sink = loop
   where loop = canPut sink >>= flip when (get source >>= maybe (return ()) (\x-> maybe (return False) (put sink) (f x) >> loop))

-- | 'tee' is similar to 'pour' except it distributes every input value from the /source/ arguments into both /sink1/
-- and /sink2/.
tee :: forall m a1 a2 a3 d x . (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d, AncestorFunctor a3 d)
       => Source m a1 x -> Sink m a2 x -> Sink m a3 x -> Trampoline d m ()
tee source sink1 sink2 = distribute
   where distribute = do c1 <- canPut sink1
                         c2 <- canPut sink2
                         when (c1 && c2)
                            (get source >>= maybe (return ()) (\x-> put sink1 x >> put sink2 x >> distribute))

-- | 'putList' puts entire list into its /sink/ argument, as long as the sink accepts it. The remainder that wasn't
-- accepted by the sink is the result value.
putList :: forall m a d x. (Monad m, AncestorFunctor a d) => [x] -> Sink m a x -> Trampoline d m [x]
putList [] sink = return []
putList l@(x:rest) sink = put sink x >>= cond (putList rest sink) (return l)

-- | 'getList' returns the list of all values generated by the source.
getList :: forall m a d x. (Monad m, AncestorFunctor a d) => Source m a x -> Trampoline d m [x]
getList source = getList' return
   where getList' f = get source >>= maybe (f []) (\x-> getList' (f . (x:)))

-- | 'consumeAndSuppress' consumes the entire source ignoring the values it generates.
consumeAndSuppress :: forall m a d x. (Monad m, AncestorFunctor a d) => Source m a x -> Trampoline d m ()
consumeAndSuppress source = get source
                            >>= maybe (return ()) (const (consumeAndSuppress source))

-- | A utility function wrapping if-then-else, useful for handling monadic truth values
cond :: a -> a -> Bool -> a
cond x y test = if test then x else y

-- | A utility function, useful for handling monadic list values where empty list means success
whenNull :: forall a m. Monad m => m [a] -> [a] -> m [a]
whenNull action list = if null list then action else return list

-- | Like 'putList', except it puts the contents of the given 'Data.Sequence.Seq' into the sink.
putQueue :: forall m a d x. (Monad m, AncestorFunctor a d) => Seq x -> Sink m a x -> Trampoline d m [x]
putQueue q sink = putList (toList (viewl q)) sink
