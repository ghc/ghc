-- {-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# OPTIONS_GHC -O2 -fforce-recomp #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE MagicHash, UnboxedTuples #-}
module Lib where

ints :: [Int]
ints = [1,2,3]
{-# NOINLINE ints #-}

-- | Motivates "inlining" of demand transformers.
-- Here, we want to stop analysing `inlining` when we see the jump to `g`, to
-- first get an approximation of `g` to see roughly how it uses its argument.
-- Then we commence analysis of `inlining`.
--
-- If we fail to do that, we'll first analyse `inlining`, then analyse the
-- outer-most `g` with the correct incoming demand, then analyse all the
-- other `g`'s with a too optimistic body demand to their fixed-points, then
-- re-iterate `inlining` to see that the second inner-most `g` now has a used
-- body demand, too, etc., until we ultimately hit the iteration limit.
inlining :: (Int, Int) -> (Int, Int)
inlining y =
  let g xs n = case xs of [] -> n; _:xs' -> g xs' n in g ints $
  let g xs n = case xs of [] -> n; _:xs' -> g xs' n in g ints $
  let g xs n = case xs of [] -> n; _:xs' -> g xs' n in g ints $
  let g xs n = case xs of [] -> n; _:xs' -> g xs' n in g ints $
  let g xs n = case xs of [] -> n; _:xs' -> g xs' n in g ints $
  let g xs n = case xs of [] -> n; _:xs' -> g xs' n in g ints $
  let g xs n = case xs of [] -> n; _:xs' -> g xs' n in g ints $
  let g xs n = case xs of [] -> n; _:xs' -> g xs' n in g ints $
  let g xs n = case xs of [] -> n; _:xs' -> g xs' n in g ints $
  let g xs n = case xs of [] -> n; _:xs' -> g xs' n in g ints $
  y

-- | If we decide to give any of the binders of a fully-connected large rec
-- group a higher priority than any other binder of the same rec group, then
-- this function will hit the iteration limit.
--
-- What happens here is that whenever we progress in one of the `f*` bindings,
-- we'll have to iterate *all* `f*` bindings of higher priority, even if they
-- are unlikely to change. Now if, say, `fn` has priority n, then we'll iterate
-- `f9` at least 10 times:
--
--   * the first two iterations happen to make `f9` reach a fixed-point
--   * then we'll re-iterate `f8` until it reaches a fixed-point.
--     Whenever it changes, we'll re-iterate `f9`. (So at least once.)
--   * then we'll re-iterate `f8` until it reaches a fixed-point.
--     Whenever it changes, we'll re-iterate `f8` and `f9`. (So at least once.)
--   * ... and so on ...
--
-- So it is absolutely crucial that
--
--   * Either we descend into `f0`, ..., `f8` when finding the fixed-point for
--     `f9`, despite those having lower priority than `f9`
--   * Or we make sure that all bindings of the rec group have the same
--     priority and then analyse them round-robin.
--     Alternatively, descend into bindings with lower priority only if those
--     bindings are part of the same rec group.
--
largeRecGroup :: Int -> Int
largeRecGroup x =
  let
    f0 0 y = y :: Int
    f0 x _ = f0 (x-1) x + f1 (x-1) x+ f2 (x-1) x + f3 (x-1) x + f4 (x-1) x + f5 (x-1) x + f6 (x-1) x + f7 (x-1) x + f8 (x-1) x + f9 (x-1) x
    f1 1 y = y :: Int
    f1 x _ = f0 (x-1) x + f1 (x-1) x+ f2 (x-1) x + f3 (x-1) x + f4 (x-1) x + f5 (x-1) x + f6 (x-1) x + f7 (x-1) x + f8 (x-1) x + f9 (x-1) x
    f2 2 y = y :: Int
    f2 x _ = f0 (x-1) x + f1 (x-1) x+ f2 (x-1) x + f3 (x-1) x + f4 (x-1) x + f5 (x-1) x + f6 (x-1) x + f7 (x-1) x + f8 (x-1) x + f9 (x-1) x
    f3 3 y = y :: Int
    f3 x _ = f0 (x-1) x + f1 (x-1) x+ f2 (x-1) x + f3 (x-1) x + f4 (x-1) x + f5 (x-1) x + f6 (x-1) x + f7 (x-1) x + f8 (x-1) x + f9 (x-1) x
    f4 4 y = y :: Int
    f4 x _ = f0 (x-1) x + f1 (x-1) x+ f2 (x-1) x + f3 (x-1) x + f4 (x-1) x + f5 (x-1) x + f6 (x-1) x + f7 (x-1) x + f8 (x-1) x + f9 (x-1) x
    f5 5 y = y :: Int
    f5 x _ = f0 (x-1) x + f1 (x-1) x+ f2 (x-1) x + f3 (x-1) x + f4 (x-1) x + f5 (x-1) x + f6 (x-1) x + f7 (x-1) x + f8 (x-1) x + f9 (x-1) x
    f6 6 y = y :: Int
    f6 x _ = f0 (x-1) x + f1 (x-1) x+ f2 (x-1) x + f3 (x-1) x + f4 (x-1) x + f5 (x-1) x + f6 (x-1) x + f7 (x-1) x + f8 (x-1) x + f9 (x-1) x
    f7 7 y = y :: Int
    f7 x _ = f0 (x-1) x + f1 (x-1) x+ f2 (x-1) x + f3 (x-1) x + f4 (x-1) x + f5 (x-1) x + f6 (x-1) x + f7 (x-1) x + f8 (x-1) x + f9 (x-1) x
    f8 8 y = y :: Int
    f8 x _ = f0 (x-1) x + f1 (x-1) x+ f2 (x-1) x + f3 (x-1) x + f4 (x-1) x + f5 (x-1) x + f6 (x-1) x + f7 (x-1) x + f8 (x-1) x + f9 (x-1) x
    f9 9 y = y :: Int
    f9 x _ = f0 (x-1) x + f1 (x-1) x+ f2 (x-1) x + f3 (x-1) x + f4 (x-1) x + f5 (x-1) x + f6 (x-1) x + f7 (x-1) x + f8 (x-1) x + f9 (x-1) x
  in f0 x x

-- TODO: Find a test case that mimics a deep stream fusion pipeline with much nested recursion
-- utils/haddock/haddock-library/src/Documentation/Haddock/Parser.hs brought our
-- previous approach to its knees, although I tried descending into out-unstable nodes that
--   * either had a higher priority
--   * or that were part of the same rec group
--   * or lexically wrapped around the binding
-- all to no avail; we used 12 iterations for a binding there instead of 9 with
-- the naive "always descend into unstable nodes" tactic.
inoutRec :: Int -> Int
inoutRec x =
  let outer y =
        let inner 0 = 0
            inner z = inner (y+z) + outer (z+1)
        in inner (y+1)
  in outer x


data FingerTree a
    = EmptyT
    | Single a
    | Deep !(Digit a) (FingerTree (Node a)) !(Digit a)

data Digit a
    = One a
    | Two a a
    | Three a a a
    | Four a a a a

data Node a
    = Node2 a a
    | Node3 a a a

infixr 5 `consTree`
infixl 5 `snocTree`

consTree :: a -> FingerTree a -> FingerTree a
consTree a EmptyT       = Single a
consTree a (Single b)   = Deep (One a) EmptyT (One b)
consTree a (Deep (Four b c d e) m sf) = m `seq`
    Deep (Two a b) (Node3 c d e `consTree` m) sf
consTree a (Deep (Three b c d) m sf) =
    Deep (Four a b c d) m sf
consTree a (Deep (Two b c) m sf) =
    Deep (Three a b c) m sf
consTree a (Deep (One b) m sf) =
    Deep (Two a b) m sf

snocTree :: FingerTree a -> a -> FingerTree a
snocTree EmptyT a       =  Single a
snocTree (Single a) b   =  Deep (One a) EmptyT (One b)
-- See note on `seq` in `consTree`.
snocTree (Deep pr m (Four a b c d)) e = m `seq`
    Deep pr (m `snocTree` Node3 a b c) (Two d e)
snocTree (Deep pr m (Three a b c)) d =
    Deep pr m (Four a b c d)
snocTree (Deep pr m (Two a b)) c =
    Deep pr m (Three a b c)
snocTree (Deep pr m (One a)) b =
    Deep pr m (Two a b)

appendTree3 :: FingerTree (Node a) -> Node a -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree3 EmptyT !a !b !c xs =
    a `consTree` b `consTree` c `consTree` xs
appendTree3 xs !a !b !c EmptyT =
    xs `snocTree` a `snocTree` b `snocTree` c
appendTree3 (Single x) a b c xs =
    x `consTree` a `consTree` b `consTree` c `consTree` xs
appendTree3 xs a b c (Single x) =
    xs `snocTree` a `snocTree` b `snocTree` c `snocTree` x
appendTree3 (Deep pr1 m1 sf1) a b c (Deep pr2 m2 sf2) =
    Deep pr1 m sf2
  where !m = addDigits3 m1 sf1 a b c pr2 m2

addDigits3 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits3 m1 (One a) b c d (Three e f g) m2 =
    appendTree3 m1 (Node3 a b c) (Node2 d e) (Node2 f g) m2
addDigits3 m1 (One a) b c d (Four e f g h) m2 =
    appendTree3 m1 (Node3 a b c) (Node3 d e f) (Node2 g h) m2
addDigits3 m1 (Two a b) c d e (Two f g) m2 =
    appendTree3 m1 (Node3 a b c) (Node2 d e) (Node2 f g) m2
addDigits3 m1 (Two a b) c d e (Three f g h) m2 =
    appendTree3 m1 (Node3 a b c) (Node3 d e f) (Node2 g h) m2
addDigits3 m1 (Two a b) c d e (Four f g h i) m2 =
    appendTree3 m1 (Node3 a b c) (Node3 d e f) (Node3 g h i) m2
addDigits3 m1 (Three a b c) !d !e !f (One g) m2 =
    appendTree3 m1 (Node3 a b c) (Node2 d e) (Node2 f g) m2
addDigits3 m1 (Three a b c) d e f (Two g h) m2 =
    appendTree3 m1 (Node3 a b c) (Node3 d e f) (Node2 g h) m2
addDigits3 m1 (Three a b c) d e f (Three g h i) m2 =
    appendTree3 m1 (Node3 a b c) (Node3 d e f) (Node3 g h i) m2
addDigits3 m1 (Four a b c d) !e !f !g (One h) m2 =
    appendTree3 m1 (Node3 a b c) (Node3 d e f) (Node2 g h) m2
addDigits3 m1 (Four a b c d) e f g (Two h i) m2 =
    appendTree3 m1 (Node3 a b c) (Node3 d e f) (Node3 g h i) m2
addDigits3 _ _ _ _ _ _ _ = error "elided"
