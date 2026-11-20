{-# LANGUAGE ExperimentalDependentTypes #-}
module NativeRetainedSourceOperand (select, ordinaryNot) where

import PreludeFilterSource (Negation)

data Mark (b :: Bool) = Mark Char

{-# NOINLINE lazyMark #-}
lazyMark :: foreach (b :: Bool) -> Mark b
lazyMark _ = Mark 'l'

{-# NOINLINE strictMark #-}
strictMark :: foreach (b :: Bool) -> Mark b
strictMark True = Mark 'r'
strictMark False = Mark 'r'

{-# NOINLINE selectMark #-}
selectMark :: Bool -> foreach (x :: Bool) -> Mark (Negation x)
selectMark branch x = (case branch of
    False -> lazyMark
    True -> strictMark) (not x)

select :: Bool -> Bool -> Char
select branch x = case selectMark branch x of Mark result -> result

ordinaryNot :: Bool -> Bool
ordinaryNot x = not x
