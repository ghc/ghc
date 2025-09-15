{-# OPTIONS -fglasgow-exts -O -ddump-stranal #-}

module Foo(foo) where

foo :: Int -> Int
foo n = baz (n+1) (bar1 n)

{-# NOINLINE bar1 #-}
bar1 n = 1 + bar n

bar :: Int -> Int
{-# NOINLINE bar #-}
{-# RULES
"bar/foo" forall n. bar (foo n) = n
   #-}
bar n = n-1

baz :: Int -> Int -> Int
{-# INLINE [0] baz #-}
baz m n = m


{- Ronam writes (Feb08)

    Note that bar becomes dead as soon as baz gets inlined. But strangely,
    the simplifier only deletes it after full laziness and CSE. That is, it
    is not deleted in the phase in which baz gets inlined. In fact, it is
    still there after w/w and the subsequent simplifier run. It gets deleted
    immediately if I comment out the rule.

    I stumbled over this when I removed one simplifier run after SpecConstr
    (at the moment, it runs twice at the end but I don't think that should
    be necessary). With this change, the original version of a specialised
    loop (the one with the rules) is not longer deleted even if it isn't
    used any more. I'll reenable the second simplifier run for now but
    should this really be necessary?

No, it should not be necessary.  A refactoring in OccurAnal makes
this work right. Look at the simplifier output just before strictness
analysis; there should be a binding for 'foo', but for nothing else.

-}
