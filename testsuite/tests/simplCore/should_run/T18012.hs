module Main (main) where

{- This program is designed to check that case-of-known-constructor
fires even if an application of a DataCon wrapper is floated out:

  * The early FloatOut pass will float `D False` out of `g`, since
    it’s a constant, non-trivial expression.

  * But since `D` is strict, the floated-out expression will actually
    be `$WD False`.

  * In simplifier phase 2, `f` will be inlined into `g`, leading to a
    case expression that scrutinizes the floated-out binding.

  * If case-of-known-constructor fires, we’ll end up with `notRule
    False`, the RULE will fire, and we get True.

  * If it doesn’t fire at phase 2, it will fire later at phase 0 when
    we inline the DataCon wrapper. But now the RULE is inactive, so
    we’ll end up with False instead.

We want case-of-known-constructor to fire early, so we want the output
to be True. See #18012 for more details. -}

main :: IO ()
main = print (g ())

data T = D !Bool

notRule :: Bool -> Bool
notRule x = x
{-# INLINE [0] notRule #-}
{-# RULES "notRule/False" [~0] notRule False = True #-}

f :: () -> T -> Bool
f () (D a) = notRule a
{-# INLINE [100] f #-} -- so it isn’t inlined before FloatOut

g :: () -> Bool
g x = f x (D False)
{-# NOINLINE g #-}
