module T5776 where

-- The point about this test is that we should get a rule like this:
-- "foo" [ALWAYS]
--    forall (@ a)
--           ($dEq  :: Eq a)
--           ($dEq1 :: Eq a)
--           (x :: a)
--           (y :: a)
--           (z :: a).
--      T5776.f (g @ a $dEq1 x y)
--              (g @ a $dEq  y z)
--      = GHC.Types.True
--
-- Note the *two* forall'd dEq parameters. This is important.
-- See Note [Simplifying RULE lhs constraints] in GHC.Tc.Solver

{-# RULES "foo" forall x y z.
      f (g x y) (g y z) = True
 #-}

g :: Eq a => a -> a -> Bool
{-# NOINLINE g #-}
g = (==)

f :: Bool -> Bool -> Bool
{-# NOINLINE f #-}
f a b = False

blah :: Int -> Int -> Bool
blah x y = f (g x y) (g x y)
 
