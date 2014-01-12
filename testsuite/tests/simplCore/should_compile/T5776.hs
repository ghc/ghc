module T5776 where

-- The point about this test is that we should get a rule like this:
-- "foo" [ALWAYS]
--    forall (@ a)
--           ($dEq :: GHC.Classes.Eq a)
--           ($dEq1 :: GHC.Classes.Eq a)
--           (x :: a)
--           (y :: a)
--           (z :: a).
--      T5776.f (GHC.Classes.== @ a $dEq1 x y)
--              (GHC.Classes.== @ a $dEq y z)
--      = GHC.Types.True
--
-- Note the *two* forall'd dEq parameters. This is important.
-- See Note [Simplifying RULE lhs constraints] in TcSimplify

{-# RULES "foo" forall x y z.
      f (x == y) (y == z) = True
 #-}

f :: Bool -> Bool -> Bool
{-# NOINLINE f #-}
f a b = False

blah :: Int -> Int -> Bool
blah x y = f (x==y) (x==y)
 
