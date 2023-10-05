module T22404 where

{-# NOINLINE foo #-}
foo :: [a] -> (a,a)
foo [x,y]  = (x,y)
foo (x:xs) = foo xs

data T = A | B | C | D

-- The point of this test is that 'v' ought
-- not to be a thunk in the optimised program
-- It is used only once in each branch.  But we
-- need a clever occurrence analyser to spot it;
-- see Note [Occurrence analysis for join points]
--     in GHC.Core.Opt.OccurAnoa

f x xs = let v = foo xs in

         let {-# NOINLINE j #-}
             j True  = case v of (a,b) -> a
             j False = case v of (a,b) -> b
         in

         case x of
            A -> j True
            B -> j False
            C -> case v of (a,b) -> b
            D -> x
