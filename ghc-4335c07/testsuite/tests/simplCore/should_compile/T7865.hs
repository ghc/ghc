module T7865 where

-- Our very expensive operation that we don't want to perform more than once
-- Don't inline it so we can see exactly where it's called in the core
{-# NOINLINE expensive #-}
expensive :: Int -> Int
expensive x = x * 100000

-- SpecConstr this function:
recursive :: [Int] -> (Int,Int) -> (Int,Int)
recursive list acc
 = case list of
   []     -> acc
   (x:xs) ->
     -- Our expensive tuple:
     -- The case is here mainly so that the expensive let isn't floated out before SpecConstr.
     let a'    = case xs of
                 [] -> acc
                 (_:_) -> (let b = expensive x in (b * 2), x)
         -- Use the expensive value once and recurse.
         -- We recurse with (_:_:_) so that a specialisation is made for that pattern,
         -- which simplifies the case xs above. This exposes the expensive let.
         (p,q) = case a' of (p',q') -> recursive (x:x:xs) (q',p')

         -- Use the expensive value again.
         -- Our problem is that this shows up as a separate let-binding for expensive, instead of reusing
         -- the already computed value from above.
     in  (p + fst a', q + snd a')

