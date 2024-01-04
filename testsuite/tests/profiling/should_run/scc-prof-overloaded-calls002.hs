-- Running this program should result in seven calls to overloaded functions
-- with increasing numbers of dictionary arguments.
--
-- With just -fprof-late-overloaded, no SCCs should be added, since none of the
-- overloaded functions are top level. With `-fprof-late-overloaded-calls`, all
-- seven calls should get *distinct* SCCs with separate source locations even
-- though the overloaded functions share an OccName (`f`).

module Main where

data X = X

instance Show     X where
instance Num      X where
instance Eq       X where
instance Enum     X where
instance Ord      X where
instance Real     X where
instance Integral X where

-- No overloaded call
{-# NOINLINE invoke0 #-}
invoke0 :: (forall a. a -> a -> String) -> X -> String
invoke0 f val = f val val

{-# NOINLINE invoke1 #-}
invoke1 :: (forall a. Show a => a -> a -> String) -> X -> String
invoke1 f val = f val val

{-# NOINLINE invoke2 #-}
invoke2 :: (forall a. (Show a, Num a) => a -> a -> String) -> X -> String
invoke2 f val = f val val

{-# NOINLINE invoke3 #-}
invoke3 :: (forall a. (Show a, Num a, Eq a) => a -> a -> String) -> X -> String
invoke3 f val = f val val

{-# NOINLINE invoke4 #-}
invoke4 :: (forall a. (Show a, Num a, Eq a, Enum a) => a -> a -> String) -> X -> String
invoke4 f val = f val val

{-# NOINLINE invoke5 #-}
invoke5 :: (forall a. (Show a, Num a, Eq a, Enum a, Ord a) => a -> a -> String) -> X -> String
invoke5 f val = f val val

{-# NOINLINE invoke6 #-}
invoke6 :: (forall a. (Show a, Num a, Eq a, Enum a, Ord a, Real a) => a -> a -> String) -> X -> String
invoke6 f val = f val val

{-# NOINLINE invoke7 #-}
invoke7 :: (forall a. (Show a, Num a, Eq a, Enum a, Ord a, Real a, Integral a) => a -> a -> String) -> X -> String
invoke7 f val = f val val

main :: IO ()
main = do
    putStrLn $ invoke0 (\_ _ -> s) X
    putStrLn $ invoke1 (\_ _ -> s) X
    putStrLn $ invoke2 (\_ _ -> s) X
    putStrLn $ invoke3 (\_ _ -> s) X
    putStrLn $ invoke4 (\_ _ -> s) X
    putStrLn $ invoke5 (\_ _ -> s) X
    putStrLn $ invoke6 (\_ _ -> s) X
    putStrLn $ invoke7 (\_ _ -> s) X
  where
    s = "wibbly"
