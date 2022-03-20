{-# Language LambdaCase, Arrows #-}

import Control.Arrow

main = return ()

baz :: ArrowChoice p => p (Maybe Int) String
baz = proc x ->
  (| id (\cases
     Just x | x > 100   -> returnA -< "big " ++ show x
     1 2    | otherwise -> returnA -< "small " ++ show x
     -> returnA -< "none")
  |) x

foo :: Arrow p => p (Maybe Int) String
foo = proc x ->
  (| id (\cases
     (Just x) | x > 100   -> returnA -< "big " ++ show x
              | otherwise -> returnA -< "small " ++ show x
     Nothing              -> returnA -< "none")
  |) x

bar :: ArrowChoice p => p (Maybe Int) String
bar = proc x ->
  (| id (\cases
     (Just x) | x > 100   -> returnA -< "big " ++ show x
              | otherwise -> returnA -< "small " ++ show x
     Nothing              -> returnA -< "none")
  |) (Just x)
