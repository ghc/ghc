module LongDistanceDo where

data D = T1 | T2

foo1 :: IO D -> IO ()
foo1 d = do
  x@y <- d
  let z = case x of
         T1 -> ()
         T2 -> case y of { T2 -> () }
  return z

foo2 :: IO D -> IO ()
foo2 d = do
  x@y <- d
  let z = case y of
         T1 -> ()
         T2 -> case x of { T2 -> () }
  return z
