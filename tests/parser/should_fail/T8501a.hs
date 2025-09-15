module Test where

foo :: IO ()
foo = do
  rec str <- return "foo"
  putStrLn str

-- Should fail
-- foo' :: IO ()
-- foo' = do
--   rec {str <- return "foo" ;
--   putStrLn str}
--   return ()

-- Should fail
-- foo'' :: IO ()
-- foo'' = do
--   rec putStrLn "test"
--   str <- return "foo"
--   putStrLn str
--   return ()

-- Should compile
-- foo'''' :: IO ()
-- foo'''' = do
--   rec <- return "foo"
--   putStrLn "test"
