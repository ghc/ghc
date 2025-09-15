module DoExpansion3 where


-- make sure all the (>>=) expansion works okay

getVal :: Int -> IO String
getVal _ = return "x"

gggg1, gggg2, gggg3, gggg4, gggg5 :: IO Int


gggg1 = do let x = 1
           let y = 2
           putStrLn x -- should error here
           return (x + 1)

gggg2 = do let x = 1
               y = getChar 2  -- should error here
               z = 3
           return x

gggg3 = do x <- getChar
           let y = 2
           z <- getChar
           return (x + y) -- should error here

gggg4 = do Just x <- getChar -- should error here
           return x

gggg5 = do
  let z :: Int = 3
  let a = 1
  putStrLn $ a + ""
