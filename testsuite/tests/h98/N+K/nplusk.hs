import System

fib :: Integer -> Integer

fib 0 = 1
fib 1 = 1
fib (n+2) = fib (n+1) + fib n
--fib n = fib (n-1) + fib (n-2)

main = do args <- getArgs
          if null args then
            putStrLn ("fib 10 = " ++ show (fib 10))
           else
            mapM_ (\arg-> putStrLn ("fib "++arg++" = " ++ show (fib (read arg))))
                  args
