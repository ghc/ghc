
import Control.Exception as E

import Data.Int

main :: IO ()
main = do putStrLn "Int8"
          mapM_ p =<< (f :: IO [Either Int8 String])
          putStrLn "Int16"
          mapM_ p =<< (f :: IO [Either Int16 String])
          putStrLn "Int32"
          mapM_ p =<< (f :: IO [Either Int32 String])
          putStrLn "Int64"
          mapM_ p =<< (f :: IO [Either Int64 String])
          putStrLn "Int"
          mapM_ p =<< (f :: IO [Either Int String])
    where p (Left x) = print x
          p (Right e) = putStrLn e

f :: (Integral a, Bounded a) => IO [Either a String]
f = sequence [ g (minBound `div` (-1)),
               g (minBound `mod` (-1)),
               g (case minBound `divMod` (-1) of (x, _) -> x),
               g (case minBound `divMod` (-1) of (_, x) -> x),
               g (minBound `quot` (-1)),
               g (minBound `rem` (-1)),
               g (case minBound `quotRem` (-1) of (x, _) -> x),
               g (case minBound `quotRem` (-1) of (_, x) -> x) ]
    where g x = do x' <- evaluate x
                   return (Left x')
                `E.catch`
                   \e -> return (Right (show (e :: SomeException)))

