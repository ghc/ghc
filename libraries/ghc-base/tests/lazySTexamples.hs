import Data.STRef.Lazy
import Control.Monad.ST.Lazy as L
import Control.Monad.ST.Strict as S
import qualified Data.STRef as S
import Data.Function (fix)
import System.IO (hPutStrLn, stderr)
import Debug.Trace (trace)

-- The following implements `fix` using lazy `ST`. It is based on code
-- by Oleg Kiselyov (source: http://okmij.org/ftp/Haskell/Fix.hs) which is
-- in the public domain according to the main page (http://okmij.org/ftp/).

fact :: (Int -> Int) -> Int -> Int
fact self 0 = 1
fact self n = n * self (pred n)

-- Test liftM style (Oleg's original style)
fix1 :: (a -> a) -> a
fix1 f = L.runST $ do
  wrap <- newSTRef (error "black hole")
  let aux = readSTRef wrap >>= (\x -> x >>= pure . f)
  writeSTRef wrap aux
  aux

-- Test fmap style
fix2 :: (a -> a) -> a
fix2 f = L.runST $ do
  wrap <- newSTRef (error "black hole")
  let aux = readSTRef wrap >>= \x -> f <$> x
  writeSTRef wrap aux
  aux

-- The following examples are by Albert Y. C. Lai, and included (under the
-- GHC license) with his permission:
-- https://mail.haskell.org/pipermail/haskell-cafe/2017-January/126182.html

example1 :: [Int]
example1 = L.runST go where
  go = do
    v <- strictToLazyST (S.newSTRef 0)
    fix (\loop -> do
            n <- strictToLazyST (do n <- S.readSTRef v
                                    S.writeSTRef v (n+1)
                                    return n
                                )
            ns <- loop
            return (n : ns))

example2 :: [Int]
example2 = L.runST main where
  main = do
    v <- strictToLazyST (S.newSTRef 0)
    sequence (repeat (strictToLazyST (do n <- S.readSTRef v
                                         S.writeSTRef v (n+1)
                                         return n
                                     )))

example3 :: L.ST s [Integer]
example3 = do
    r <- newSTRef 0
    let loop = do
            x <- readSTRef r
            writeSTRef r $ x + 1
            xs <- loop
            writeSTRef r $ x + 2
            return $ x : xs
    loop

example4 :: L.ST s [Integer]
example4 = do
    r <- newSTRef 0
    let loop = do
            x <- readSTRef r
            writeSTRef r $ x + 1
            xs <- loop
            error "this line is dead code"
            return $ x : xs
    loop

star n s = trace ("<" ++ s ++ show n ++ ">") (return ())

-- Albert called this "Sprinkle sprinkle little stars, how
-- I wonder when you are"
example5 :: L.ST s [Integer]
example5 = do
    star 0 "init begin"
    r <- newSTRef 0
    star 0 "init end"
    let loop n = do
            star n "A"
            x <- readSTRef r
            star n "B"
            writeSTRef r $ x + 1
            star n "C"
            xs <- loop (n+1)
            star n "D"
            writeSTRef r $ x + 2
            star n "E"
            return $ x : xs
    loop 0

main :: IO ()
main = do
  print $ fix1 fact 5
  print $ fix2 fact 6
  print $ take 5 example1
  print $ take 5 example2
  print $ take 10 (L.runST example3)
  print $ take 10 (L.runST example4)
  hPutStrLn stderr $ show (take 5 (L.runST example5))
