import Control.Concurrent
import Control.Monad hiding (empty)
import Data.Sequence (ViewL(..), empty, fromList, viewl, (<|), (|>), (><))
import System.Random

-- This test

threads = 4
samples = 5000

main = loopTest threads samples

loopTest t s = do
  isClean <- testRace t s
  when (not isClean) $ putStrLn "race condition!"

testRace t s = do
  ref <- liftM (take (t*s) . randoms) getStdGen
  iss <- threadRandoms t s
  return (isInterleavingOf (ref::[Int]) iss)

threadRandoms :: Random a => Int -> Int -> IO [[a]]
threadRandoms t s = do
  vs <- sequence $ replicate t $ do
    v <- newEmptyMVar
    forkIO (sequence (replicate s randomIO) >>= putMVar v)
    return v
  mapM takeMVar vs

isInterleavingOf xs yss = iio xs (viewl $ fromList yss) EmptyL where
  iio (x:xs) ((y:ys) :< yss) zss
    | x /= y = iio (x:xs) (viewl yss) (viewl (fromViewL zss |> (y:ys)))
    | x == y = iio xs (viewl ((ys <| yss) >< fromViewL zss)) EmptyL
  iio xs ([] :< yss) zss = iio xs (viewl yss) zss
  iio [] EmptyL EmptyL = True
  iio _ _ _ = False

fromViewL (EmptyL) = empty
fromViewL (x :< xs) = x <| xs

