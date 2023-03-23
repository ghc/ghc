import Control.Exception
import Control.Monad
import System.Mem.Weak
import System.Mem
import System.IO.Unsafe
import Control.Concurrent
import Data.IORef


data Dog = Dog { age :: !Int, dogTag :: DogTag}
data DogTag = DogTag {name :: String, weakPtr :: !(Weak Dog)}

mkDogTag :: Dog -> DogTag
mkDogTag d = unsafePerformIO $ do
  threadDelay 10
  wk <- mkWeakPtr d $ Nothing
  pure $ DogTag "k9" wk

mkDog :: Int -> IO Dog
mkDog age =
  let
    ret =
      Dog
        { age = age
        , dogTag = mkDogTag ret
        }
  in do
  performMajorGC
  evaluate $ dogTag ret
  pure $! ret

main = do
  dogs <- newIORef []
  forM [1..2000] $ \n -> forkIO $ do
    dog <- mkDog n
    -- print $ age dog
    modifyIORef' dogs (dog:)
  threadDelay 100000

