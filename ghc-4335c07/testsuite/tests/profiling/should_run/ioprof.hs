import Control.Concurrent
import Control.Exception
import Control.Monad (ap, liftM)
import Control.Applicative

type S = String

newtype M s a = M { unM :: s -> (s,a) }

instance Monad (M s) where
  (M m) >>= k = M $ \s -> case m s of
                            (s',a) -> unM (k a) s'
  return a = M $ \s -> (s,a)

instance Functor (M s) where
    fmap = liftM

instance Applicative (M s) where
    pure  = return
    (<*>) = ap

errorM :: String -> M s a
errorM s = M $ \_ -> error s

runM :: M s a -> s -> a
runM (M m) s = case m s of (_,a) -> a

main = print (runM (bar ["a","b"]) "state")

bar :: [String] -> M s [String]
bar xs = mapM foo xs

foo :: String -> M s String
foo x = errorM x
