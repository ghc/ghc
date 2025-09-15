-- T15226
import Control.Exception (evaluate)

-- Just in case Prelude.repeat changes for some reason.
import Prelude hiding (repeat)
import Data.Coerce

-- We want to be sure that the compiler *doesn't* know that
-- all the elements of the list are in WHNF, because if it
-- does, GHC.Core.Opt.ConstantFold may erase the seq#'s altogether.
repeat :: a -> [a]
repeat a = res
  where res = a : res
{-# NOINLINE repeat #-}  -- Belt *and* suspenders

newtype Foo = Foo Int

silly :: [Int] -> IO ()
silly = foldr go (pure ())
  where
    go x r = do
      x' <- (coerce (evaluate :: Foo -> IO Foo) :: Int -> IO Int) x
      evaluate (x' + 3)  -- GHC should know that x' has been evaluated,
                         -- so this calculation will be erased entirely.
                         -- Otherwise, we'll create a thunk to pass to
                         -- evaluate.
      r

main :: IO ()
-- 10,000,000 repetitions take only a twentieth of a second,
-- but allocations go up dramatically if the result is not
-- known evaluated.
main = silly $ take 10000000 $ repeat 1
