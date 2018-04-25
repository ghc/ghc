{-# LANGUAGE ImplicitParams, RankNTypes #-}
import GHC.Stack

foo :: (?callStack :: CallStack) => [Int]
foo = map (srcLocStartLine . snd) (getCallStack ?callStack)

bar1 :: [Int]
bar1 = foo

bar2 :: [Int]
bar2 = let ?callStack = freezeCallStack ?callStack in foo

main :: IO ()
main = do
  print bar1
  print bar2
  withFrozenCallStack (error "look ma, no stack!")
