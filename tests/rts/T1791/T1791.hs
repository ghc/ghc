import Control.Exception
force :: [a] -> [a]
force [] = []
force x@(a:b) = x `seq` a : force b

{-# NOINLINE infiniteList #-}
infiniteList :: [Int]
infiniteList = [1..]


heapOverflow :: IO ()
heapOverflow = do
  evaluate $ length infiniteList -- Force the list
  evaluate infiniteList -- So that the list cannot be garbage collected.
  return ()

main :: IO ()
main = heapOverflow `catch` \x -> case x of
  HeapOverflow -> putStrLn "Heap overflow caught!"
  _ -> throwIO x
