module Main where

import GHC.Stack.CloneStack (StackEntry(..), cloneMyStack, decode)

userFunction :: IO [StackEntry]
userFunction = do
  putStr ""
  stk <- cloneMyStack
  putStr ""
  es <- decode stk
  putStr ""
  return es

main :: IO ()
main = do
  entries <- userFunction
  let ours = filter ((== "Main") . moduleName) entries
  mapM_ (\e -> putStrLn (moduleName e ++ "\t" ++ functionName e)) ours
