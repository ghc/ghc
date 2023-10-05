{-# LANGUAGE ImplicitParams, PartialTypeSignatures #-}

module Main where

import GHC.Stack

f1 :: (?loc :: CallStack) => String
f1 = show $ map (srcLocStartLine . snd) $ getCallStack ?loc

f2 :: (?loc :: CallStack) => _
f2 = show $ map (srcLocStartLine . snd) $ getCallStack ?loc

f3 :: (?loc :: CallStack, _) => String
f3 = show $ map (srcLocStartLine . snd) $ getCallStack ?loc

main :: IO ()
main = do
  putStrLn f1
  putStrLn f2
  putStrLn f3
