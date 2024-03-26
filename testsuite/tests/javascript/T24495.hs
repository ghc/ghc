{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -O1 #-}
-- -O1 required to make "rest" thunk SingleEntry

module Main where

import GHC.CString
import GHC.JS.Prim (JSVal, toJSString)

foo :: Double -> IO ()
foo x = debugString (toJSString ("2 " ++ s))
  where
  x' = if x == 0 then "b" else "c"
  y' = if x == 0 then "b" else "c"
  s = "a" ++ x' ++ " " ++ y' ++ "d"

main :: IO ()
main = foo 0


foreign import javascript "((s) => { console.log(s); })"
  debugString :: JSVal -> IO ()
