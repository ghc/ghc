{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Main where

import Debug.Trace

f0 a = "fun"
f0' ~a = "fun2"

f1 ~n =
  case n of
    a -> "case"
f1' ~n =
  case n of
    ~a -> "case2"

f2 = \a -> "lambda"
f2' = \ ~a -> "lambda2"

newtype Age = MkAge Int

f4, f4' :: Age -> String
f4 (MkAge a) = "newtype"
f4' ~(MkAge a) = "newtype2"

main :: IO ()
main = mapM_ (\(what,f) -> putStrLn (f (v what))) fs
  where fs =
          [("fun",f0 )
          ,("fun lazy",f0')
          ,("case",f1)
          ,("case lazy",f1')
          ,("lambda",f2)
          ,("lambda lazy",f2')
          ,("newtype",(\ ~i -> f4 (MkAge i)))
          ,("newtype lazy",(\ ~i -> f4' (MkAge i)))]
        v n = trace ("evaluated in " ++ n) 1
