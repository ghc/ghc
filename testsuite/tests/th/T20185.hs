{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Language.Haskell.TH
import T20185a

i :: Int
i = $(getFieldE [|y|] "bar")

j = $([| x.foo.bar |])

k :: X -> Int
k = $([| (.foo.bar) |])

main :: IO ()
main = do
  print i
  print j
  print (k x)
  y <- [| (.foo.bar) |]
  print y
  putStrLn . pprint =<< [| x.foo.bar |]
  putStrLn . pprint =<< [| (id x).foo.bar |]
  putStrLn . pprint =<< [| (id (id x).foo).bar |]
  putStrLn . pprint =<< [| (.foo.bar) |]
  putStrLn . pprint =<< [| (.foo.bar) x |]
