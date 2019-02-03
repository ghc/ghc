{-# LANGUAGE TemplateHaskell #-}
module T16195 where

import T16195A

main2 :: IO ()
main2 = return ()

main :: IO ()
main = $$foo

main3 :: IO ()
main3 = putStrLn ($$showC $$unitC)


