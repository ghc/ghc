{-# LANGUAGE BangPatterns #-}

module Main where

main = print (case Nothing of
                  !(~(Just x)) -> "ok"
                  Nothing   -> "bad")

