{-# LANGUAGE Strict #-}

module Main where

main = do
  ~a <- return (error "don't error here!")
  b <- return (error "error here!") -- this binding should be strict
  print "should never reach here"
