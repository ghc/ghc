{-# LANGUAGE OverloadedStrings #-}

module T27124 where

foo :: [String] -> Bool
foo "HI" = True
foo _ = False

main = pure ()
