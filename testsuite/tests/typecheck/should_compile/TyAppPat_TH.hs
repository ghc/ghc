{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

apat :: Q Pat
apat = [p| Just @[a] xs |]

main = do
  print ()
