{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Language.Haskell.TH

apat :: Q Pat
apat = [p| Just @[a] xs |]

main = do
  print ()
