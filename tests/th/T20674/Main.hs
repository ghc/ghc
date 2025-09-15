{-# LANGUAGE TemplateHaskell #-}
module Main where

import P

main = $([| return () |])
