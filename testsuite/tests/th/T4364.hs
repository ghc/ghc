{-# LANGUAGE TemplateHaskell #-}
module T4364 where

data Z

type N0 = $( [t| Z |] )
type N1 = $( [t| Z |] )
