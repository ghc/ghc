{-# LANGUAGE TemplateHaskell #-}

module T15502 where

import Language.Haskell.TH.Syntax  (Lift(lift))

main = print ( $( lift (toInteger (maxBound :: Int) + 1) )
             , $( lift (minBound :: Int) )
             )
