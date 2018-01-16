{-# LANGUAGE TemplateHaskell #-}
module T7021 where

import T7021a

func :: a -> Int
func = $(test)
