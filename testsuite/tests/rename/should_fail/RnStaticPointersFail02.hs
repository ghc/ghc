{-# LANGUAGE StaticPointers #-}

module RnStaticPointersFail02 where

f = static T

data T = TDataCons
