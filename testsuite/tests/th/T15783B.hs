{-# LANGUAGE TemplateHaskell #-}
module T15783B(f) where

d = 0

f = [|| d ||]
