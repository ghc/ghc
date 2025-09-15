{-# LANGUAGE TemplateHaskell #-}
module TH_NestedSplicesFail2 where

f2 y = [|| $y ||]
