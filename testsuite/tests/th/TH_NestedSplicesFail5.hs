{-# LANGUAGE TemplateHaskell #-}
module TH_NestedSplicesFail5 where

g1 = [|  [|  'r'  |]  |]
