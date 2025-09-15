{-# LANGUAGE TemplateHaskell #-}
module TH_NestedSplicesFail7 where

g3 = [|  [|| 'b' ||]  |]
