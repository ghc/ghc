{-# LANGUAGE TemplateHaskell #-}
module TH_NestedSplicesFail3 where

f3 x = $$( [| 'x' |] )
