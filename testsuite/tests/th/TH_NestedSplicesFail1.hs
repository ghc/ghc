{-# LANGUAGE TemplateHaskell #-}
module TH_NestedSplicesFail1 where

f1 x = [| $$x |]
