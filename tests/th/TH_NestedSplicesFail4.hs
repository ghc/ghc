{-# LANGUAGE TemplateHaskell #-}
module TH_NestedSplicesFail4 where

f4 x = $( [|| 'y' ||] )
