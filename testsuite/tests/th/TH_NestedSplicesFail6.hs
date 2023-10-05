{-# LANGUAGE TemplateHaskell #-}
module TH_NestedSplicesFail6 where

g2 = [|| [|| 'g' ||] ||]
