{-# Language TemplateHaskell #-}

module T12561 where

import T12561A

main = $$(t1) + $$(t2)
