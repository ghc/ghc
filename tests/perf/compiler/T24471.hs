{-# LANGUAGE TemplateHaskell #-}
module T24471 where

import T24471a

{-# OPAQUE foo #-}
foo :: (List_ Int a -> a) -> a
foo alg = $$(between [|| alg ||] 0 1000)
