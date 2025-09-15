{-# LANGUAGE TemplateHaskell #-}

module T5434 where

import T5434a

$(genShadow1)

v :: Bool
v = True

$(genShadow2)
