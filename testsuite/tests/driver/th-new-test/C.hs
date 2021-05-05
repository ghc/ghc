{-# LANGUAGE TemplateHaskell #-}
module C where

import D

c = print $(d)
