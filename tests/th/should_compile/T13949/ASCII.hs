{-# LANGUAGE TemplateHaskell       #-}

module ASCII () where

import Tree
import PatternGenerator

type EP g = Bool

templateFoo ''EP ['A'..'Z']
