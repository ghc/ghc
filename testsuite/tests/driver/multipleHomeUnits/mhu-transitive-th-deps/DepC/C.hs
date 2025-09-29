{-# LANGUAGE TemplateHaskell #-}

module C where

import Language.Haskell.TH.Syntax (lift)
import A (a)

c :: Int
c = $(lift a)

