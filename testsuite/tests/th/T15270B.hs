{-# LANGUAGE TemplateHaskell #-}

module T10047B where

import Language.Haskell.TH

-- Passing datacon name to varE should fail.
x = $(varE 'Just)
