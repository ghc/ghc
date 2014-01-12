{-# LANGUAGE TemplateHaskell, Generics #-}
{-# OPTIONS_GHC -v0 #-}

-- We should only generate one set of generic to/from functions
-- for T, despite the multiple chunks caused by the TH splices
-- See Trac #3391

module T3391 where

data T = MkT

$(return [])

$(return [])
