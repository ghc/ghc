{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StaticPointers #-}
module SP where

import GHC.StaticPtr

unit = $(deRefStaticPtr $ (static [| () |]))


