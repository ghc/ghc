{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE TemplateHaskell #-}

module T7276 where

x = $( [d| y = 3 |] )
