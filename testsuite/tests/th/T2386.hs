{-# LANGUAGE TemplateHaskell #-}

-- Test #2386

module T2386 where

import T2386_Lib

foo = $(makeOne)
