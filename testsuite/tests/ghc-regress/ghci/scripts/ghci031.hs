{-# LANGUAGE DatatypeContexts #-}
-- Trac #2138
-- If we :i D, we should see the Eq constraint

module Foo where

data Eq a => D a = C a

