{-# LANGUAGE ViewPatterns #-}

module T19843b where

import Prelude (map, even, undefined, Bool)

foo (Map even -> xs) = undefined
