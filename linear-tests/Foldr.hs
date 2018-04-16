{-# LANGUAGE NoImplicitPrelude #-}
module FoldrExample where
{-
inplace/bin/ghc-stage1 -O2 -dcore-lint
-}

import GHC.Base
import Data.Maybe

qux :: [Maybe Char] -> String
qux str = foldr (maybe id (:)) "" str
