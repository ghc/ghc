{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.String.Compat (
  module Base
, String
, lines
, words
, unlines
, unwords
) where

import Data.String as Base

#if !(MIN_VERSION_base(4,4,0))
import Prelude (String, lines, words, unlines, unwords)
#endif
