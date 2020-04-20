{-# LANGUAGE Arrows, LambdaCase #-}
module ParserArrowLambdaCase where

import Control.Arrow

foo :: () -> ()
foo = proc () -> (| id (\case
  () -> () >- returnA) |) ()
