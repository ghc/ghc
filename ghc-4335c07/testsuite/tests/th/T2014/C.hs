{-# LANGUAGE TemplateHaskell #-}
module C where

import B

foo :: a
foo = undefined
  where second = $( expQ )
