{-# LANGUAGE TemplateHaskell #-}
module A where

import B

a = print $(b)
