{-# LANGUAGE TemplateHaskell #-}
module A where

import B

main = $(b)
