{-# LANGUAGE TemplateHaskell, GADTs #-}
module T15815B where

import T15815A

mkFoo [t| Int -> Int |]
