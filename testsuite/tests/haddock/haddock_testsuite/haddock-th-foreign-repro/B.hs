{-# LANGUAGE TemplateHaskell #-}
module B where

import A

$([d| bar = $(return foo) |])
