{-# LANGUAGE TemplateHaskell #-}
module T15471 where

import T15471A


qux = $$(test_foo)

bar y = $$(list_foo [|| y ||] )

main = print (qux 5) >> print (bar True)
