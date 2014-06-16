{-# LANGUAGE TemplateHaskell #-}

module T5665 where

import T5665a

data Record = Record { recordField :: Int }

$(doSomeTH "SomeType" ''Int)
