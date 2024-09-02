{-# LANGUAGE RecordWildCards #-}
module T25056 where

import T25056b

foo :: T -> ()
foo (T { unT = x }) = x
