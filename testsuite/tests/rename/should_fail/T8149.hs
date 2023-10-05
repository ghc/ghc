{-# OPTIONS_GHC -Wall #-}

module T8149 where

import Control.Monad.Trans.Writer (WriterT, runWriterT)

foo :: Bool
foo = runWriterT `seq` True

