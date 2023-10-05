
{-# LANGUAGE TemplateHaskell #-}

module Foo where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.IO

$( do u1 <- runQ (dataToExpQ (const Nothing) ())
      u2 <- runQ [| () |]
      runIO $ print (u1 == u2)
      runIO $ hFlush stdout
      return []
 )
