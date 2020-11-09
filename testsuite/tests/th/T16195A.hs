{-# LANGUAGE TemplateHaskell #-}
module T16195A where

import Language.Haskell.TH

foo :: Code Q (IO ())
foo = [|| return () ||]

showC :: Code Q (() -> String)
showC = [|| show ||]

unitC :: Code Q ()
unitC = [|| () ||]
