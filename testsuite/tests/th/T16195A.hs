{-# LANGUAGE TemplateHaskell #-}
module T16195A where

import Language.Haskell.TH

foo :: Q (TExp (IO ()))
foo = [|| return () ||]

showC :: Q (TExp (() -> String))
showC = [|| show ||]

unitC :: Q (TExp ())
unitC = [|| () ||]
