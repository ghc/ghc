{-# LANGUAGE TemplateHaskellQuotes #-}
module TH_StringLift where

import Language.Haskell.TH.Syntax

foo :: Quote m => String -> m (TExp String)
foo x = [|| x ||]

foo2 :: Quote m => String -> m Exp
foo2 x = [| x |]
