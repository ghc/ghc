{-# LANGUAGE TemplateHaskell #-}
module TemplateHaskellSplices where

import TemplateHaskellQuasiquotes

$(aDecl)

foo = id $(anExpression2)
