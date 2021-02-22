{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}
module TemplateHaskellSplices where

import TemplateHaskellQuasiquotes

$(aDecl)

foo = id $(anExpression2)
