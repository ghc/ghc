{-# LANGUAGE TemplateHaskell #-}

module SUse where

import qualified Language.Haskell.TH.Syntax as TH
import SDef
import GHC.Exts

bar = $( inline aStrictFunction aValue )
