{-# LANGUAGE TemplateHaskell #-}

-- Test Trac #3467

module T3467 where

import Language.Haskell.TH
import Foreign

sizeq :: Name -> Q Exp
sizeq n = [| sizeOf (undefined :: $(conT n)) |]
