{-# LANGUAGE TemplateHaskell #-}

module T10047A where

import Language.Haskell.TH

-- Passing var name to conE should fail.
x = $(conE 'id)
