{-# LANGUAGE TemplateHaskell #-}

-- Trac #2632

module MkData where

import Language.Haskell.TH

op :: Num v => v -> v -> v
op a b = a + b

decl1 = [d| func = 0 `op` 3 |]

decl2 = [d| op x y = x
            func = 0 `op` 3 |]

