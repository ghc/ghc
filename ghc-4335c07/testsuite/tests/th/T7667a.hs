{-# LANGUAGE TemplateHaskell #-}

module T7667a where

import Language.Haskell.TH

   -- to be correct, this should be ConE, not VarE!
false = $( return $ VarE (mkName "False") )