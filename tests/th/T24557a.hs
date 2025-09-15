{-# LANGUAGE TemplateHaskell #-}

module Ta where

import Language.Haskell.TH

$(invisP (varT (mkName "pat"))) = ()
