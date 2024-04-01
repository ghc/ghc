{-# LANGUAGE TemplateHaskell #-}

module Tb where

import Language.Haskell.TH

f (blah, $(invisP (varT (mkName "pat"))) ) = ()
