{-# LANGUAGE TemplateHaskell #-}

module Tc where

import Language.Haskell.TH

g = do
  $(invisP (varT (mkName "pat"))) <- pure ()
  pure ()
