{-# LANGUAGE TemplateHaskell #-}

module Roles4 where

import Language.Haskell.TH

data Sticky a b = MkSticky (a b)

$( do roles <- reifyRoles (mkName "Sticky")
      runIO $ putStrLn (show roles)
      return [] )