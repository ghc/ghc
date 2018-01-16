{-# LANGUAGE TemplateHaskell #-}

-- Test that LINE pragmas influence type error messages correctly

module ShouldFail where

import Language.Haskell.TH

$( do p  <- pragLineD 42 "virtual file"
      ds <- [d| x = $(varE (mkName "doesntExist")) |]
      return (p:ds) )
