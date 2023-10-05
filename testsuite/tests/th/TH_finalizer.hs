{-# LANGUAGE TemplateHaskell #-}

module ShouldCompile where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

$( do
     addModFinalizer (do b <- getQ; reportWarning (show (b::Maybe Bool)))
     return [] )
$( putQ True >> return [] )
