{-# LANGUAGE TemplateHaskell #-}
module T18330 where
import Language.Haskell.TH.Syntax

$(addDependentFile "T18330.extra" >> return [])
