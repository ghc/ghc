{-# LANGUAGE TemplateHaskell #-}
module SelfRecomp02 where

import Language.Haskell.TH.Syntax

main = $(addDependentFile "SelfRecomp02.hs" >> [| print () |])
