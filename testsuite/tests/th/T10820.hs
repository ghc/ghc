{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Language.Haskell.TH.Syntax
import GHC.LanguageExtensions

main = do
  print $(isExtEnabled Cpp                  >>= lift)
  print $(isExtEnabled LiberalTypeSynonyms  >>= lift)
  print $(isExtEnabled RankNTypes           >>= lift)
  print $(isExtEnabled TypeSynonymInstances >>= lift)
  print $(isExtEnabled MagicHash            >>= lift)
