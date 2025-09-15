{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH

main = print $$( $$(unsafeCodeCoerce $ typedBracketE $ litE $ charL 'a' :: Code Q (Code Q Char)) )
