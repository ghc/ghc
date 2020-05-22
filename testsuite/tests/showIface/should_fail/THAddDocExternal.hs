{-# LANGUAGE TemplateHaskell #-}

module THAddDocExternal where

import Language.Haskell.TH
import THAddDocExternalA

addDoc (DeclDoc 'f) "Hello world" >> pure []
