{-# LANGUAGE TemplateHaskell #-}

module THPutDocExternal where

import Language.Haskell.TH
import THPutDocExternalA

putDoc (DeclDoc 'f) "Hello world" >> pure []
