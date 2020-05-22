{-# LANGUAGE TemplateHaskell #-}

module THPutDocNonExistent where

import Language.Haskell.TH

class A a where
data B

$(putDoc (InstDoc ''A (ConT ''B)) "a" >> pure [])
