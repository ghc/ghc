{-# LANGUAGE TemplateHaskell #-}
module PunTypeSplice (a) where
import Language.Haskell.TH (Q, Type)
a :: Q Type
a = [t| Int |]
