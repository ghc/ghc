{-# LANGUAGE ExperimentalDependentTypes, TemplateHaskell #-}
module NativeTypeLetQuote where

import Language.Haskell.TH (Q, Type)

quote :: Q Type
quote = [t| let b = Bool in Maybe b |]
