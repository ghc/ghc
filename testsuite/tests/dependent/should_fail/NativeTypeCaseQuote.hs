{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
module NativeTypeCaseQuote where
import Language.Haskell.TH (Q, Type)
q :: Q Type
q = [t| case 'True of { True -> Int; False -> Bool } |]
