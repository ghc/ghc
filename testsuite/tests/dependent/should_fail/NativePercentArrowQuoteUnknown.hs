{-# LANGUAGE Modifiers, TemplateHaskell #-}
module NativePercentArrowQuoteUnknown where
import Language.Haskell.TH
bad :: Q Type
bad = [t| forall p. Int %p -> Int |]
