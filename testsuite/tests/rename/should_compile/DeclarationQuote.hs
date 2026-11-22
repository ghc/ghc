{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module DeclarationQuote where
import Data.List (List)
import Language.Haskell.TH (Dec, Q)

quoted :: Q (List Dec)
quoted = [d|
  data Both = Both
  quotedUse :: Both -> Both
  quotedUse Both = Both
  |]

data Outer = T

shadowAcrossQuote :: Q (List Dec)
shadowAcrossQuote = [d|
  data T = X
  typeHere :: T
  typeHere = X
  termOutside :: Outer
  termOutside = T
  |]

shadowWithinQuote :: Q (List Dec)
shadowWithinQuote = [d|
  a :: Bool
  a = True
  local :: forall a. a -> Bool
  local _ = a
  |]
