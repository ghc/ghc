module T20598b where

import T20598a

class ToValue a where
  toValue :: a -> Value

instance ToValue Value where
  toValue = id

instance ToValue a => ToValue [a] where
  toValue = mkArray . map toValue
