{-# LANGUAGE TypeFamilies #-}

-- The class/instance setup for #27016: an associated type family `Line`
-- whose instance `Line HDoc = HLine` is the redex that used to block the
-- specialisation rule for pprLabel from firing.  See T27016b, T27016.
module T27016a where

newtype HLine = HLine { runHLine :: Int -> Int }
newtype HDoc  = HDoc  { runHDoc  :: Int -> Int }

class IsLine d where
  txt   :: String -> d
  (<+>) :: d -> d -> d

class IsLine (Line d) => IsDoc d where
  type Line d
  line :: Line d -> d

instance IsLine HLine where
  txt s = HLine (\n -> n + length s)
  HLine f <+> HLine g = HLine (f . g)

instance IsDoc HDoc where
  type Line HDoc = HLine
  line (HLine f) = HDoc f
