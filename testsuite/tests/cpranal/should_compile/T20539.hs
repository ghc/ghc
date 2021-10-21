module T20593 where

newtype Measured a = Measured { unmeasure :: () -> a }
data Subdiagram = Subdiagram () (Measured ())

mkSubdiagram :: () -> Subdiagram
mkSubdiagram d = Subdiagram d (Measured (\_ -> ()))
