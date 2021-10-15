{-# LANGUAGE PolyKinds, UnliftedNewtypes, NoFieldSelectors #-}
import GHC.Exts

newtype Y (a :: TYPE rep) = MkY { y_fld :: a }
