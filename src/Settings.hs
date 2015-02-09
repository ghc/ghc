module Settings (
    IntegerLibrary (..), integerLibrary,
    buildHaddock
    ) where

data IntegerLibrary = IntegerGmp | IntegerGmp2 | IntegerSimple

instance Show IntegerLibrary where
    show library = case library of
         IntegerGmp    -> "integer-gmp"
         IntegerGmp2   -> "integer-gmp2"
         IntegerSimple -> "integer-simple"

integerLibrary :: IntegerLibrary
integerLibrary = IntegerGmp2

buildHaddock :: Bool
buildHaddock = True
