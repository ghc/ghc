-- !!! It is now illegal to import a module hiding 
-- !!! an entity that it doesn't export
module ShouldCompile where
import Data.List hiding ( wibble )
