-- !!! unqual name conflicts in export lists.  Should fail
module C ( Mod131_A.f, g, module Mod131_B ) where
import Mod131_B(f)
import qualified Mod131_A(f)
g = f 
