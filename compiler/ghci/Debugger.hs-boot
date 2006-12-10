module Debugger where
import Breakpoints
import qualified Data.Map as Map
import Data.Array.Unboxed


data BkptTable a  = BkptTable { 
                           -- | An array of breaks, indexed by site number
     breakpoints :: Map.Map a (UArray Int Bool)  
                           -- | A list of lines, each line can have zero or more sites, which are annotated with a column number
   , sites       :: Map.Map a [[(SiteNumber, Int)]] 
   }
