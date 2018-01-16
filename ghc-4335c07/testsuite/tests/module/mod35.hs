-- !!! Redefining imported data constructors
-- was: Imported constructor clashes with local constructor
module M where
import Prelude(Bool(True,False)) 
data T = True
