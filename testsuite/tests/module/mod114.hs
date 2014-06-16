-- !!! Type re-exportation test
-- (from bug reported by Ross Paterson.)
module Mod114 (Stuff) where
import Mod114_Help hiding(Stuff(..))
