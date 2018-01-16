-- !!! (..) importation of partially exported class
-- (test / bug report due to Ross Paterson.)
module M where
import Mod121_A(C(..))
f = m2 (1::Int)
