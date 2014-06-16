-- !!! (..) importation of partially exported types
-- (test / bug report due to Ross Paterson.)
module M where
import Mod120_A(T(..))
f = Foo
