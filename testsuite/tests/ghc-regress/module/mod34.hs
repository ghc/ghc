-- !!! Defining local value with same name as imported class member.
-- was: Imported member fun clashes with local var definition
module M where
import Ix(Ix(..))
index x = x
