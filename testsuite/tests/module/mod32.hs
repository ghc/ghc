-- !!! Defining local class with same name as imported type.
-- was: Imported tycon clashes with local class definition
module M where
import Prelude(Int,Bool)
class Int a where (==) :: a -> a -> Bool
