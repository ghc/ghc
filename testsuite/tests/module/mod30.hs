-- !!! Imported class clashes with local class definition
module M where
import Prelude(Eq,Bool)
class Eq a where (==) :: a -> a -> Bool
