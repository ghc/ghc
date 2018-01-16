-- !!! Defining local type with same name as imported class
-- was: Imported class clashes with local type definition
module M where
import Prelude(Eq,Bool)
type Eq = Bool
