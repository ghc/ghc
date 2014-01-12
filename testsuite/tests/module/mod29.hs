-- !!! Testing for out of scope Prelude types.
-- was: Imported tycon clashes with local definition
--      (but that's OK, as long as the type isn't _used_.)
module M where
import Prelude(Int)
type Int = Char
