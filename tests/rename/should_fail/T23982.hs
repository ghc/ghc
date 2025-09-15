module T23982 where

import T23982_aux (A)

-- Trying to use the type constructor A instead of the data constructor A
-- Should get a message that's more helpful than just "Illegal term level use".
test = A
