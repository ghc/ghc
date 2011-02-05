-- !!! Default export list isn't the same as (module M)
-- This should succeed, exporting only the local 'sort',
-- and not being confused by the 'sort' from 'List'.
-- (Hugs gets this wrong)

module M where 

import Data.List as M
sort = "foo"
