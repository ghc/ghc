-- Nice test from Ross: should export only the 
-- local 'sort', and not be confused by the 'sort' from 'List'

module M where

import List as M
sort = "foo"