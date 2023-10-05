-- Test that importing an overloaded field and using it as a selector
-- leads to a suitable error

{-# LANGUAGE DuplicateRecordFields #-}

import OverloadedRecFldsFail04_A as I

-- Qualified overloaded fields are not allowed here
x' = I.x

-- But this is okay
f e = e { I.x = True, I.y = False }

main = return ()
