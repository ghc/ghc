{-# LANGUAGE OverloadedRecordFields #-}

import OverloadedRecFldsFail04_A as I

-- Qualified overloaded fields are not allowed here
x' = I.x

-- But this is okay
f e = e { I.x = True, I.y = False }