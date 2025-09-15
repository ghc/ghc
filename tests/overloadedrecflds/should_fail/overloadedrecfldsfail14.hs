{-# LANGUAGE DuplicateRecordFields #-}

-- Test that we deal gracefully with non-fields in updates

data S = MkS { x :: Int }
data T = MkT { x :: Int }

y :: Bool
y = True

-- y isn't a field
f r = r { x = 3, y = False }

main = return ()
