-- Test type errors contain field names, not selector names

{-# LANGUAGE AllowDuplicateRecordFields #-}

data T = MkT { x :: Int }

y = x x

main = return ()
