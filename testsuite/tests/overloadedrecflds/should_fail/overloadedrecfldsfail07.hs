-- Test type errors contain field names, not selector names

{-# LANGUAGE DuplicateRecordFields #-}

data T = MkT { x :: Int }

y = x x

main = return ()
