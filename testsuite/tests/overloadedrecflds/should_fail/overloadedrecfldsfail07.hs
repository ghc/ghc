-- Test type errors contain field names, not selector names

{-# LANGUAGE OverloadedRecordFields #-}

data T = MkT { x :: Int }

y = x x

main = return ()