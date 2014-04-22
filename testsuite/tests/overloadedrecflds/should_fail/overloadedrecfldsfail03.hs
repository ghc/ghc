{-# LANGUAGE OverloadedRecordFields #-}

foo = True

data T = MkT { foo :: Int }

main = print foo
