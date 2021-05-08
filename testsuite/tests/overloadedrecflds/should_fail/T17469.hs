{-# LANGUAGE DuplicateRecordFields #-}

import T17469A

main :: IO ()
main = print MkFoo { foo = "", bar = True }
