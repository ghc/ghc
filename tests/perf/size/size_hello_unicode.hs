-- same as size_hello_artifact but we test the size of the resulting executable with unicode general category involved.
module Main where

-- read uses unicode general category to detect spaces
main = print (read @Int "1337")
