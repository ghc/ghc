{-# LANGUAGE OverloadedStrings #-}
module BadWarning where

data MyString = MyString String

f1 (MyString "a") = undefined
f1 (MyString "bb") = undefined
f1 _ = undefined

f2 (MyString "aa") = undefined
f2 (MyString "bb") = undefined
f2 _ = undefined

-- Genuine overlap here!
f3(MyString ('a':_)) = undefined
f3 (MyString "a") = undefined
f3 _ = undefined
