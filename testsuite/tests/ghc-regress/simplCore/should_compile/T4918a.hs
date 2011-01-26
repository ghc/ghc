module T4918a where

class MyEnum a where 
 myEnum :: [a]

instance MyEnum () where 
 myEnum = [()]

