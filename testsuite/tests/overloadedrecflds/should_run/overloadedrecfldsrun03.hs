{-# LANGUAGE OverloadedRecordFields, TypeFamilies #-}

data family F a

data instance F Int  = MkFInt  { foo :: Int }
data instance F Bool = MkFBool { bar :: Bool }


data family G a

data instance G Int = MkGInt { foo :: Int }
data instance G Bool = MkGBool { bar :: Bool }


main = do print (foo (MkFInt 42))
          print (foo (MkGInt 42))
          print (bar (MkFBool True))
          print (bar (MkGBool True))
