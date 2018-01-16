-- Test that DuplicateRecordFields can be used along with
-- TypeFamilies (with selectors only if unambiguous)

{-# LANGUAGE DuplicateRecordFields, TypeFamilies #-}

data family F a

data instance F Int  = MkFInt  { foo :: Int }
data instance F Bool = MkFBool { bar :: Bool, baz :: Bool }


data family G a

data instance G Int = MkGInt { foo :: Int }
data instance G Bool = MkGBool { bar :: Bool }

x = MkFBool { bar = False, baz = True }

y :: F Bool
y = x { bar = True }

get_bar MkFBool{bar=bar} = bar

main = do print (baz y)
          print (get_bar y)
