
data Foo a = Foo { foo :: a
                 , bar :: Int
                 } deriving Show

up s t = s { foo = t }

obj = Foo { foo = 1 :: Int
          , bar = 99
          }

main = print ( obj
             , up obj ()
             , obj {foo=True}
             )
