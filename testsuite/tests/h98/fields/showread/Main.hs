
data Foo a = Foo { foo :: a
                 , bar :: Int
                 } deriving (Show,Read)

up s t = s { foo = t }

obj = Foo { foo = 1 :: Int
          , bar = 99
          }

main = let expr = ( obj
                  , up obj ()
                  , obj {foo=True}
                  )
       in do
       print expr
       print (((read.show) expr) `asTypeOf` expr)
