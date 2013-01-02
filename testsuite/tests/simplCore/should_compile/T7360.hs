-- Both these functions should successfully simplify
-- using the combine-identical-alterantives optimisation

module T7360 where

data Foo = Foo1 | Foo2 | Foo3 !Int
    
fun1 :: Foo -> ()
fun1 x = case x of
               Foo1 -> ()
               Foo2 -> ()
               Foo3 {} -> ()

fun2 x = case x of
          [] -> length x
          (_:_) -> length x
