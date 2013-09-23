-- Both these functions should successfully simplify
-- using the combine-identical-alternatives optimisation

module T7360 where

data Foo = Foo1 | Foo2 | Foo3 !Int
    
fun1 :: Foo -> ()
{-# NOINLINE fun1 #-}
fun1 x = case x of
               Foo1 -> ()
               Foo2 -> ()
               Foo3 {} -> ()

fun2 x = (fun1 Foo1,  -- Keep -ddump-simpl output 
                      -- in a predicatable order
         case x of
          [] -> length x
          (_:_) -> length x)
