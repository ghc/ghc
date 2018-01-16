-- | Module with single foreign export
module MyForeignLib.Hello (sayHi) where

import MyForeignLib.SomeBindings
import MyForeignLib.AnotherVal

foreign export ccall sayHi :: IO ()

-- | Say hi!
sayHi :: IO ()
sayHi = putStrLn $
     "Hi from a foreign library! Foo has value " ++ show valueOfFoo
  ++ " and anotherVal has value " ++ show anotherVal
