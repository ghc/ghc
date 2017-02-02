{-# LANGUAGE PolyKinds #-}

import Type.Reflection
import Data.Kind

data ComposeK (f :: k' -> Type) (g :: k -> k') a = ComposeK (f (g a))

main :: IO ()
main = do
    let x :: ComposeK Maybe Maybe Int
        x = undefined

    App x y <- pure $ typeOf x
    print (x, y)

    App x y <- pure x
    print (x, y)

    App x y <- pure x
    print (x, y)

    App x y <- pure x   -- This makes GHC panic
    print (x, y)
