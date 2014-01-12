{-# LANGUAGE ImplicitParams, GADTs #-}

module T3743 where

class Foo a

data M where M :: Foo a => a -> M

x :: (?x :: ()) => ()
x = undefined

-- foo :: (?x :: ()) => M -> ()
foo y = case y of
    M _ -> x
