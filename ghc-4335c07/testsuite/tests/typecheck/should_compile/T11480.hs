{-# LANGUAGE FlexibleContexts, UndecidableSuperClasses #-}

module T11480 where

class C [a] => D a
class D a => C a

foo :: C a => a -> a
foo = undefined
