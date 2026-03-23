{-# OPTIONS_GHC -dno-typeable-binds #-} -- don't generate $trModule bindings, which require some known-keys
module Internal (

    A (A)

) where

    data A = A
