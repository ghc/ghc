{-# LANGUAGE DataKinds #-}

module Unsatisfiable1 where

import GHC.TypeError ( Unsatisfiable, unsatisfiable, ErrorMessage(Text) )

type Msg = Text "Cannot call 'uncallable'."

uncallable :: Unsatisfiable Msg => ()
uncallable = unsatisfiable @Msg

uncallable' :: Unsatisfiable Msg => ()
uncallable' = uncallable

-------------------------------------

unusual :: Unsatisfiable Msg => Char
unusual = 42  -- no error

k :: Unsatisfiable (Text "No") => ()
k = uncallable  -- no error
