{-# LANGUAGE DataKinds #-}

module UnsatisfiableFail1 where

import GHC.TypeError

type Msg = Text "Cannot call 'uncallable'."

uncallable :: Unsatisfiable Msg => ()
uncallable = unsatisfiable @Msg

rejected = uncallable
