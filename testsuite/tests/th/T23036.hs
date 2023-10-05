{-# LANGUAGE TemplateHaskell #-}
module T23036 where

import Language.Haskell.TH

a, b, c :: ()
a = $([|let x = undefined in ()|])
b = $([|let !x = undefined in ()|])
c = $([|let ~x = undefined in ()|])

-- Test strictness annotations are also correctly handled in function and pattern binders
d, e, f:: ()
d = $([|let !(x,y) = undefined in ()|])
e = $([|let (!x,y,~z) = undefined in ()|])
f = $([|let f !x ~y z = undefined in ()|])

