{-# LANGUAGE IncoherentInstances, MultiParamTypeClasses, FlexibleInstances #-}

import Tcfail218_Help

instance C [a] b where foo = undefined
instance C a Int where foo = undefined

-- Should fail, as a more specific, unifying but not matching, non-incoherent instance exists.
x :: ([a],b)
x = foo

main = return ()
