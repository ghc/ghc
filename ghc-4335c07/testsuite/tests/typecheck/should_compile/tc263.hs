{-# LANGUAGE IncoherentInstances, MultiParamTypeClasses, FlexibleInstances #-}

-- Get a non-incoherent instance from that file
import Tc263_Help

instance C [a] b where foo = undefined
instance C a Int where foo = undefined

y :: ([Int],Int)
y = foo

main = return ()
