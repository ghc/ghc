module T22048 where

{-# NOINLINE g #-}
g :: Bool -> Bool
g = not

-- With -fomit-interface-pragmas these rules should not make it into interface files.
{-# RULES
"imported_rule"    [~1] forall xs. map g xs              = []
"local_rule"       [~1] forall .   g True                = False
#-}
