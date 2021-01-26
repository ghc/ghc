{-# OPTIONS_GHC -O -ddump-rule-rewrites #-}
{-# LANGUAGE MagicHash #-}

module T18668 where

import GHC.Exts

{-# RULES "funky" (+#) = (*#) #-}
{-# RULES "flip" forall x. (>#) x = (<#) x #-}

x = (I# (2# +# 3#), I# (1# ># 0#))
