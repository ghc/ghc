{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

{-  With "hugs -98 +o test.hs" gives me:
    ERROR "test.hs":8 - Cannot justify constraints in instance member binding
    *** Expression    : fromStr
    *** Type          : FromStr [a] => String -> [a]
    *** Given context : FromStr [a]
    *** Constraints   : FromStr [a]

    Adding the constraint "FromStr a" to the declaration of fromStr fixes 
    the problem, but that seems like it should be redundant. Removing the 
    second instance (lines 10-11) also fixes the problem, interestingly enough.

    /Bjorn Bringert -}

-- August 08: on reflection I think a complaint about overlapping
-- instances for line 8 is absolutely right, so I've changed this to
-- expected-failure

-- Sept 08: on further reflection (!) I'm changing it back
-- See Note [Subtle interaction of recursion and overlap]
-- in TcInstDcls

module ShouldCompile  where

class FromStr a where
     fromStr :: String -> a

typeError :: FromStr a => a -> a
typeError t = error "type error"

instance FromStr [a] where
     fromStr _ = typeError undefined  -- line 8

instance FromStr [(String,a)] where  -- line 10
     fromStr _ = typeError undefined  -- line 11
