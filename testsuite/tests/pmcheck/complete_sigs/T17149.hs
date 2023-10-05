{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bug where

class Member a b where
  prj :: b -> Maybe a

pattern P :: Member a b => a -> b
pattern P a <- (prj -> Just a)

{-# COMPLETE P :: Bool #-}

-- | Trying to instantiate P with 0 type arguments doesn't work, it takes 2.
-- This seemingly unrelated fact, only relevant through the COMPLETE set, may
-- not lead the compiler to crash or do shady stuff.
fun :: Bool -> ()
fun True = ()
fun _    = ()
