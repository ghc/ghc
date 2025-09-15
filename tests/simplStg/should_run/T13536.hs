import Debug.Trace

newtype Id a = Id a


unId True _ = Nothing -- make lazy
unId False (Just (Id x)) = (Just x)
unId False Nothing = Nothing
{-# NOINLINE unId #-}

val n = trace "evaluated once, as it should" (Just (Id n))
{-# NOINLINE val #-}

foo b n = unId b (val n)
{-# NOINLINE foo #-}

main = print (foo False 1)
