{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE PatternSynonyms #-}

module T18960b where

pattern P :: a -> a
pattern P x = x
{-# COMPLETE P :: () #-}

bar :: ()
bar = case undefined of
    P ((), "hello") -> ()
    -- prints too many of apparently the same missing patterns,
    -- because we prefer to report positive info (@((), [])@) rather than
    -- negative info (@P ((), x:_) where x is not one of {'h'}@)

baz :: ()
baz = case undefined of
    P ((), "hello") -> ()
    -- This function is proof that we in theory can provide a "better" warning.
