{-# LANGUAGE RankNTypes #-}

-- Dead arguments are kept in specialisations.
-- See Note [Do not drop dead args from specialisations] in GHC.Core.Opt.Specialise

module ShouldCompile where

foo :: () -> Show a => a -> String
foo _x y = show y ++ "!"
{-# NOINLINE[0] foo #-}

bar :: String
bar = foo () (42 :: Int)
