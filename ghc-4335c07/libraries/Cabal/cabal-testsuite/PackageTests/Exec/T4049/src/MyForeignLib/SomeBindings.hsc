-- | Module that needs the hsc2hs preprocessor
module MyForeignLib.SomeBindings where

#define FOO 1

#ifdef FOO
-- | Value guarded by a CPP flag
valueOfFoo :: Int
valueOfFoo = 5678
#endif
