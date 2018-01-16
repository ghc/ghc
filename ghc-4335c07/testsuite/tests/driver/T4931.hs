{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -DFOO="bar baz" #-}
main = print FOO

-- Test that GHC can compile option pragmas containing spaces.
-- When a .hsc contains `#define FOO "bar baz"`, hsc2hs emits:
--
--     {-# OPTIONS_GHC -optc-DFOO="bar baz" #-}
