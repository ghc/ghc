{-# LANGUAGE NoImplicitPrelude #-}

-- Counterpart to T27013a/T27013d (see #27013, !15899).
--
-- Like the `composition` package, this module needs no known-key.
-- It is compiled with -hide-all-packages (see all.T) and WITHOUT
-- -frebindable-known-names (unlike T27013{a,d}).
--
-- Under the default -fno-rebindable-known-names, GHC implicitly imports
-- GHC.Essentials, which fails because nothing provides it, even though this
-- module needs no known entity. We want a structured error here (not a panic)
-- that points at -frebindable-known-names as the way to avoid the implicit
-- import. See Note [Finding GHC.Essentials] in GHC.Builtin.
module T27013f where

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)
