{-# LANGUAGE NoImplicitPrelude #-}

-- Counterpart to T27013a/T27013d (see #27013, !15899).
--
-- Like the `composition` package, this module needs no known-key.
-- It is compiled with -hide-all-packages (see all.T) and WITHOUT
-- -frebindable-known-names (unlike T27013{a,d}).
--
-- Under the default -fno-rebindable-known-names, GHC must assume
-- GHC.Essentials is an implicit dependency and tries to discover its
-- package, which fails because nothing provides it. We want a structured
-- error here (not a panic) that points at -frebindable-known-names as the
-- way to avoid forcing that dependency. See GHC.Iface.Load.
module T27013f where

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)
