module GHC.Linker
   (
   )
where

-- Note [Linkers and loaders]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Linkers are used to produce linked objects (.so, executables); loaders are
-- used to link in memory (e.g., in GHCi) with the already loaded libraries
-- (ghc-lib, rts, etc.).
--
-- Linking can usually be done with an external linker program ("ld"), but
-- loading is more tricky:
--
--    * Fully dynamic:
--       when GHC is built as a set of dynamic libraries (ghc-lib, rts, etc.)
--       and the modules to load are also compiled for dynamic linking, a
--       solution is to fully rely on external tools:
--
--       1) link a .so with the external linker
--       2) load the .so with POSIX's "dlopen"
--
--    * When GHC is built as a static program or when libraries we want to load
--    aren't compiled for dynamic linking, GHC uses its own loader ("runtime
--    linker"). The runtime linker is part of the rts (rts/Linker.c).
