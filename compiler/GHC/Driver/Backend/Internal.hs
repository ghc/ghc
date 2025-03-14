{-|
Module      : GHC.Driver.Backend.Internal
Description : Interface for migrating legacy clients of the GHC API

In versions of GHC up through 9.2, a `Backend` was represented only by
its name.  This module is meant to aid clients written against the GHC
API, versions 9.2 and older.  The module provides an alternative way
to name any back end found in GHC 9.2.  /Code within the GHC source
tree should not import this module./ (#20927).

Only back ends found in version 9.2 have names.

-}

module GHC.Driver.Backend.Internal
   ( -- * Name of a back end
     BackendName(..)
   )

where


import GHC.Prelude

data BackendName
   = NCG           -- ^ Names the native code generator backend.
   | LLVM          -- ^ Names the LLVM backend.
   | ViaC          -- ^ Names the Via-C backend.
   | JavaScript    -- ^ Names the JS backend.
   | Interpreter   -- ^ Names the ByteCode interpreter.
   | NoBackend     -- ^ Names the `-fno-code` backend.
 deriving (Eq, Show)
