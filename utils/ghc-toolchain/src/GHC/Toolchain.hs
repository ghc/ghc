module GHC.Toolchain
  ( module GHC.Toolchain.Target
  , module GHC.Toolchain.Tools.Ar
  , module GHC.Toolchain.Tools.Cc
  , module GHC.Toolchain.Tools.Cpp
  , module GHC.Toolchain.Tools.Cxx
  , module GHC.Toolchain.Tools.Link
  , module GHC.Toolchain.Tools.MergeObjs
  , module GHC.Toolchain.Tools.Nm
  , module GHC.Toolchain.Tools.Ranlib
  ) where

import GHC.Toolchain.Target

import GHC.Toolchain.Tools.Ar
import GHC.Toolchain.Tools.Cc
import GHC.Toolchain.Tools.Cpp
import GHC.Toolchain.Tools.Cxx
import GHC.Toolchain.Tools.Link
import GHC.Toolchain.Tools.MergeObjs
import GHC.Toolchain.Tools.Nm
import GHC.Toolchain.Tools.Ranlib


-- Note [ghc-toolchain overview]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This is @ghc-toolchain@, a library and executable which we ship with GHC for
-- configuring compilation targets and their necessary native toolchains.
--
-- The @ghc-toolchain@ library contains definitions regarding a target description file:
--
--  * "GHC.Toolchain.Program" defines Program as an executable and the set of flags to run it.
--
--  * "GHC.Toolchain.Tools.*" define various tools of the toolchain, each wraps a Program
--    and potentially characteristics about the tool (e.g. the ccLinkSupportsNoPie field of CcLink)
--
--  * A 'Target' is the top-level definition that describes a target, including
--    the toolchain tools targeting this target, and the characteristics of the
--    target (e.g. 'WordSize')
--    This definition is already used by the Hadrian build system.
--
-- The executable tool fills a similar role as autoconf; specifically it:
--
--  * determines various characteristics of the platform (e.g. the word size)
--
--  * probes the constituent tools needed by GHC (e.g. the C compiler, ar
--    archiver, object merging tool, etc.),
--
--  * checks for various quirks and bugs in the toolchain
--
--  * from this information, constructs a "target description" which GHC will
--    use at compile-time
--
-- The goal here is to facilitate convenient cross-compilation by allowing
-- users to add new target descriptions to an existing GHC installation
-- (whereas previously autoconf would perform the above only at
-- installation-time of the binary distribution).
--
-- In addition to being used by the user, ghc-toolchain is also invoked by
-- GHC's source distribution @configure@ script during bootstrapping
-- (see m4/ghc_toolchain.m4). In this case, Hadrian also consumes the target
-- description to determine how it should invoke the various toolchain components
-- (see 'Hadrian.Oracles.TextFile.getBuildTarget' and friends).


