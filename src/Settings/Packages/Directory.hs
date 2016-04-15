module Settings.Packages.Directory (directoryPackageArgs) where

import Expression
import GHC (directory)
import Predicates (builderCc, package)

-- TODO: I had to define symbol __GLASGOW_HASKELL__ as otherwise directory.c is
-- effectively empty. I presume it was expected that GHC will be used for
-- compiling all C files, but I don't know why. It seems that directory.c is the
-- only file which requires special treatment when using GCC.
directoryPackageArgs :: Args
directoryPackageArgs = package directory ?
    builderCc ? arg "-D__GLASGOW_HASKELL__"
