module GHC.Types.SourceFile
   ( HscSource(..)
   , hscSourceToIsBoot
   , isHsBootOrSig
   , isHsigFile
   , hscSourceString
   )
where

import GHC.Prelude
import GHC.Utils.Binary
import GHC.Unit.Types

-- Note [HscSource types]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- There are three types of source file for Haskell code:
--
--      * HsSrcFile is an ordinary hs file which contains code,
--
--      * HsBootFile is an hs-boot file, which is used to break
--        recursive module imports (there will always be an
--        HsSrcFile associated with it), and
--
--      * HsigFile is an hsig file, which contains only type
--        signatures and is used to specify signatures for
--        modules.
--
-- Syntactically, hs-boot files and hsig files are quite similar: they
-- only include type signatures and must be associated with an
-- actual HsSrcFile.  isHsBootOrSig allows us to abstract over code
-- which is indifferent to which.  However, there are some important
-- differences, mostly owing to the fact that hsigs are proper
-- modules (you `import Sig` directly) whereas HsBootFiles are
-- temporary placeholders (you `import {-# SOURCE #-} Mod).
-- When we finish compiling the true implementation of an hs-boot,
-- we replace the HomeModInfo with the real HsSrcFile.  An HsigFile, on the
-- other hand, is never replaced (in particular, we *cannot* use the
-- HomeModInfo of the original HsSrcFile backing the signature, since it
-- will export too many symbols.)
--
-- Additionally, while HsSrcFile is the only Haskell file
-- which has *code*, we do generate .o files for HsigFile, because
-- this is how the recompilation checker figures out if a file
-- needs to be recompiled.  These are fake object files which
-- should NOT be linked against.

data HscSource
   = HsSrcFile  -- ^ .hs file
   | HsBootFile -- ^ .hs-boot file
   | HsigFile   -- ^ .hsig file
   deriving (Eq, Ord, Show)

-- | Tests if an 'HscSource' is a boot file, primarily for constructing elements
-- of 'BuildModule'. We conflate signatures and modules because they are bound
-- in the same namespace; only boot interfaces can be disambiguated with
-- `import {-# SOURCE #-}`.
hscSourceToIsBoot :: HscSource -> IsBootInterface
hscSourceToIsBoot HsBootFile = IsBoot
hscSourceToIsBoot _ = NotBoot

instance Binary HscSource where
    put_ bh HsSrcFile = putByte bh 0
    put_ bh HsBootFile = putByte bh 1
    put_ bh HsigFile = putByte bh 2
    get bh = do
        h <- getByte bh
        case h of
            0 -> return HsSrcFile
            1 -> return HsBootFile
            _ -> return HsigFile

hscSourceString :: HscSource -> String
hscSourceString HsSrcFile   = ""
hscSourceString HsBootFile  = "[boot]"
hscSourceString HsigFile    = "[sig]"

-- See Note [HscSource types]
isHsBootOrSig :: HscSource -> Bool
isHsBootOrSig HsBootFile = True
isHsBootOrSig HsigFile   = True
isHsBootOrSig _          = False

isHsigFile :: HscSource -> Bool
isHsigFile HsigFile = True
isHsigFile _        = False
