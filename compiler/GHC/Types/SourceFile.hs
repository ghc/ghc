module GHC.Types.SourceFile
   ( HscSource(..)
   , SourceModified (..)
   , isHsBootOrSig
   , isHsigFile
   , hscSourceString
   )
where

import GHC.Prelude
import GHC.Utils.Binary

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

-- | Indicates whether a given module's source has been modified since it
-- was last compiled.
data SourceModified
  = SourceModified
       -- ^ the source has been modified
  | SourceUnmodified
       -- ^ the source has not been modified.  Compilation may or may
       -- not be necessary, depending on whether any dependencies have
       -- changed since we last compiled.
  | SourceUnmodifiedAndStable
       -- ^ the source has not been modified, and furthermore all of
       -- its (transitive) dependencies are up to date; it definitely
       -- does not need to be recompiled.  This is important for two
       -- reasons: (a) we can omit the version check in checkOldIface,
       -- and (b) if the module used TH splices we don't need to force
       -- recompilation.

