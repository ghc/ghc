module GHC.Types.Target
   ( Target(..)
   , TargetId(..)
   , InputFileBuffer
   , pprTarget
   , pprTargetId
   )
where

import GHC.Prelude
import GHC.Driver.Phases ( Phase )
import GHC.Unit
import GHC.Data.StringBuffer ( StringBuffer )
import GHC.Utils.Outputable

import Data.Time

-- | A compilation target.
--
-- A target may be supplied with the actual text of the
-- module.  If so, use this instead of the file contents (this
-- is for use in an IDE where the file hasn't been saved by
-- the user yet).
data Target
  = Target {
      targetId           :: !TargetId, -- ^ module or filename
      targetAllowObjCode :: !Bool,     -- ^ object code allowed?
      targetContents     :: !(Maybe (InputFileBuffer, UTCTime))
      -- ^ Optional in-memory buffer containing the source code GHC should
      -- use for this target instead of reading it from disk.
      --
      -- Since GHC version 8.10 modules which require preprocessors such as
      -- Literate Haskell or CPP to run are also supported.
      --
      -- If a corresponding source file does not exist on disk this will
      -- result in a 'SourceError' exception if @targetId = TargetModule _@
      -- is used. However together with @targetId = TargetFile _@ GHC will
      -- not complain about the file missing.
    }

data TargetId
  = TargetModule !ModuleName
        -- ^ A module name: search for the file
  | TargetFile !FilePath !(Maybe Phase)
        -- ^ A filename: preprocess & parse it to find the module name.
        -- If specified, the Phase indicates how to compile this file
        -- (which phase to start from).  Nothing indicates the starting phase
        -- should be determined from the suffix of the filename.
  deriving Eq

type InputFileBuffer = StringBuffer


pprTarget :: Target -> SDoc
pprTarget (Target id obj _) =
    (if obj then empty else char '*') <> pprTargetId id

instance Outputable Target where
    ppr = pprTarget

pprTargetId :: TargetId -> SDoc
pprTargetId (TargetModule m) = ppr m
pprTargetId (TargetFile f _) = text f

instance Outputable TargetId where
    ppr = pprTargetId

