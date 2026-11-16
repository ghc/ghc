-- | The result of one benchmark run, as written by @eps-hackage-run@ and read
-- by @eps-hackage summary@. Shared by both packages (they are built with
-- different compilers), stored with 'show' / 'read'.
module EpsHackage.Result
  ( Result (..)
  , StoreStats (..)
  ) where

data Result = Result
  { resLabel         :: String         -- ^ the compiler (setup label)
  , resMode          :: String         -- ^ e.g. @ghc-api@, @store:sqlite@
  , resSucceeded     :: Bool
  , resWallSeconds   :: Double         -- ^ of the 'GHC.load', without the final GC
  , resMutatorSeconds :: Double
  , resGcSeconds     :: Double
  , resAllocatedBytes :: Integer
  , resMaxLiveBytes  :: Integer        -- ^ max residency, sampled at major GCs
  , resMaxMemInUseBytes :: Integer     -- ^ peak memory the RTS had in use
  , resLiveAtEndBytes :: Integer       -- ^ after a major GC at the end, session alive
  , resMaxRssBytes   :: Integer        -- ^ VmHWM: includes memory outside the heap
  , resMajorGcs      :: Integer
  , resStoreStats    :: Maybe StoreStats
  , resOutputs       :: [(FilePath, String)]
      -- ^ hashes of the .hi/.o files, if requested (needs -fobject-determinism
      -- to be comparable)
  } deriving (Show, Read)

data StoreStats = StoreStats
  { ssModules      :: Int
  , ssDecls        :: Int
  , ssLookups      :: Int
  , ssCacheHits    :: Int
  , ssRehydrations :: Int
  } deriving (Show, Read)
