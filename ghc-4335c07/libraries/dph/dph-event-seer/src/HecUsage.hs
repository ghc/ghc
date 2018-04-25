-- | HEC usage analysis.
--   Count time spent with none, some and all HECs active or in garbage collection.
module HecUsage where

import GHC.RTS.Events
import GHC.RTS.Events.Analysis

import Pretty

import Data.Map (Map)
import qualified Data.Map as M


-- | Count results as either N HECs active, or some GC going on
data HecCurrentCap = HecCap Int | HecGC
    deriving (Show,Eq,Ord)


data HecUsageReport = HecUsageReport
    { hrResults :: Map HecCurrentCap Timestamp
    , hrTotal   :: Timestamp }


data HecUsageState = HecUsageState
    { hsReport  :: HecUsageReport
    , hsRunThr  :: Int
    , hsRunGc   :: Int
    , hsPrevEvt :: Timestamp }


-- | An empty usage report
emptyHecR :: HecUsageReport
emptyHecR = HecUsageReport M.empty 0


-- | An empty state
emptyHecS :: HecUsageState
emptyHecS = HecUsageState emptyHecR 0 0 0


-- | Merge two usage reports, just summing their elements
mergeHecR :: HecUsageReport -> HecUsageReport -> HecUsageReport
mergeHecR (HecUsageReport lmap ltot)
          (HecUsageReport rmap rtot)
 =  HecUsageReport (M.unionWith (+) lmap rmap) (ltot+rtot)


hecUsageMachine :: Machine HecUsageState CapEvent
hecUsageMachine = Machine
    { initial = emptyHecS
    , final   = const False
    , alpha   = alph
    , delta   = delt
    }
 where
  -- Ignore events with no HEC associated
  alph (CapEvent (Just _) (Event _ evt)) = alph_evt evt
  alph _                                 = False

  -- Only interested in threads starting, stopping, and GC events
  alph_evt (RunThread _)        = True
  alph_evt (StopThread _ _)     = True
  alph_evt (StartGC)            = True
  alph_evt (EndGC)              = True
  alph_evt _                    = False

  {-# INLINE delt #-}
  delt (HecUsageState (HecUsageReport counts timetotal) runR runG timelast)
        (CapEvent (Just _cap) (Event timenow evtinfo))
   = let
        -- Update number of active threads and GCs
        (runR',runG')
          = case evtinfo of
              RunThread _     -> (runR + 1, runG)
              StopThread _ _  -> (runR - 1, runG)
              StartGC         -> (runR, runG + 1)
              EndGC           -> (runR, runG - 1)
              _               -> (runR, runG)
        diff    = timenow - timelast
        -- Insert into results
        counts' = update counts (hecCapOf runR runG) diff
        -- Count total time
        total'  = timetotal + diff
     in
        Just $ HecUsageState (HecUsageReport counts' total') runR' runG' timenow
  delt _ _ = Nothing

  -- If HECs are active and GC is running, count it as active time.
  -- Only count GC time if no HECs active.
  hecCapOf 0 0          = HecCap 0
  hecCapOf 0 n | n > 0  = HecGC
  hecCapOf n _          = HecCap n

  update !counts !k !v  = M.insertWith' (+) k v counts


instance Pretty HecCurrentCap where
  ppr h = case h of
          HecCap n
           -> ppr n <> text " caps"
          HecGC
           -> text "In GC"


instance Pretty HecUsageState where
  ppr (HecUsageState report runR runG _) = vcat [ ppr report, running' ]
   where
    running'       = if (runR,runG) == (0,0)
                     then text ""
                     else text "Some threads still running? "
                          <> (text $ show (runR,runG))



instance Pretty HecUsageReport where
  ppr (HecUsageReport counts total) = counts'
   where
    counts'        = pprMap (padL 10 . (<>text ":") . ppr) pprTimestamp counts

    pprTimestamp v = pprPercent v <> text " " <> pprTime v

    pprPercent   v = padR 5  $ ppr (v * 100 `div` total) <> text "%"

    pprTime      v = padR 10
                   $ cat
                   [ text "("
                   , pprTimestampEng v
                   , text ")" ]

