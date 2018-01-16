-- | Total of DPH operations and, for each one, percent of time n HECs were active:
--   For example, if a mapD was parallel but extracts was sequential, it might show as
--
--  Extracts    300ms
--      0 caps   0%
--      1 caps  90%
--      2 caps   2%
--      3 caps   0%
--      4 caps   0%
--      In GC    8%
--
--  MapD        100ms
--      0 caps   0%
--      1 caps   5%
--      2 caps   0%
--      3 caps   0%
--      4 caps  95%
--      In GC    0%
--
module DphOpsHecs where

import GHC.RTS.Events
import GHC.RTS.Events.Analysis
import Pretty
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Array.Parallel.Unlifted.Distributed.What as W
import           Data.Set (Set)
import qualified Data.Set as S
import qualified DphOps   as D
import qualified Events   as E
import qualified HecUsage as H
import Data.List        (sortBy)


type DphOpsHecsComps
    = ( Int                 -- ^ count - number of times called
      , Timestamp           -- ^ total time spent
      , H.HecUsageReport    -- ^ current report total
      , [CapEvent]          -- ^ pending events, to be processed when comp is completed
      , H.HecUsageState )   -- ^ HEC state when comp was started

-- | State for tracking ops * hecs
data DphOpsHecsState = DphOpsHecsState
    { sWhats     :: Map W.Comp DphOpsHecsComps
    , sTotalTime :: Timestamp
    , sUnparsed  :: Int
    , sOpenWhats :: Set W.Comp
    , sHecState  :: H.HecUsageState }


-- | Convert a bunch of CapEvents into a report
dphOpsHecs :: [CapEvent] -> DphOpsHecsState
dphOpsHecs capEvts
 =  let dphEvts  = concatMap getDphEvt $ map D.parseComp $ D.getGangEvents capEvts
        capEvts' = map getCapTs capEvts
        -- list of DPH operations and HEC events, interspersed, ordered by time
        merged   = mergeFst dphEvts capEvts'

        state    = DphOpsHecsState M.empty 0 0 S.empty H.emptyHecS 
    in  foldl go state merged
 where
  go s (_,evt)
   = case evt of
     -- DPH operation has completed, so perform HEC analysis on that comp
     Left (D.DTComplete what dur)
      -> let what'  = D.clearJoinComp what
             whats' = updateWhats what' dur (sWhats s)
         in  s { sOpenWhats = S.delete what' (sOpenWhats s)
               , sWhats     = whats'
               , sTotalTime = dur + sTotalTime s }

     -- DPH operation has started: mark it as 'open' so we can track events for it
     -- Also note the starting state (currently running threads etc) for later
     Left (D.DTIssuing  what)
      -> let what' = D.clearJoinComp what
         in  s { sOpenWhats = S.insert what' (sOpenWhats s)
               , sWhats     = updateWhatHec what' (sHecState s) (sWhats s)}

     -- Track events to any open ops, and update current state
     Right capEvt
      ->     s { sWhats     = insCap (sWhats s) (sOpenWhats s) capEvt
               , sHecState  = step (sHecState s) capEvt }

  -- Completed DPH operation, so run HEC analysis if it has any pending events
  updateWhats what dur whats
   = case M.lookup what whats of
     Nothing
      -> whats
     Just (cnt,durtot,hecRpt,caps, hecSt@(H.HecUsageState _ hRunR hRunG hPrevTime))
      -> let mach    = H.hecUsageMachine { initial = H.HecUsageState H.emptyHecR hRunR hRunG hPrevTime }
             hec'    = E.runMachine mach $ reverse caps
             hecRpt' = H.mergeHecR hecRpt $ H.hsReport hec'
         in  
             M.insert what (cnt+1, dur+durtot, hecRpt', [], hecSt) whats

  -- Set starting state
  updateWhatHec what hecSt whats
   = case M.lookup what whats of
     Nothing
      -> M.insert what (0, 0, H.emptyHecR, [], hecSt) whats
     Just (cnt,durtot,hecRpt,caps, _)
      -> M.insert what (cnt, durtot, hecRpt, caps, hecSt) whats

  -- Single-step evaluate HEC usage
  step state capEvt
   = let mach = H.hecUsageMachine { initial = state }
     in  E.runMachine mach [capEvt]

  -- Record event in all open comps
  insCap whats opens capEvt
   = S.fold (insCap' capEvt) whats opens
  insCap' capEvt comp whats
   = M.insertWith
        (\_ (cnt, dur, hecs, caps, hec) -> (cnt, dur, hecs, capEvt:caps, hec))
        comp
        (0, 0, H.emptyHecR, [capEvt], H.emptyHecS)
        whats

-- | Filter DPH events to get just valid ones, and their time
getDphEvt :: D.ParseComp -> [(Timestamp, Either D.DphTrace CapEvent)]
getDphEvt e
 = case e of
   D.POk d t     -> [(t, Left d)]
   D.PIgnored    -> []
   D.PUnparsable -> []

-- | Put cap event into either so it can be merged with DPH events
getCapTs :: CapEvent -> (Timestamp, Either D.DphTrace CapEvent)
getCapTs c@(CapEvent _ (Event timenow _))
 = (timenow, Right c)

-- | Take two sorted lists and merge them together, so the output is sorted
mergeFst :: Ord a => [(a,b)] -> [(a,b)] -> [(a,b)]
mergeFst as' bs' = go as' bs'
 where
  go [] bs = bs
  go as [] = as
  go ((ta,a):as) ((tb,b):bs)
     | ta < tb   = (ta,a) : go as ((tb,b):bs)
     | otherwise = (tb,b) : go ((ta,a):as) bs

instance Pretty DphOpsHecsState where
 ppr (DphOpsHecsState whats total unparse _ _) = vcat [unparse', ops']
  where
   unparse'
        = if unparse == 0
          then text ""
          else text "Errors: " <> ppr unparse <> text " events unable to be parsed"

   ops' = vcat $ map pprOp $ sortBy cmp $ M.assocs whats
   cmp (_,(_,p,_,_,_)) (_,(_,q,_,_,_))
        = case compare p q of
          GT -> LT
          EQ -> EQ
          LT -> GT

   pprOp (c,(calls,duration,hecRpt,_,_))
    = vcat
     [ padLines (cat
        [ pprPercent duration
        , text " "
        , padR 10 (text "(" <> pprTimestampEng duration <> text ")")
        , text " "
        , padR 10 (ppr calls)
        , text " "
        ]) (show $ ppr c)
     , nest 10 $ ppr hecRpt 
     , text "" ]

   pprPercent   v = padR 5  $ ppr (v * 100 `div` total) <> text "%"

