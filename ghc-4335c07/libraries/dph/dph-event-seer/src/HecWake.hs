-- HEC wakeup analysis
--      Find the times between a HEC asking to wake a thread on a different HEC.
module HecWake where

import GHC.RTS.Events
import GHC.RTS.Events.Analysis

import Data.Map (Map)
import qualified Data.Map as M

import Data.List (sortBy)

import Pretty


-- | A single wake-up event. List of these used for results.
data HecWake = HecWake
                !Timestamp -- ^ length
                !Timestamp -- ^ when
                !ThreadId

data HecWakeState = HecWakeState (Map ThreadId Timestamp) [HecWake]

hecWakeMachine :: Machine HecWakeState CapEvent
hecWakeMachine = Machine
  { initial = HecWakeState M.empty []
  , final   = const False
  , alpha   = alph
  , delta   = delt
  }
 where
  alph (CapEvent Nothing _) = False
  alph (CapEvent _ (Event _ (RunThread _))) = True
  -- Only wakeups that are pointed at other caps
  alph (CapEvent (Just thiscap) (Event _ (WakeupThread _ other))) = thiscap /= other
  alph _ = False

  -- Record the wake request, so we can find time difference when it starts
  delt (HecWakeState !waketimes lens) (CapEvent (Just thiscap) (Event timenow (WakeupThread tid other)))
   | thiscap /= other
   = Just $ HecWakeState (ins tid timenow waketimes) lens
  -- Record time difference when a thread starts
  delt (HecWakeState !waketimes !lens) (CapEvent (Just _cap) (Event timenow (RunThread tid)))
   | Just timethen <- M.lookup tid waketimes
   = let wake' = HecWake (timenow - timethen) timethen tid in
     Just $ HecWakeState (M.delete tid waketimes) (wake' : lens)
  delt s _ = Just s

  -- Record wakeup, but don't overwrite an older value
  ins !k !v !m
        | M.member k m  = m
        | otherwise     = M.insert k v m



pprHecWake :: HecWakeState -> Doc
pprHecWake (HecWakeState waketimes lens) = vcat [ waketimes', lens' ]
 where
  waketimes' = if M.null waketimes then text "" else text "not all threads scheduled to wake up actually woke up!"

  lens'
   = if null lens
        then text "No wakes"
        else vcat
            [ text "Min:   " <> ppr minL
            , text "Max:   " <> ppr maxL
            , text "Med:   " <> (padR 10 $ pprTimestampEng medianL)
            , text "Avg:   " <> (padR 10 $ pprTimestampEng avgL)
            , text "Count: " <> (padR 10 $ ppr lenL)
            ]
  sorted  = sortBy cmp lens
  lenL    = length sorted

  minL    = head sorted
  maxL    = last sorted
  avgL    = (sum $ map lenOf sorted) `div` fromIntegral lenL

  medianL = if lenL `mod` 2 == 0
            then (lenOf (sorted !! lenL_2) + lenOf (sorted !! (lenL_2 - 1))) `div` 2
            else lenOf $ sorted !! lenL_2
  lenL_2  = lenL `div` 2

  cmp x y = lenOf x `compare` lenOf y
  lenOf (HecWake len _ _) = len

instance Pretty HecWake where
  ppr (HecWake len when what) = cat
        [ padR 10 $ pprTimestampEng len
        , text " (@"
        , padR 10 $ pprTimestampAbs when
        , text ", thread "
        , text $ show what
        , text ")"
        ]
