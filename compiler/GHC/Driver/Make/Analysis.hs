{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module GHC.Driver.Make.Analysis where

import GHC.Prelude

import GHC.Driver.Session

import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Utils.Logger

import qualified Data.Map as Map

import Control.Monad
import Data.IORef
import Data.Maybe

import qualified Data.Map.Strict as M
import Data.Functor.Identity
import Data.Ord
import Data.List (sortBy)
import qualified Data.Set as S
import Text.Printf
import GHC.Driver.Make.Types

analyseBuildGraph :: Logger -> [MakeActionMeta] -> IO ()
analyseBuildGraph logger metas_io = do
    new_metas <- mapM (traverseMakeActionMetaX (readIORef . getIORefMaybe)) metas_io
    let all_completed = all (isJust . make_action_meta_timing) new_metas
    when all_completed $ do
      let Identity new_metas_completed = mapM (traverseMakeActionMetaX (return . Identity . fromJust)) new_metas
      let -- Longest path to each node
          lp = longest_path (info_map new_metas_completed)
          earliest_complete = earliest_finish_time new_metas_completed lp
          -- Earliest we could possibly finish with infinite processors
          latest_finish   = maximum earliest_complete
          -- Total time if we did -j1
          seq_time = sum (map (timingMillisecs . runIdentity . make_action_meta_timing) new_metas_completed)
          parrelism_score =
            seq_time
            / latest_finish

          im = info_map new_metas_completed


          max_flows = sortBy (comparing snd) $ M.assocs (M.map fst lp)

          timing_for_id mid = timingMillisecs $ runIdentity $ make_action_meta_timing (im M.! mid)

          flow_x_time :: MakeActionId -> (Flow, b) -> Double
          flow_x_time mid (flow, _) = realToFrac (getFlow flow) * timing_for_id mid

          max_flows_x_time = sortBy (comparing snd) $ M.assocs (M.mapWithKey flow_x_time lp)

          max_dur = sortBy (comparing (fmap timingMillisecs . make_action_meta_timing)) new_metas_completed

      -- Printing logic
      let print_id_pair :: (a -> SDoc) -> (MakeActionId, a) -> SDoc
          print_id_pair ppr_a (mid, dat) = ppr (make_action_meta_origin info) <+> parens (int (getMakeActionId mid)) <> colon <+> ppr_a dat
           where
            info = im M.! mid


          header s = text "=====" <+> text s <+> text "====="
          block s body = vcat [ header s, body ]
          vcat_numbered docs = vcat $ zipWith (\n doc -> text (printf "%0*d" padding n) <+> doc) [0 :: Int ..] docs
            where
              padding = ceiling @Double @Int (logBase 10 (fromIntegral $ length docs))

      logDumpFile logger (mkDumpStyle alwaysQualify) Opt_D_dump_make_stats "make" FormatText $ vcat [
        block "Maximum Duration" (
          vcat_numbered  (map (print_id_pair (ppr . runIdentity)) (map ((,) <$> make_action_meta_id <*> make_action_meta_timing) (reverse $ max_dur)))
          ),
        block "Maximum Flows"
          (vcat_numbered (map (print_id_pair ppr) (reverse max_flows))),
        block "Flows x Time"
          (vcat_numbered (map (print_id_pair (doublePrec 3)) (reverse max_flows_x_time))),
        block "Statistics"
          (vcat [ text "longest path" <> colon <+> doublePrec 3 latest_finish <> text "s"
               , text "parallelism score" <> colon <+> doublePrec 3 parrelism_score
               , text "sequential time" <> colon <+> doublePrec 3 seq_time <> text "s"
               ]) ]



  where

    MakeActionId last_action_id = make_action_meta_id $ last (sortBy (comparing make_action_meta_id) metas_io)

    first_action_ids = map make_action_meta_id $ (filter (null . make_action_meta_dep_ids) metas_io)

    info_map :: [MakeActionMetaX f] -> M.Map MakeActionId (MakeActionMetaX f)
    info_map metas = M.fromList [(make_action_meta_id m, m) | m <- metas]

    earliest_finish_time :: [MakeActionMetaX Identity] -> M.Map MakeActionId (Flow, Double) -> M.Map MakeActionId Double
    earliest_finish_time meta_info m = Map.fromList
      [(make_action_meta_id, timingMillisecs t + (maybe 0 snd $ M.lookup make_action_meta_id m))
      | MakeActionMeta{..} <- meta_info
      , Identity t <- [make_action_meta_timing] ]

    -- Creates a map of "earliest start time"
    longest_path :: M.Map MakeActionId (MakeActionMetaX Identity) -> M.Map MakeActionId (Flow,  Double)
    longest_path node_info = foldl' go M.empty (map MakeActionId [0..last_action_id])
      where
        reverse_deps :: M.Map MakeActionId (S.Set MakeActionId)
        reverse_deps = Map.fromListWith (S.union) [(dep, S.singleton make_action_meta_id) | MakeActionMeta{..} <- metas_io, dep <- make_action_meta_dep_ids]

        go :: M.Map MakeActionId (Flow, Double)
              -> MakeActionId
              -> M.Map MakeActionId (Flow, Double)
        go m cur_id =
          let
              (flow_to_me, time_to_me, m') = case M.lookup cur_id m of
                                              Just (flow, time) -> (flow, time, m)
                                              Nothing -> (initial_flow, 0, M.insert cur_id (initial_flow, 0) m)

              cur_info   = node_info M.! cur_id
              rev_deps   = fromMaybe S.empty $ M.lookup cur_id reverse_deps
              Identity cur_time = make_action_meta_timing cur_info
              out_flow = splitFlow flow_to_me (length rev_deps)
          in foldl' (update_children out_flow (time_to_me + timingMillisecs cur_time)) m' rev_deps

        update_children new_f new_t m upd_id =
          M.insertWith comb upd_id (new_f, new_t) m

        comb (a1, b1) (a2, b2) = (a1 `addFlow` a2, max b1 b2)

        initial_flow = splitFlow initialFlow (length first_action_ids)

newtype Flow = Flow { getFlow :: Rational } deriving (Eq, Show, Ord)

instance Outputable Flow where
  ppr (Flow f) = doublePrec 3 (realToFrac f)

initialFlow :: Flow
initialFlow = Flow 1
splitFlow :: Flow -> Int -> Flow
splitFlow (Flow f) n = Flow (f / fromIntegral n)
addFlow :: Flow -> Flow -> Flow
addFlow (Flow f) (Flow g) = Flow (f + g)