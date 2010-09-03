
module T3286 (train) where

import qualified Data.Map as M
import Data.List (groupBy, foldl')
import Data.Maybe (fromMaybe, fromJust)
import Data.Function (on)
import T3286b

type Prob = LogFloat

learn_states :: (Ord state) => [(observation, state)] -> M.Map state Prob
learn_states xs = histogram $ map snd xs

learn_observations ::  (Ord state, Ord observation) =>
                       M.Map state Prob
                    -> [(observation, state)]
                    -> M.Map (observation, state) Prob
learn_observations state_prob = M.mapWithKey f . histogram
    where f (_, state) prob = prob / (fromJust $ M.lookup state state_prob)

histogram :: (Ord a) => [a] -> M.Map a Prob
histogram xs = let hist = foldl' undefined M.empty xs in
                M.map (/ M.foldrWithKey (\_ a b -> a + b) 0 hist) hist

train :: (Ord observation, Ord state) =>
            [(observation, state)]
         -> (observation -> [Prob])
train sample = model
    where
        states = learn_states sample
        state_list = M.keys states

        observations = learn_observations states sample
        observation_probs = fromMaybe (fill state_list []) . (flip M.lookup $
                            M.fromList $ map (\ (e, xs) -> (e, fill state_list xs)) $
                                map (\ xs -> (fst $ head xs, map snd xs)) $
                                groupBy     ((==) `on` fst)
                                            [(observation, (state, prob))
                                                | ((observation, state), prob) <- M.toAscList observations])

        model = observation_probs

        fill :: Eq state => [state] -> [(state, Prob)] -> [Prob]
        fill = undefined
