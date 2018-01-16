
-- ==========================================================--
-- === Miscellaneous stuff for the frontiers algorithm.   ===--
-- ===                                    FrontierMisc.hs ===--
-- ==========================================================--

module FrontierMisc2 where
import BaseDefs
import Utils
import MyUtils
import AbstractVals2
import SuccsAndPreds2
import AbstractMisc



-- ==========================================================--
--
fsZULB :: Rep -> Rep -> Rep

fsZULB (RepTwo fru) (RepTwo frl)
   = RepTwo (fsZULB_2 fru frl)

fsZULB (Rep1 lfu hfsu) (Rep1 lfl hfsl)
   = Rep1 (fsZULB_2 lfu lfl) (myZipWith2 fsZULB hfsu hfsl)

fsZULB (Rep2 lfu mfu hfsu) (Rep2 lfl mfl hfsl)
   = Rep2 (fsZULB_2 lfu lfl) (fsZULB_2 mfu mfl) (myZipWith2 fsZULB hfsu hfsl)

fsZULB_2 (Min1Max0 aru f1u f0u) (Min1Max0 arl f1l f0l)
   = Min1Max0 aru f1l f0u


-- ==========================================================--
--
fmSelect :: Int ->
            [FrontierElem] ->
            [FrontierElem] ->
            Bool ->
            Maybe FrontierElem

fmSelect a_rand up_space down_space fromTop
   = let min_max_pairs
            = take 30 [(mi, ma) | mi <- up_space,
                                    ma <- down_space, mi `avBelowEQfrel` ma]
         mmpl = length min_max_pairs
         n = a_rand `mod` mmpl
         selected_pair = min_max_pairs ## n
     in
         if null min_max_pairs
         then Nothing
         else 
         if fromTop
         then Just (second selected_pair)
         else Just (first  selected_pair)


-- ==========================================================--
--
fmIsNothing :: Maybe a -> Bool

fmIsNothing Nothing   = True
fmIsNothing (Just _)  = False


-- ==========================================================--
--
fmMaxIntersection :: [FrontierElem] -> [FrontierElem] -> [FrontierElem]

fmMaxIntersection xx yy
   = avMaxfrel [ x `avGLBfrel` y | x <- xx, y <- yy ]


-- ==========================================================--
--
fmMinIntersection :: [FrontierElem] -> [FrontierElem] -> [FrontierElem]

fmMinIntersection xx yy
   = avMinfrel [ x `avLUBfrel` y | x <- xx, y <- yy ]


-- ==========================================================--
--
fmReviseMinXX :: [Domain] ->
                 [FrontierElem] -> 
                 FrontierElem -> 
                 [FrontierElem]

fmReviseMinXX ds trial_min_xx args
   = let (x_underneath, x_not_underneath)
            = splitList (`avBelowEQfrel` args) trial_min_xx
         optimised_result 
            = fmReviseMinXX_aux
                    (fmMinIntersection x_underneath (spSuccsFrel ds args))
                    x_not_underneath
         fmReviseMinXX_aux xs ys 
            = if     length xs < length ys 
              then   foldr avMinAddPtfrel xs ys
              else   foldr avMinAddPtfrel ys xs
     in
         optimised_result


-- ==========================================================--
--
fmReviseMaxYY :: [Domain] -> 
                 [FrontierElem] -> 
                 FrontierElem -> 
                 [FrontierElem]

fmReviseMaxYY ds trial_max_yy args
   = let (y_above, y_not_above)
            = splitList (args `avBelowEQfrel`) trial_max_yy
         optimised_result
            = fmReviseMaxYY_aux
                    y_not_above
                    (fmMaxIntersection y_above (spPredsFrel ds args))
         fmReviseMaxYY_aux xs ys
            = if    length xs > length ys
              then  foldr avMaxAddPtfrel xs ys
              else  foldr avMaxAddPtfrel ys xs
     in
         optimised_result


-- ==========================================================--
-- === end                                FrontierMisc.hs ===--
-- ==========================================================--
