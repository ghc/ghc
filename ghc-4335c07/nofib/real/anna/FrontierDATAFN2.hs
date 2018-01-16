
-- ==========================================================--
-- === Find frontiers using Hunt's algorithm.             ===--
-- === Only works for functions whose result lattice      ===--
-- === does not contain any function spaces               ===--
-- === ("data functions").                                ===--
-- ===                                                    ===--
-- ===                                  FrontierDATAFN.hs ===--
-- ==========================================================--

module FrontierDATAFN2 where
import BaseDefs
import Utils
import MyUtils
import AbstractVals2
import SuccsAndPreds2
import AbstractMisc
import AbstractEval2
import AbsConc3
import FrontierMisc2



-- ==========================================================--
--
fdImprove :: (Route -> Route) ->    -- coercion function
             MemoList ->            -- possibly useful info
             [Domain] ->            -- argument domains
             Bool ->                -- True == naive startup
             [FrontierElem] ->      -- max-0 superset
             [FrontierElem] ->      -- min-1 superset
             ([FrontierElem], [FrontierElem])

fdImprove coerce memo dss naive max0_super min1_super
   = let          
         (zero_result_pairs, one_result_pairs) 
            = splitList (fdIsZero.coerce.second) memo
         zero_nodes_max
            = avMaxfrel (map (MkFrel . first) zero_result_pairs)
         one_nodes_min
            = avMinfrel (map (MkFrel . first) one_result_pairs)
         new_max0
            = (if naive then [MkFrel (map avTopR dss)] else max0_super)
              `avLUBmax0frontier` 
              (spMax0FromMin1 dss one_nodes_min)
         new_min1
            = (if naive then [MkFrel (map avBottomR dss)] else min1_super)
              `avGLBmin1frontier` 
              (spMin1FromMax0 dss zero_nodes_max)
     in
         (new_max0, new_min1)


-- ==========================================================--
--
fdFind :: ACMode ->
          HExpr Naam ->        -- tree of abstract function
          Domain ->            -- domain of function to be found
          [Domain] ->          -- small arg domains
          [Domain] ->          -- big arg domains
          Rep ->               -- bounding rep
          (Route -> Route) ->  -- the coercion function
          Bool ->              -- True == naive startup
          [FrontierElem] ->    -- inherited low factor's min-1-frontier
          MemoList ->          -- possibly useful information
          (Rep, MemoList)

fdFind s_or_l hexpr (Func dss Two) small_argds big_argds
       (RepTwo (Min1Max0 ar_prev min1_prev max0_prev))
       coerce naive min1_ilf memo
   = let 
         (better_max0, better_min1)
            = fdImprove coerce memo small_argds naive max0_prev min1_prev
         (fr, new_memo_additions)
            = fdFs2 s_or_l hexpr small_argds big_argds 
                    ( {-min1_ilf `avGLBmin1frontier`-} better_min1)
                    better_max0 coerce
     in
         (RepTwo fr, 
          new_memo_additions)


fdFind s_or_l hexpr (Func dss (Lift1 dts)) small_argds big_argds
       (Rep1 (Min1Max0 ar_prev min1_prev_lf max0_prev_lf) prev_hfs) 
       coerce naive min1_ilf memo
   = let 
         (better_lf_max0, better_lf_min1)
            = fdImprove (fdLo1.coerce) memo small_argds naive 
                        max0_prev_lf min1_prev_lf
         (lofact, lofact_memo_additions)
            = fdFs2 s_or_l hexpr small_argds big_argds
                    ( {-min1_ilf `avGLBmin1frontier`-} better_lf_min1)
                       better_lf_max0 (fdLo1.coerce)
         useful_lofact_memo
            = filter (not.fdIsZero.fdLo1.coerce.second) 
                     (lofact_memo_additions ++ memo)

         min1_lofact
            = case lofact of {Min1Max0 lf_ar lf_f1 lf_f0 -> lf_f1}

         (hifacts, hifact_memo_additions)
            = fdFind_aux s_or_l small_argds big_argds dts hexpr prev_hfs coerce 
                         useful_lofact_memo False min1_lofact naive
     in
         (Rep1 lofact hifacts, 
          lofact_memo_additions ++ hifact_memo_additions)


fdFind s_or_l hexpr (Func dss (Lift2 dts)) small_argds big_argds
       (Rep2 (Min1Max0 ar_prev_lf min1_prev_lf max0_prev_lf) 
             (Min1Max0 ar_prev_mf min1_prev_mf max0_prev_mf) prev_hfs)
       coerce naive min1_ilf memo
   = let 
         (better_lf_max0, better_lf_min1)
            = fdImprove (fdLo2.coerce) memo small_argds naive
                        max0_prev_lf min1_prev_lf
         (lofact, lofact_memo_additions)
            = fdFs2 s_or_l hexpr small_argds big_argds
                    ( {-min1_ilf `avGLBmin1frontier`-} better_lf_min1)
                                         better_lf_max0 (fdLo2.coerce)
         useful_lofact_memo
            = filter (not.fdIsZero.fdLo2.coerce.second)
                     (lofact_memo_additions ++ memo)

         min1_lofact
            = case lofact of {Min1Max0 lf_ar lf_f1 lf_f0 -> lf_f1}

         (better_mf_max0, better_mf_min1)
            = fdImprove (fdMid2.coerce) useful_lofact_memo small_argds naive
                        max0_prev_mf min1_prev_mf
         (midfact, midfact_memo_additions)
            = fdFs2 s_or_l hexpr small_argds big_argds
                    ( {-min1_lofact `avGLBmin1frontier`-} better_mf_min1)
                                          better_mf_max0 (fdMid2.coerce)
         useful_midfact_memo
            = filter (not.fdIsZero.fdMid2.coerce.second)
                     (midfact_memo_additions ++ useful_lofact_memo)

         min1_midfact
            = case midfact of {Min1Max0 mf_ar mf_f1 mf_f0 -> mf_f1}

         (hifacts, hifact_memo_additions)
            = fdFind_aux s_or_l small_argds big_argds dts hexpr prev_hfs coerce
                         useful_midfact_memo True min1_midfact naive
     in
         (Rep2 lofact midfact hifacts,
          lofact_memo_additions ++ 
          midfact_memo_additions ++ hifact_memo_additions)


-- ==========================================================--
--
fdFind_aux :: ACMode ->
              [Domain] ->          -- small argument domains
              [Domain] ->          -- big argument domains
              [Domain] ->          -- result domains
              HExpr Naam ->        -- the tree
              [Rep] ->             -- previous high factors
              (Route -> Route) ->  -- unlifted coercion function
              MemoList ->          -- the memo
              Bool ->              -- True => Lift2,  False => Lift1
              [FrontierElem] ->    -- inherited low factor
              Bool ->              -- True => naive startup
              ([Rep], MemoList)
              

fdFind_aux s_or_l small_argds big_argds dts hexpr prev_hfs coerce 
           initial_hf_memo double_lift min1_ilf naive
   = let
         high_coerce
            = if double_lift then fdHi2 else fdHi1
         small_hifact_domains
            = map (avUncurry small_argds) dts
         hifact_info_tuples
            = myZipWith4 mkTuple (0 `myIntsFromTo` (length dts - 1))
                         prev_hfs (map avBottomR dts) small_hifact_domains
         mkTuple n hf_prev bottom small_hf_domain
            = (small_hf_domain, (high_coerce bottom n . coerce), hf_prev)

         (hf_memo_additions, hifacts)
            = mapAccuml doOne [] hifact_info_tuples

         doOne memo_adds_so_far (hf_dom, hf_coerce, hf_preev)
            = let (rep, more_memo_additions)
                     = fdFind s_or_l hexpr hf_dom small_argds big_argds 
                              hf_preev hf_coerce naive min1_ilf
                              (memo_adds_so_far ++ initial_hf_memo)
              in  (more_memo_additions ++ memo_adds_so_far, rep)
      in
         (hifacts, hf_memo_additions)


-- ==========================================================--
--
fdIdent :: Route -> Route
fdIdent p  = p

fdLo1 :: Route -> Route
fdLo1 Stop1     = Zero
fdLo1 (Up1 rs)  = One

fdHi1 :: Route -> Int -> Route -> Route
fdHi1 bottom n Stop1     = bottom
fdHi1 bottom n (Up1 rs)  = rs ## n

fdLo2 :: Route -> Route
fdLo2 Stop2       = Zero
fdLo2 Up2         = One
fdLo2 (UpUp2 rs)  = One

fdMid2 :: Route -> Route
fdMid2 Stop2       = Zero
fdMid2 Up2         = Zero
fdMid2 (UpUp2 rs)  = One

fdHi2 :: Route -> Int -> Route -> Route
fdHi2 bottom n Stop2       = bottom
fdHi2 bottom n Up2         = bottom
fdHi2 bottom n (UpUp2 rs)  = rs ## n

fdIsZero :: Route -> Bool
fdIsZero x = case x of {Zero -> True; One -> False}


-- ==========================================================--
--
fdFs2 :: ACMode -> 
         HExpr Naam ->         -- the tree
         [Domain] ->           -- small arg domains
         [Domain] ->           -- big arg domains
         [FrontierElem] ->     -- min-1 superset (inherited low factor)
         [FrontierElem] ->     -- max-0 superset (previous value)
         (Route -> Route) ->   -- the coercion function
         (Frontier, MemoList)

fdFs2 s_or_l hexpr small_argds big_argds min1_prev max0_prev coerce
   = let initial_yy
            = max0_prev
         initial_xx
            = min1_prev
         (final_yy, final_xx, finalMemo) 
            = fdFs_aux s_or_l hexpr small_argds big_argds coerce 
                       initial_yy initial_xx True [] (utRandomInts 1 2)
         result 
            = Min1Max0 (length small_argds) final_xx final_yy
     in
         --if not (sane result small_argds)
         --then panic (show (min1_prev, max0_prev, 
         --            fmSelect 2 initial_xx initial_yy False)) else
         (result, finalMemo)

--sane (Min1Max0 ar f1 f0) dss
--   = f1 == spMin1FromMax0 dss f0  &&
--     f0 == spMax0FromMin1 dss f1


-- ==========================================================--
--
fdFs_aux :: ACMode ->
            HExpr Naam ->        -- the tree
            [Domain] ->          -- small argument domains
            [Domain] ->          -- big argument domains
            (Route -> Route) ->  -- the coercion function
            [FrontierElem] ->    -- tentative max-0 frontier
            [FrontierElem] ->    -- tentative min-1 frontier
            Bool ->              -- True == take from top
            MemoList ->          -- memo list so far
            [Int] ->
            ([FrontierElem], [FrontierElem], MemoList)

fdFs_aux s_or_l hexpr small_argds big_argds
         coerce trial_max_yy trial_min_xx fromTop memo rands
   = let edgez 
            = fmSelect (head rands) trial_min_xx trial_max_yy fromTop
         Just (MkFrel args)
            = edgez
         args_at_proper_sizes
            = myZipWith3 (acConc s_or_l) big_argds small_argds args
         evald_app 
            = aeEvalExact hexpr (map HPoint args_at_proper_sizes)
         coerced_evald_app
            = coerce evald_app
         revised_max_yy 
            = fmReviseMaxYY small_argds trial_max_yy (MkFrel args)
         revised_min_xx 
            = fmReviseMinXX small_argds trial_min_xx (MkFrel args)
         new_memo
            = (args, evald_app) : memo
     in
         if      fmIsNothing edgez
         then    (sort trial_max_yy, sort trial_min_xx, memo)
         else 
         if      coerced_evald_app == One
         then    fdFs_aux s_or_l
                          hexpr small_argds big_argds coerce 
                          revised_max_yy trial_min_xx False new_memo (tail rands)
         else
         if      coerced_evald_app == Zero
         then    fdFs_aux s_or_l 
                          hexpr small_argds big_argds coerce 
                          trial_max_yy revised_min_xx True new_memo (tail rands)
         else    panic "fdFs_aux"
       


-- ==========================================================--
-- === end                              FrontierDATAFN.hs ===--
-- ==========================================================--
