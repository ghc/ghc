
-- ==========================================================--
-- === Find frontiers using Hunt's algorithm.             ===--
-- ===                                 FrontierSearch5.hs ===--
-- ==========================================================--

module FrontierGENERIC2 where
import BaseDefs
import Utils
import MyUtils
import AbstractVals2
import SuccsAndPreds2
import AbstractEval2
import AbsConc3
import FrontierMisc2
import FrontierDATAFN2
import AbstractMisc
import Apply


-- ==========================================================--
--
fsMakeFrontierRep :: ACMode ->      -- safe or live
                     Bool ->        -- True == naive initialisation
                     HExpr Naam ->  -- the tree
                     Domain ->      -- domain of function to be found (abstraction)
                     [Domain] ->    -- arg domains at full size
                     Route ->       -- upper bound
                     Route ->       -- lower bound
                     (Route, Int)   -- abstraction of function


fsMakeFrontierRep s_or_l naive hexpr func_domain big_arg_ds 
                  lower_boundR upper_boundR 
   = let
         (is_caf, small_arg_ds) 
            = case func_domain of
                 Func [] dt        -> (True, panic "fsMakeFrontierRep(1)")
                 Func dss dt       -> (False, dss)
                 non_func_domain   -> (True, panic "fsMakeFrontierRep(2)")
         getRep (Rep rep)
            = rep
         upper_bound
            = getRep upper_boundR
         lower_bound
            = getRep lower_boundR
         bound_rep
            = fsZULB upper_bound lower_bound
         init_memo
            = []
         caf_result
            = aeEvalConst hexpr
         non_data_fn_result
            = fsFind s_or_l hexpr func_domain 
                     small_arg_ds big_arg_ds bound_rep 0 [] naive
         (data_fn_result, final_memo)
            = fdFind s_or_l hexpr func_domain
                     small_arg_ds big_arg_ds bound_rep fdIdent naive 
                     (panic "no inherited min1") init_memo
         data_fn_evals
            = length final_memo
         caf_result_norm
            = case caf_result of {Rep rep -> apPapConst rep; other -> other}
         is_data_fn
            = amIsDataFn func_domain
       in
         if     is_caf
         then   (caf_result_norm, 0)
         else
         if     is_data_fn
         then   (Rep data_fn_result, data_fn_evals)
         else   (Rep non_data_fn_result, (-1))



-- ==========================================================--
--
fsFind :: ACMode ->
          HExpr Naam ->       -- tree
          Domain ->           -- domain (abstraction) of fn to be found
          [Domain] ->         -- small arg domains
          [Domain] ->         -- big arg domains
          Rep ->              -- bounding rep
          Int ->              -- something to do with the AppInfo
          [AppInfo] ->        -- the AppInfo (surprise!)
          Bool ->             -- naive start
          Rep

fsFind 
     s_or_l 
     hexpr 
     (Func dss Two) 
     small_argds 
     big_argds 
     (RepTwo bounds) n as naive
   = 
     RepTwo (fsFs2 s_or_l 
                   hexpr 
                   small_argds 
                   big_argds 
                   bounds
                   (as++[A2])
                   naive )


fsFind
     s_or_l
     hexpr
     (Func dss (Lift1 dts))
     small_argds
     big_argds
     (Rep1 bounds_lf bounds_hfs) n as naive
   =
     let
         lofact 
            = fsFs2 s_or_l
                    hexpr
                    small_argds
                    big_argds
                    bounds_lf
                    (as++[ALo1])
                    naive
         hifact_ds
            = map (avUncurry dss) dts
         lofact_arity
            = length dss
         hifacts
            = myZipWith4 doOne 
                         hifact_ds 
                         dts 
                         bounds_hfs 
                         (0 `myIntsFromTo` (length dts - 1))
         doOne hifact_d hifact_targ_domain bounds nn
            = fsFind s_or_l
                     hexpr
                     hifact_d
                     small_argds
                     big_argds
                     bounds
                     lofact_arity
                     (as++[AHi1 lofact_arity nn hifact_targ_domain])
                     naive
     in
         Rep1 lofact hifacts


fsFind
     s_or_l
     hexpr
     (Func dss (Lift2 dts))
     small_argds
     big_argds
     (Rep2 bounds_lf bounds_mf bounds_hfs) n as naive
   =
     let
         lofact 
            = fsFs2 s_or_l
                    hexpr
                    small_argds
                    big_argds
                    bounds_lf
                    (as++[ALo2])
                    naive
         midfact
            = fsFs2 s_or_l
                    hexpr
                    small_argds
                    big_argds
                    bounds_mf
                    (as++[AMid2])
                    naive
         hifact_ds
            = map (avUncurry dss) dts
         lofact_arity
            = length dss
         hifacts
            = myZipWith4 doOne 
                         hifact_ds 
                         dts 
                         bounds_hfs 
                         (0 `myIntsFromTo` (length dts - 1))
         doOne hifact_d hifact_targ_domain bounds nn
            = fsFind s_or_l
                     hexpr
                     hifact_d
                     small_argds
                     big_argds
                     bounds
                     lofact_arity
                     (as++[AHi2 lofact_arity nn hifact_targ_domain])
                     naive
     in
         Rep2 lofact midfact hifacts


-- ==========================================================--
--
fsApp :: [AppInfo] ->
         [HExpr Naam] ->
         HExpr Naam ->
         Route

fsApp [A2] xs h 
   = fsEvalConst h xs

fsApp [ALo1] xs h
   = case fsEvalConst h xs of
        Stop1  -> Zero
        Up1 _  -> One

fsApp ((AHi1 n x d):as) xs h
   = let app_res       = fsEvalConst h (take n xs)
         nth_upp_obj   = case app_res of
                            Stop1   -> avBottomR d
                            Up1 rs  -> rs ## x
     in
         fsApp as (drop n xs) (HPoint nth_upp_obj)

fsApp [ALo2] xs h
   = case fsEvalConst h xs of
        Stop2    -> Zero
        Up2      -> One
        UpUp2 _  -> One

fsApp [AMid2] xs h
   = case fsEvalConst h xs of
        Stop2    -> Zero
        Up2      -> Zero
        UpUp2 _  -> One

fsApp ((AHi2 n x d):as) xs h
   = let app_res       = fsEvalConst h (take n xs)
         nth_upp_obj   = case app_res of
                            Stop2     -> avBottomR d
                            Up2       -> avBottomR d
                            UpUp2 rs  -> rs ## x
     in
         fsApp as (drop n xs) (HPoint nth_upp_obj)


-- ==========================================================--
--
fsEvalConst :: HExpr Naam ->
               [HExpr Naam] ->
               Route

fsEvalConst h@(HLam _ _) xs = aeEvalExact h xs
fsEvalConst h@(HPoint p) [] = p
fsEvalConst h@(HPoint _) xs = aeEvalConst (HVAp h xs)


-- ==========================================================--
--
fsFs2 :: ACMode ->
         HExpr Naam ->
         [Domain] ->        -- small arg domains
         [Domain] ->        -- big arg domains
         Frontier ->        -- bounds
         [AppInfo] ->
         Bool ->            -- True == naive startup
         Frontier

fsFs2
     s_or_l
     hexpr
     small_argds
     big_argds
     (Min1Max0 ar1 min1_init max0_init)
     as
     naive
   =
     let arity
            = length small_argds
         initial_yy
            = if     naive
              then   [MkFrel (map avTopR small_argds)]   
              else   max0_init
         initial_xx
            = if     naive
              then   [MkFrel (map avBottomR small_argds)]
              else   min1_init
         (final_yy, final_xx)
            = fsFs_aux s_or_l
                       hexpr
                       small_argds
                       big_argds
                       initial_yy
                       initial_xx
                       as
                       True
                       (utRandomInts 1 2)
     in
         Min1Max0 arity final_xx final_yy



-- ==========================================================--
--
fsFs_aux :: ACMode ->
            HExpr Naam ->
            [Domain] ->          -- small arg domains
            [Domain] ->          -- real arg domains
            [FrontierElem] ->    -- yy_frontier
            [FrontierElem] ->    -- xx_frontier
            [AppInfo] ->         -- application info
            Bool ->              -- True == take from top
            [Int] ->             -- random numbers
            ([FrontierElem], [FrontierElem])

fsFs_aux 
     s_or_l
     hexpr
     small_argds
     big_argds
     trial_max_yy
     trial_min_xx
     app_info
     fromTop
     rands
   =
     let
         edges
            = fmSelect (head rands) trial_min_xx trial_max_yy fromTop
         Just (MkFrel args)
            = edges
         args_at_proper_sizes
            = makeBigger args small_argds big_argds
         evald_app
            = fsApp app_info (map HPoint args_at_proper_sizes) hexpr
         revised_max_yy 
            = fmReviseMaxYY small_argds trial_max_yy (MkFrel args)
         revised_min_xx 
            = fmReviseMinXX small_argds trial_min_xx (MkFrel args)
         makeBigger rs     []     []      
            = rs
         makeBigger (r:rs) (s:ss) (b:bs)
            = acConc s_or_l b s r : makeBigger rs ss bs
     in
         if      fmIsNothing edges 
         then    (sort trial_max_yy, sort trial_min_xx)
         else 
         if      evald_app == One
         then    fsFs_aux s_or_l
                          hexpr
                          small_argds
                          big_argds
                          revised_max_yy
                          trial_min_xx
                          app_info
                          False
                          (tail rands)
         else
         if      evald_app == Zero
         then    fsFs_aux s_or_l
                          hexpr
                          small_argds
                          big_argds
                          trial_max_yy
                          revised_min_xx
                          app_info
                          True
                          (tail rands)
         else    
                 panic "fsFs_aux"
       


-- ==========================================================--
-- === end                             FrontierSearch5.hs ===--
-- ==========================================================--
