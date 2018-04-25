
-- ==========================================================--
-- === Concretisation of function points.                 ===--
-- ===                                        AbsConc3.hs ===--
-- ==========================================================--

module AbsConc3 where
import BaseDefs
import Utils
import MyUtils
import AbstractVals2
import SuccsAndPreds2
import AbstractMisc
import DomainExpr


-- ==========================================================--
-- 
acUncurryWRT :: Domain -> Domain -> Domain
--              small     big
--
acUncurryWRT Two anyThing
   = anyThing

acUncurryWRT (Lift1 ds1) (Lift1 ds2)
   = Lift1 (myZipWith2 acUncurryWRT ds1 ds2)

acUncurryWRT (Lift2 ds1) (Lift2 ds2)
   = Lift2 (myZipWith2 acUncurryWRT ds1 ds2)

acUncurryWRT (Func ds_s dt_s) (Func ds_b dt_b)
   = let small_arity  = length ds_s
         big_arity    = length ds_b
         fixed_at_outer_level
            = if     small_arity == big_arity 
              then   Func ds_b dt_b
              else
              if     small_arity < big_arity
              then   Func (take small_arity ds_b) 
                          (Func (drop small_arity ds_b) dt_b)
              else   panic "acUncurryWRT"
         totally_fixed 
            = case fixed_at_outer_level of
                 Func ds_ol dt_ol
                   -> Func (myZipWith2 acUncurryWRT ds_s ds_ol) 
                           (acUncurryWRT dt_s dt_ol)
     in
        totally_fixed


-- ==========================================================--
-- 
acNormAndCurried :: Domain -> Domain -> (Domain, Domain)

acNormAndCurried small_d big_d
   = let big_d_u = amStrongNormalise big_d
     in (big_d_u, acUncurryWRT small_d big_d_u)


-- ==========================================================--
--              big domain   smaller domain
acCompatible :: Domain ->    Domain      -> Bool
--
-- In the (DFunc _ _) (DFunc _ _) case, note that
-- the big domain is 
-- assumed to be curried, so its apparent arity is
-- the same as that of the small domain.
--
acCompatible _ Two
   = True

acCompatible (Lift1 ds1) (Lift1 ds2)
   = myAndWith2 acCompatible ds1 ds2

acCompatible (Lift2 ds1) (Lift2 ds2)
   = myAndWith2 acCompatible ds1 ds2

acCompatible (Func big_ss big_t) (Func smaller_ss smaller_t)
   = acCompatible big_t smaller_t &&
     myAndWith2 acCompatible big_ss smaller_ss

acCompatible _ _
   = False


-- ==========================================================--
-- 
acConc :: ACMode -> Domain -> Domain -> Route -> Route

acConc s_or_l big_d small_d small_r
   = let isFn = case small_d of { Func _ _ -> True; _ -> False }
         (big_d_u, big_d_c)
            = if isFn then acNormAndCurried small_d big_d else (big_d, big_d)
         isOk = acCompatible big_d_c small_d
         small_rep = case small_r of Rep rep -> rep
     in
         if    big_d == small_d
         then  small_r
         else
         if    not isOk 
         then  panic "acConc: incompatible domains\n\n"
         else
         if    isFn 
         then  Rep (acConcRep s_or_l big_d_c big_d_u small_d small_rep)
         else  acConcData s_or_l big_d_u small_d small_r


-- ==========================================================--
--                      big       small
--
acConcData :: ACMode -> Domain -> Domain -> Route -> Route

acConcData s_or_l db Two One
   = avTopR db
acConcData s_or_l db Two Zero
   = avBottomR db

acConcData s_or_l (Lift1 dbs) (Lift1 dss) Stop1
   = Stop1
acConcData s_or_l (Lift1 dbs) (Lift1 dss) (Up1 rs)
   = Up1 (myZipWith3 (acConc s_or_l) dbs dss rs)

acConcData s_or_l (Lift2 dbs) (Lift2 dss) Stop2
   = Stop2
acConcData s_or_l (Lift2 dbs) (Lift2 dss) Up2
   = Up2
acConcData s_or_l (Lift2 dbs) (Lift2 dss) (UpUp2 rs)
   = UpUp2 (myZipWith3 (acConc s_or_l) dbs dss rs)


-- ==========================================================--
--                     big_c     big_u     small
acConcRep :: ACMode -> Domain -> Domain -> Domain -> Rep -> Rep

acConcRep s_or_l big_d_c@(Func dss_b_c dt_b_c)
                 big_d_u@(Func dss_b_u dt_b_u)
                 small_d@(Func dss_s_c dt_s_c)
                 rep
     = let concd_source
              = acConcSource s_or_l big_d_u small_d rep
           concd_source_d
              = amStrongNormalise (acConcSourceD big_d_c small_d)
           concd_all
              = acConcTarget s_or_l dt_b_c concd_source_d concd_source
       in  
           concd_all


-- ==========================================================--
-- Concretise target domain of a function.  
--                    target_big    rep_current
acConcTarget :: ACMode -> Domain -> Domain -> Rep -> Rep
-- target_big may be a function space, derived from 
-- *curried* final desired domain.
--
acConcTarget   
     s_or_l
     Two
   c@(Func dsc Two)
   f@(RepTwo _)
   = f


acConcTarget   
     s_or_l
     (Lift1 dts)
   c@(Func dsc Two)
   f@(RepTwo fr)
   = let doOne dt = acConcTarget s_or_l dt c f
     in
         Rep1 fr (map doOne dts)


acConcTarget   
     s_or_l
     (Lift2 dts)
   c@(Func dsc Two)
   f@(RepTwo fr)
   = let doOne dt = acConcTarget s_or_l dt c f
     in
         Rep2 fr fr (map doOne dts)


acConcTarget
     s_or_l
     (Func es g)  
   c@(Func dsc Two)
   f@(RepTwo fr)
   = let arity_increase   = length es
         new_c            = Func (dsc++es) Two
         increased_arity
            = case s_or_l of
                 Safe -> ac_increase_arity_safe arity_increase dsc es fr
                 Live -> ac_increase_arity_live arity_increase dsc es fr
     in
         acConcTarget s_or_l g new_c increased_arity


acConcTarget
     s_or_l
     (Lift1 dts_b)
   c@(Func dss (Lift1 dts_s))
   f@(Rep1 lf hfs)
   = let hfds_small = map (avUncurry dss) dts_s
         hfds_big   = map (avUncurry dss) dts_b
         hfds_targ  = myZipWith2 doOne hfds_small hfds_big
         doOne (Func xxss_s xxt_s) (Func xxss_b xxt_b)
            = let xxss_fin = drop (length xxss_s) xxss_b
              in
                  if     null xxss_fin
                  then   xxt_b
                  else   Func xxss_fin xxt_b
         hfs_big = myZipWith3 (acConcTarget s_or_l) hfds_targ hfds_small hfs
     in
         Rep1 lf hfs_big


acConcTarget
     s_or_l
     (Lift2 dts_b)
   c@(Func dss (Lift2 dts_s))
   f@(Rep2 lf mf hfs)
   = let hfds_small = map (avUncurry dss) dts_s
         hfds_big   = map (avUncurry dss) dts_b
         hfds_targ  = myZipWith2 doOne hfds_small hfds_big
         doOne (Func xxss_s xxt_s) (Func xxss_b xxt_b)
            = let xxss_fin = drop (length xxss_s) xxss_b
              in
                  if     null xxss_fin
                  then   xxt_b
                  else   Func xxss_fin xxt_b
         hfs_big = myZipWith3 (acConcTarget s_or_l) hfds_targ hfds_small hfs
     in
         Rep2 lf mf hfs_big



-- ==========================================================--
--
ac_increase_arity_safe :: Int ->        -- arity increase
                          [Domain] ->   -- existing arg domains
                          [Domain] ->   -- new arg domains
                          Frontier ->   -- the function 
                          Rep

ac_increase_arity_safe arity_increase argds new_argds fr
   = let special_case   = avIsBottomRep (RepTwo fr)
         final_argds    = argds ++ new_argds
         new_bottoms    = map avBottomR new_argds
         special_fix    = avBottomR_aux (Func final_argds Two)
     in
         if      special_case
         then    special_fix
         else    ac_ia_aux Safe arity_increase new_bottoms final_argds fr



-- ==========================================================--
--
ac_increase_arity_live :: Int ->        -- arity increase
                          [Domain] ->   -- existing arg domains
                          [Domain] ->   -- new arg domains
                          Frontier ->   -- the function 
                          Rep

ac_increase_arity_live arity_increase argds new_argds fr
   = let special_case   = avIsTopRep (RepTwo fr)
         final_argds    = argds ++ new_argds
         new_tops       = map avTopR new_argds
         special_fix    = avTopR_aux (Func final_argds Two)
     in
         if      special_case
         then    special_fix
         else    ac_ia_aux Live arity_increase new_tops final_argds fr



-- ==========================================================--
--
ac_ia_aux :: ACMode ->     -- mode
             Int ->        -- arity increase
             [Route] ->    -- top/bottom routes for new args
             [Domain] ->   -- arg domains **after** arity increase
             Frontier ->   -- the function
             Rep

ac_ia_aux 
     s_or_l 
     ai 
     new_points
     final_argds
     (Min1Max0 ar f1 f0)
   = let (new_f1, new_f0) = ac_extend_fr s_or_l final_argds f1 f0 new_points
     in
         RepTwo (Min1Max0 (ar+ai) new_f1 new_f0)



-- ==========================================================--
--
ac_extend_fr :: ACMode -> 
                [Domain] -> 
                [FrontierElem] -> 
                [FrontierElem] ->
                [Route] -> 
                ([FrontierElem], [FrontierElem])

ac_extend_fr s_or_l final_argds f1 f0 new_points
   = let new_f0_safe = [MkFrel (frel++new_points) | MkFrel frel <- f0]
         new_f1_safe = spMin1FromMax0 final_argds new_f0_safe 
         new_f1_live = [MkFrel (frel++new_points) | MkFrel frel <- f1]
         new_f0_live = spMax0FromMin1 final_argds new_f1_live 
     in  
         case s_or_l of
            Safe -> (new_f1_safe, new_f0_safe)
            Live -> (new_f1_live, new_f0_live)


-- ==========================================================--
--                            big_args    small_args
acConcSource_aux :: ACMode -> [Domain] -> [Domain] -> Frontier -> Frontier

acConcSource_aux Safe dbs dss (Min1Max0 ar f1 f0)
   = let dbs_used = take ar dbs
         new_f0 = map ( \(MkFrel pts) ->
                  MkFrel (myZipWith3 (acConc Safe) dbs_used dss pts)) f0
         new_f1 = spMin1FromMax0 dbs_used new_f0
     in
         Min1Max0 ar new_f1 new_f0

acConcSource_aux Live dbs dss (Min1Max0 ar f1 f0)
   = let dbs_used = take ar dbs
         new_f1 = map ( \(MkFrel pts) ->
                  MkFrel (myZipWith3 (acConc Live) dbs_used dss pts)) f1
         new_f0 = spMax0FromMin1 dbs_used new_f1
     in
         Min1Max0 ar new_f1 new_f0


-- ==========================================================--
-- Concretise source domain of a function
--                        big       small
acConcSource :: ACMode -> Domain -> Domain -> Rep -> Rep
--
-- we pass in the *entire* desired final domain of the
-- function, in *curried* form. This is preserved by
-- subsequent calls. In general
-- there may be more "dss" than components of each frontier
-- point.  This is OK: only as many "dss" as are relevant
-- are actually concretised.  "acConcTarget"
-- sticks on those "dss" components ignored here.
--

acConcSource s_or_l (Func dss_b dt_b) 
                    (Func dss_s Two) 
                    (RepTwo fr)
   = RepTwo (acConcSource_aux s_or_l dss_b dss_s fr)


acConcSource s_or_l (Func dss_b (Lift1 dts_b)) 
                    (Func dss_s (Lift1 dts_s))
                    (Rep1 lf hfs)
   = let new_lf             = acConcSource_aux s_or_l dss_b dss_s lf
         hf_big_domains     = map (avUncurry dss_b) dts_b
         hf_small_domains   = map (avUncurry dss_s) dts_s
         new_hfs
            = myZipWith3 (acConcSource s_or_l) hf_big_domains hf_small_domains hfs
     in
         Rep1 new_lf new_hfs


acConcSource s_or_l (Func dss_b (Lift2 dts_b)) 
                    (Func dss_s (Lift2 dts_s))
                    (Rep2 lf mf hfs)
   = let new_lf             = acConcSource_aux s_or_l dss_b dss_s lf
         new_mf             = acConcSource_aux s_or_l dss_b dss_s mf
         hf_big_domains     = map (avUncurry dss_b) dts_b
         hf_small_domains   = map (avUncurry dss_s) dts_s
         new_hfs
            = myZipWith3 (acConcSource s_or_l) hf_big_domains hf_small_domains hfs
     in
         Rep2 new_lf new_mf new_hfs


-- ==========================================================--
-- Figure out the domain of the thing created by acConcSource.
--               big       small
acConcSourceD :: Domain -> Domain -> Domain

acConcSourceD (Func dss_b dt_b) (Func dss_s Two)
   = Func dss_b Two

acConcSourceD (Func dss_b (Lift1 dts_b)) (Func dss_s (Lift1 dts_s))
   = let low_fac_arity   = length dss_s
         hf_big_ds       = map (avUncurry dss_b) dts_s  {- XXXXXX -}
         hf_small_ds     = map (avUncurry dss_s) dts_s
         hf_resultants   = myZipWith2 acConcSourceD hf_big_ds hf_small_ds
         hf_res2         = map drop_lf_ar hf_resultants
         drop_lf_ar (Func ess et) 
            = let ess2 = drop low_fac_arity ess
              in     if null ess2 
              then   et
              else   Func ess2 et
     in
         Func dss_b (Lift1 hf_res2)

acConcSourceD (Func dss_b (Lift2 dts_b)) (Func dss_s (Lift2 dts_s))
   = case
        acConcSourceD (Func dss_b (Lift1 dts_b)) (Func dss_s (Lift1 dts_s))
     of
        Func dss_res (Lift1 dts_res) -> Func dss_res (Lift2 dts_res)


-- ==========================================================--
--
acMakeInstance :: ACMode ->  -- should be Safe for real applications
                  DExpr ->   -- simplest instance domain of point (DXFunc _ _)
                  DSubst ->  -- binds domain vars to required instances
                  Route ->   -- simplest instance of function
                  Route      -- function at desired instance
{- Constraints, assumptions, &c.
   1.  The domain variables in the DExpr correspond exactly with
       those supplied in the DSubst.
   2.  The DExpr is of the form (DXFunc _ _) and correspondingly
       the supplied Point is of the form (DFunc _ _, RFunc _),
       and that the DFunc's args and DXFunc's args are 
       appropriately related.
-}
acMakeInstance s_or_l
               simplest_dx
               rho_d
               f_simplest
  = let
       finalDomain = amStrongNormalise (dxApplyDSubst rho_d simplest_dx)
       basicDomain =                   (dxApplyDSubst_2 simplest_dx)
    in
       acConc s_or_l finalDomain basicDomain f_simplest


-- ==========================================================--
-- === end                                    AbsConc3.hs ===--
-- ==========================================================--
