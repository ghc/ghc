	
-- ==========================================================--
-- === Implementation of Gebreselassie Baraki's           ===--
-- === polymorphism stuff                   BarakiConc.hs ===--
-- ==========================================================--

module BarakiConc3 where
import BaseDefs
import Utils
import MyUtils
import AbstractVals2
import SuccsAndPreds2
import AbstractMisc
import DomainExpr
import AbsConc3
import BarakiMeet


-- ==================================================--
-- === Application of a embedding functor (e-app) ===--
-- === to a point.                                ===--
-- ==================================================--

-- ==========================================================--
--
bcEApp_d :: DRRSubst -> DExpr -> Domain

bcEApp_d rho DXTwo
   = Two
bcEApp_d rho (DXLift1 dxs)
   = Lift1 (map (bcEApp_d rho) dxs)
bcEApp_d rho (DXLift2 dxs)
   = Lift2 (map (bcEApp_d rho) dxs)
bcEApp_d rho (DXFunc dss dt)
   = Func (map (bcEApp_d rho) dss) (bcEApp_d rho dt)
bcEApp_d rho (DXVar alpha)
   = bcGetD (utSureLookup rho "bcEApp_d" alpha)


-- ==========================================================--
--
bcEApp :: DRRSubst -> DExpr -> Route -> Route

bcEApp rho DXTwo Zero
   = Zero
bcEApp rho DXTwo One
   = One

bcEApp rho (DXLift1 dxs) Stop1 
   = Stop1
bcEApp rho (DXLift1 dxs) (Up1 rs)
   = Up1 (myZipWith2 (bcEApp rho) dxs rs)

bcEApp rho (DXLift2 dxs) Stop2
   = Stop2
bcEApp rho (DXLift2 dxs) Up2
   = Up2
bcEApp rho (DXLift2 dxs) (UpUp2 rs)
   = UpUp2 (myZipWith2 (bcEApp rho) dxs rs)

bcEApp rho (DXVar alpha) Zero
   = bcGetR (utSureLookup rho "bcEApp(1)" alpha)
bcEApp rho (DXVar alpha) One
   = bcGetT (utSureLookup rho "bcEApp(2)" alpha)

bcEApp rho (DXFunc dxss dxt) (Rep rep)
   = let repDomain = dxApplyDSubst_2 (DXFunc dxss dxt)
     in
         bcEdotFdotC rho dxt repDomain rep dxss


-- =============================================--
-- === Composition of an embedding functor e ===--
-- === with a function f, hence: e.f         ===--
-- =============================================--

-- ==========================================================--
--
bcEdotF :: DRRSubst ->   -- binds domain variables to Points
           DExpr ->      -- the embedding functor "e"
           Domain ->     -- domain of some function "f"
           Rep ->        -- representation of "f"
           Rep           -- representation of "e.f"


bcEdotF
     rho 
     DXTwo
   d@(Func _ Two)
   f@(RepTwo _) 
   = f


bcEdotF 
     rho 
     (DXLift1 dxs)
   d@(Func dss (Lift1 dts))
   f@(Rep1 lf hfs)
   = let hf_domains = map (avUncurry dss) dts
         new_hfs = myZipWith3 (bcEdotF rho) dxs hf_domains hfs
     in
         Rep1 lf new_hfs


bcEdotF 
     rho 
     (DXLift2 dxs)
   d@(Func dss (Lift2 dts))
   f@(Rep2 lf mf hfs)
   = let hf_domains = map (avUncurry dss) dts
         new_hfs = myZipWith3 (bcEdotF rho) dxs hf_domains hfs
     in
         Rep2 lf mf new_hfs


bcEdotF
     rho 
     (DXVar alpha)
   d@(Func ds2 Two)
   f@(RepTwo f1f0)
   = let f_top      = avTopR_aux_2 ds2
         f_top_rep  = RepTwo f_top

         evb Two = f
         evb (Lift1 es) = Rep1 f1f0 (map evb es)
         evb (Lift2 es) = Rep2 f1f0 f1f0 (map evb es)

         ev Two Zero               = f
         ev Two One                = f_top_rep

         ev (Lift1 es) Stop1       = Rep1 f1f0 (map evb es)
         ev (Lift1 es) (Up1 rs)    = Rep1 f_top (myZipWith2 ev es rs)

         ev (Lift2 es) Stop2       = Rep2 f1f0 f1f0 (map evb es)
         ev (Lift2 es) Up2         = Rep2 f_top f1f0 (map evb es)
         ev (Lift2 es) (UpUp2 rs)  = Rep2 f_top f_top (myZipWith2 ev es rs)

         evLookup = utSureLookup rho "bcEdotF" alpha
         evD = bcGetD evLookup
         evR = bcGetR evLookup
     in
         ev evD evR



-- =============================================--
-- === Composition of a function f with a    ===--
-- === list of closure functors [c1 ... cn], ===--
-- === hence: "f.[c1 ... cn]"                ===--
-- =============================================--

-- ==========================================================--
--
bcFdotC :: DRRSubst ->   -- binds domain variables to Points
           [DExpr] ->    -- the closure functor "[c1 ... cn]"
           [Domain] ->   -- ????????????????????????
           Domain ->     -- domain of some function "f"
           Rep ->        -- representation of "f"
           Rep           -- representation of "f.[c1 ... cn]"

bcFdotC rho dxs newDs (Func dss dt) rep
   = let new_rep = bcApplyF0 eapp_pt newDs rep
         eapp_pt (MkFrel fels)
           = MkFrel (myZipWith2 (bcEApp rho) dxs fels)
     in
         new_rep
          

-- ==========================================================--
-- apply some function to the max0 frontiers of a function
-- and recalculate the corresponding min1 frontiers.
--
bcApplyF0 :: (FrontierElem -> 
              FrontierElem) ->    -- what to apply to min1 points
             [Domain] ->          -- source domains of abstract function
             Rep ->               -- abstract function
             Rep                  -- resulting abstract function

bcApplyF0 f dss (RepTwo fr)
   = RepTwo (bcApplyF0_2 f dss fr)

bcApplyF0 f dss (Rep1 lf hfs)
   = let new_lf = bcApplyF0_2 f dss lf
         new_hfs = map (bcApplyF0 f dss) hfs
     in
         Rep1 new_lf new_hfs

bcApplyF0 f dss (Rep2 lf mf hfs)
   = let new_lf = bcApplyF0_2 f dss lf
         new_mf = bcApplyF0_2 f dss mf
         new_hfs = map (bcApplyF0 f dss) hfs
     in
         Rep2 new_lf new_mf new_hfs


-- ==========================================================--
--
bcApplyF0_2 :: (FrontierElem -> FrontierElem) -> 
               [Domain] -> 
               Frontier -> 
               Frontier

bcApplyF0_2 f dss fr@(Min1Max0 ar f1 f0)
   = let new_f0 = map f f0
         new_f1 = []
     in
         Min1Max0 ar new_f1 new_f0


-- =================================================--
-- === Given embedding functor "e", function "f" ===--
-- === and closure functor "c", computes         ===--
-- === "e.f.c" (ie "Ge.f2.Fc", in accordance     ===--
-- === with Baraki's theory).                    ===--
-- =================================================--

-- ==========================================================--
--
bcEdotFdotC :: DRRSubst ->  -- binds domain variables to Points
               DExpr ->     -- target domain functor, "Ge"
               Domain ->    -- domain of "f2"
               Rep ->       -- the function "f2"
               [DExpr] ->   -- source domain functors, "F[c1 ... cn]"
               Route

bcEdotFdotC rho g_e fDomain@(Func fds fdt) f f_cs
   = let f_dot_c         = bcFdotC rho f_cs newDs fDomain f
         newDs           = map (bcEApp_d rho) f_cs
         fd_dot_c        = Func newDs fdt
         e_dot_f_dot_c   = bcEdotF rho g_e fd_dot_c f_dot_c
     in
         Rep e_dot_f_dot_c


-- ==========================================================--
--
bcGetR (d, r, t) = r
bcGetD (d, r, t) = d
bcGetT (d, r, t) = t


-- =========================================================--
-- === Do Baraki-style concretisation of function points ===--
-- =========================================================--

-- ==========================================================--
--
bcMakeInstance :: 
         Bool ->    -- True if use Baraki, False if use Conc
         Int ->     -- How hard to work (for Baraki)
         ACMode ->  -- Mode for Conc (IRRELEVANT)
         DExpr ->   -- simplest instance domain of point (DXFunc _ _)
         DSubst ->  -- binds domain vars to required instances
         Route ->   -- simplest instance of function
         Route      -- function at desired instance

-------------------------------------------------------------------
-- Function-valued objects case.                                 --
-------------------------------------------------------------------
-- Constraints, assumptions, &c.                                 --
-------------------------------------------------------------------
-- 1.  The function in question must be first-order.             --
-- 2.  The domain variables in the DExpr correspond exactly with --
--     those supplied in the DSubst.                             --
-- 3.  The DExpr is of the form (DXFunc _ _) and correspondingly --
--     the supplied Point is of the form (DFunc _ _, RFunc _),   --
--     and that the DFunc's args and DXFunc's args are           --
--     appropriately related.                                    --
-------------------------------------------------------------------
bcMakeInstance 
      use_baraki
      threshold
      s_or_l
      simplest_dirty@(DXFunc args_f_functors_dirty result_g_functor_dirty)
      rho_d
      f@(Rep fRep)
  = let
       ---------------------------
       -- Clean up the functors --
       ---------------------------

       simplest@(DXFunc args_f_functors result_g_functor)
          = bcClean simplest_dirty

       -----------------------------------------
       -- The domain of the simplest instance --
       -----------------------------------------

       simplestD = dxApplyDSubst_2 simplest 

       -----------------------------------------------
       -- the domain variables in the instantiation --
       -----------------------------------------------

       domainVarNames = map first rho_d

       --------------------------------------------------
       -- the domains corresponding to those variables --
       --------------------------------------------------

       bigInstanceDomains = map second rho_d

       -----------------------------------------------------
       -- Find out if we actually need to do anything.    --
       -- baseInstance will also be True if this is       --
       -- f is non-polymorphic, since rho_d will be empty --
       -----------------------------------------------------

       baseInstance = myAll (==Two) bigInstanceDomains

       -------------------------------------------------------
       -- find out if we can actually use Baraki's stuff.   --
       -- No function spaces in instantiations, and         --
       -- no non-top-level function spaces in the function. --
       -------------------------------------------------------

       barakiApplicable 
          = myAll (not.amContainsFunctionSpace)      bigInstanceDomains &&
                  (not.dxContainsFnSpace)            result_g_functor &&
            myAll (not.dxContainsSubsidiaryFnSpace)  args_f_functors

       -----------------------------------------------------
       -- Find out if we can use Baraki's first order     --
       -- optimisations.  This requires first order       --
       -- instantiations of a first order function.       --
       -- The former condition is true anyway if we're    --
       -- using Baraki's method.                          --
       -----------------------------------------------------

       canUseOpt
          = all (not.dxContainsFnSpace) args_f_functors

       -----------------------------------------------------------
       -- the set of points over which we have to take the meet --
       -----------------------------------------------------------

       bigInstancePoints 
          = let
                individualIndices 
                   = if     canUseOpt
                     then   map amMeetIRoutes bigInstanceDomains
                     else   map amAllRoutesMinusTopJONES bigInstanceDomains
                allIndices 
                   = myCartesianProduct individualIndices
            in 
                take (max 1 threshold) allIndices

       --------------------------------------
       -- The tops of the instance domains --
       --------------------------------------

       instanceTops = map avTopR bigInstanceDomains

       -------------------------------------------------------------
       -- all the bindings of domain variables to relevant points --
       -------------------------------------------------------------

       allRhos
          = let makeOneRho rs
                  = myZipWith4 (\n d r t -> (n, (d, r, t)))
                        domainVarNames bigInstanceDomains rs instanceTops
            in
                map makeOneRho bigInstancePoints

       ---------------------------------------------
       -- all of the e.f.[c1 ... cn] compositions --
       ---------------------------------------------

       all_edotfdotc
          = let makeOneEdotFdotC rho
                   = bcEdotFdotC rho result_g_functor simplestD 
                                 fRep args_f_functors
            in
                map makeOneEdotFdotC allRhos

       ----------------------------------------------------
       -- the domain of the function after instantiation --
       ----------------------------------------------------

       big_function_domain = dxApplyDSubst rho_d simplest

    in
       if        baseInstance
       then      f
       else if   not use_baraki || not barakiApplicable
       then      acMakeInstance s_or_l simplest rho_d f
       else      bmNorm big_function_domain (foldl1 bmGLB all_edotfdotc)


----------------------------------------
-- Non-function valued objects case.  --
-- Just use Conc.  In principle could --
-- use Baraki but I don't think this  --
-- is really worth bothering with.    --
----------------------------------------
bcMakeInstance
      use_baraki
      threshold
      s_or_l
      simplest
      rho_d
      f
   = acMakeInstance s_or_l simplest rho_d f


-- ==========================================================--
--
bcClean :: DExpr -> DExpr

bcClean DXTwo               = DXTwo
bcClean (DXLift1 [])        = DXTwo
bcClean (DXLift2 [])        = DXLift1 [DXTwo]
bcClean (DXLift1 dxs)       = DXLift1 (map bcClean dxs)
bcClean (DXLift2 dxs)       = DXLift2 (map bcClean dxs)
bcClean (DXVar v)           = DXVar v
bcClean (DXFunc dxss dxt)   = DXFunc (map bcClean dxss) (bcClean dxt)

-- ==========================================================--
-- === end                                  BarakiConc.hs ===--
-- ==========================================================--
