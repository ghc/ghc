
-- ==========================================================--
-- === Strictness analyser -- v6             StrictAn6.hs ===--
-- ==========================================================--

module StrictAn6 where
import BaseDefs
import Utils
import MyUtils
import BarakiConc3
import Constructors
import PrintResults
import AbstractVals2
import DomainExpr
import TExpr2DExpr
import AbstractMisc
import Inverse
import AbstractEval2
import Simplify
import FrontierGENERIC2
import SmallerLattice
import AbsConc3

import Data.List(transpose)
import Data.Char(isLower,isUpper)

-- ==========================================================--
-- Call analyser and format results
--
saMain :: AnnExpr Naam TExpr ->
          TypeDependancy ->
          AList Naam TExpr ->
          AList Naam [Naam] ->
          AList Naam (HExpr Naam) ->
          [TypeDef] ->
          [Flag] ->
          AList Domain Int ->
          [Char]

saMain typedTree typeDAR simplestTEnv freeVars builtins dataDefs flags table
   = let domaindTree
            = tx2dxAnnTree typeDAR typedTree        
         recGroups
            = saMkGroups domaindTree
         simplestDEnv
            = map2nd (tx2dx typeDAR) simplestTEnv
         simplestDs
            = map2nd dxApplyDSubst_2 simplestDEnv
         statics
            = (simplestDEnv, simplestDs, cargs, 
               freeVars, flags, (pLim, mLim, lLim, uLim, sRat), table)
         cargs
            = saMkCargs dataDefs
         mindless_inv
            = SimpleInv `elem` utSCflags statics
         use_baraki
            = NoBaraki `notElem` utSCflags statics
         saResult      
            = saUndoCAFkludge (saGroups statics builtins recGroups)
         setting_info
            = saSettingInfo pLim mLim lLim uLim sRat mindless_inv use_baraki
         result        
            = concat (map (saPrinter statics mindless_inv) saResult)
         pLim
            = case head (filter isP flags) of {PolyLim n -> n}
         mLim
            = case head (filter isM flags) of {MonoLim n -> n}
         lLim
            = case head (filter isL flags) of {LowerLim n -> n}
         uLim
            = case head (filter isU flags) of {UpperLim n -> n}
         sRat
            = case head (filter isS flags) of {ScaleUp n -> n}
         isP x
            = case x of {PolyLim _ -> True; _ -> False}
         isM x
            = case x of {MonoLim _ -> True; _ -> False}
         isL x
            = case x of {LowerLim _ -> True; _ -> False}
         isU x
            = case x of {UpperLim _ -> True; _ -> False}
         isS x
            = case x of {ScaleUp _ -> True; _ -> False}
     in
         if     ForceAll `notElem` flags
         then   setting_info ++ result
         else
         if     typedTree == typedTree       &&
                typeDAR == typeDAR           &&
                simplestTEnv == simplestTEnv &&
                freeVars == freeVars         &&
                builtins == builtins         &&
                dataDefs == dataDefs         &&
                flags == flags               &&
                table == table
         then   setting_info ++ result
         else   panic "saMain: Forcing failed."



-- ==========================================================--
--
saSettingInfo :: Int -> Int -> Int -> Int -> Int -> Bool -> Bool -> String

saSettingInfo pLim mLim lLim uLim sRat mindless_inv use_baraki
   = "\n================\n" ++
     "=== Settings ===\n" ++
     "================\n" ++
     "\nScaleup ratio = " ++ show sRat ++ "/10" ++
     "\nLower lattice size limit = " ++ show lLim ++
     "\nUpper lattice size limit = " ++ show uLim ++
     (if use_baraki then
     --"\nMonomorphic generalisation limit = " ++ show mLim ++
     "\nPolymorphic generalisation limit = " ++ show pLim
                    else
     "\nNot using Gebreselassie Baraki's generalisation technique.") ++
     (if mindless_inv then
     "\nUsing inefficient inverses" else "")
     ++ "\n\n\n" ++
     "==================\n" ++
     "=== Strictness ===\n" ++
     "==================\n"



-- ==========================================================--
--
saGroups :: StaticComponent -> 
            AList Naam (HExpr Naam) -> 
            DefnGroup (Naam, AnnExpr Naam DExpr) ->
            [SAInfo]

saGroups statics beta [] = []

{- New Idea. (or a return to an old idea?)
   Instead of remaking the HExpr's from the AnnExpr's on every
   fixpointing iteration, just do it at the start, and during fixpointing
   allow the system to plug in the appropriate current values.  This
   saves a lot of wasted effort and also allows us to do some
   optimisations on the HExpr's immediately after they are created.
   Assumption: in a recursive fn, all calls to self are done at the
   basic instance.
-}

{- Non recursive function binding.
   ===============================

   The current beta will contain bindings for all functions
   preceding this one.  This fn does not call itself, so we
   chuck it into "sa" with beta as it is, supplying none of the
   free vars.  Then optimise it.  Then knock it into a 
   frontier representation.
-}

saGroups statics beta ((False, [(defname, defrhs)]): rest)
   = let hrhs 
            = siVectorise (optFunc (sa statics beta defrhs))
         defDexpr
            = utSureLookup (utSCdexprs statics) "sa(1)" defname
         defDomain
            = saCAFkludge (utSureLookup (utSCdomains statics) "sa(2)" defname)
         optFunc
            = if Simp `elem` utSCflags statics then siSimplify else id
         show_hexprs
            = ShowHExpr `elem` utSCflags statics
         callSearchResult
            = saNonRecStartup statics defname defDomain hrhs
         route
            = saGetResult (last callSearchResult)
         betaAug 
            = [(defname, HPoint route)]
         restInfo
            = saGroups statics (betaAug++beta) rest
     in
         (if show_hexprs then [SAHExpr defname hrhs] else [])
         ++
         callSearchResult
         ++
         restInfo

         
{- Recursive function binding.
   ===========================

   This is not so simple.  As before, beta as supplied contains
   bindings for all functions preceding this group.  When we call
   "sa", we cannot substitute anything for recursive calls because
   this needs to be done dynamically by the fixpointer.  So again, we
   call "sa" with beta as supplied, then stuff the resultants through
   the optimiser.

   Subsequently we make up some initial approximations for these things
   and hand over the problem to the fixpointer.
-}

saGroups statics beta ((True, defs):rest)
   = let defNames
            = map first defs
         defRhss
            = map second defs
         hrhss
            = map (siVectorise.optFunc.sa statics beta) defRhss
         defDexprs
            = map (utSureLookup (utSCdexprs statics) "sa(3)") defNames
         defDomains
            = map (utSureLookup (utSCdomains statics) "sa(4)") defNames
         callFixResult
            = saFixStartup statics defNames
                           (map saCAFkludge defDomains) hrhss
         fixpoints
            = map saGetResult (filter saIsResult callFixResult)
         betaAug
            = myZip2 defNames (map HPoint fixpoints)
         optFunc 
            = if Simp `elem` utSCflags statics then siSimplify else id
         show_hexprs
            = ShowHExpr `elem` utSCflags statics
         restinfo
            = saGroups statics (betaAug++beta) rest
     in  
         (if show_hexprs then myZipWith2 SAHExpr defNames hrhss else [])
         ++
         callFixResult
         ++
         restinfo



-- ==========================================================--
--
saFixStartup :: StaticComponent ->
                [Naam] ->             -- names of fns in groups
                [Domain] ->           -- final domains of functions
                [HExpr Naam] ->       -- trees
                [SAInfo]
saFixStartup
     statics
     names
     domains
     trees
   =
     let
         final_arg_dss
            = map saGetArgs domains
         (poly_limit, mono_limit, low_limit, high_limit, scale_ratio)
            = utSClims statics
         sequence
            = slMakeSequence (utSCsizes statics) scale_ratio
                             final_arg_dss low_limit high_limit
         init_arg_dss
            = map second (saGetNextRec sequence)
         targ_ds
            = map saGetRes domains
         init_domains
            = myZipWith2 saMkFunc init_arg_dss targ_ds
         final_domains
            = myZipWith2 saMkFunc final_arg_dss targ_ds
         safe_and_live_bottoms
            = map avBottomR init_domains
         result 
            = saFixMain statics
                        names
                        sequence
                        init_arg_dss
                        targ_ds
                        final_arg_dss
                        safe_and_live_bottoms
                        safe_and_live_bottoms
                        trees
                        0
         local_commentary
            = saMakeSizeInfo sequence names
     in
         local_commentary
         ++
         result



-- ==========================================================--
--
saNonRecStartup :: StaticComponent ->
                   Naam ->             -- name of fn
                   Domain ->           -- final domain of function
                   HExpr Naam ->       -- tree
                   [SAInfo]
saNonRecStartup
     statics
     name
     domain
     tree
   =
     let
         final_arg_ds
            = saGetArgs domain
         (poly_limit, mono_limit, low_limit, high_limit, scale_ratio)
            = utSClims statics
         sequence
            = slMakeSequence (utSCsizes statics) scale_ratio
                             [final_arg_ds] low_limit high_limit
         init_arg_ds
            = second (saGetNextNonRec sequence)
         targ_d
            = saGetRes domain
         init_domain
            = saMkFunc init_arg_ds targ_d
         final_domains
            = saMkFunc final_arg_ds targ_d
         max0_init_safe
            = avBottomR init_domain
         min1_init_live
            = avTopR init_domain
         local_commentary
            = saMakeSizeInfo sequence [name]
         result
            = saNonRecSearch statics
                             name
                             sequence
                             init_arg_ds
                             targ_d
                             final_arg_ds
                             max0_init_safe
                             min1_init_live
                             tree
     in
         local_commentary
         ++
         result



-- ==========================================================--
--
saNonRecSearch :: StaticComponent ->
                  Naam ->               -- name of fn
                  Sequence ->           -- sequence
                  [Domain] ->           -- prev arg domains
                  Domain ->             -- target domain
                  [Domain] ->           -- final arg domains
                  Route ->              -- max1 initialiser
                  Route ->              -- min0 initialiser
                  HExpr Naam ->         -- the tree
                  [SAInfo]
saNonRecSearch
     statics
     name
     sequence
     old_arg_ds
     targ_d
     final_arg_ds
     old_safe_abstraction
     old_live_abstraction
     tree
   = 
     let
         finished_after_this_search 
            = saSequenceIsEmpty (saGetSeqTail sequence)
         given_up_early
            = saGivenUpEarly sequence
         (size, curr_arg_ds)
            = saGetNextNonRec sequence
         given_up_early_result
            = head (saFinalExpansion statics
                                     [final_domain] 
                                     [old_domain] 
                                     [old_safe_abstraction])
         done_result
            = if     given_up_early
              then   [SAGiveUp [name],
                      SAResult name final_domain given_up_early_result]
              else   [SAResult name final_domain next_safe]
         curr_domain
            = saMkFunc curr_arg_ds targ_d
         final_domain
            = saMkFunc final_arg_ds targ_d
         old_domain
            = saMkFunc old_arg_ds targ_d
         curr_safe_initialiser
            = acConc Live curr_domain old_domain old_safe_abstraction {-Live safe-}
         curr_live_initialiser
            = acConc Safe curr_domain old_domain old_live_abstraction {-Safe live-}
         (next_safe, next_safe_evals)
            = fsMakeFrontierRep Safe False 
                                tree
                                curr_domain
                                final_arg_ds
                                curr_live_initialiser
                                curr_safe_initialiser
         (next_live, next_live_evals)
            = fsMakeFrontierRep Live False 
                                tree
                                curr_domain
                                final_arg_ds
                                curr_live_initialiser
                                curr_safe_initialiser
         local_commentary
            = [SASearch Safe name size next_safe_evals,
               SASearch Live name size next_live_evals]
         not_done_result
            = saNonRecSearch statics 
                             name 
                             (saGetSeqTail sequence)
                             curr_arg_ds 
                             targ_d 
                             final_arg_ds 
                             next_safe 
                             next_live 
                             tree
     in
         if     finished_after_this_search
         then   local_commentary ++ done_result
         else   local_commentary ++ not_done_result



-- ==========================================================--
--
saFixMain :: StaticComponent ->
             [Naam] ->               -- names of fns in group
             Sequence ->             -- expansion sequence for each function
             [[Domain]] ->           -- previous argument domains
             [Domain] ->             -- target domains of functions
             [[Domain]] ->           -- final argument domains
             [Route] ->              -- safe abstractions in a previous lattice
             [Route] ->              -- live abstractions in a previous lattice
             [HExpr Naam] ->         -- trees
             Int ->
             [SAInfo]                -- final result ?!?!

saFixMain
     statics
     names
     sequences
     prev_arg_dss
     targ_ds
     final_arg_dss
     prev_safe
     prev_live
     trees
     lev
   =
     let
         finished
            = saSequenceIsEmpty sequences
         gave_up_early
            = saGivenUpEarly sequences
         curr_arg_dss
            = map second (saGetNextRec sequences)
         sizes_here
            = map first (saGetNextRec sequences)
         prev_domains
            = myZipWith2 saMkFunc prev_arg_dss targ_ds
         curr_domains
            = myZipWith2 saMkFunc curr_arg_dss targ_ds
         curr_safe
            = myZipWith3 (acConc Safe) curr_domains prev_domains prev_safe
         curr_live
            = myZipWith3 (acConc Live) curr_domains prev_domains prev_live
         max0_init
            = curr_live 
              --myZipWith3 (acConc Live) 
              --curr_domains prev_domains prev_live {-Live safe-}
         min1_init
            = curr_safe
              --myZipWith3 (acConc Safe) 
              --curr_domains prev_domains prev_safe {-Safe live-}
         thisSizeInfo
            = saFixAtSizeLive statics
                              curr_live
                              names
                              curr_domains
                              final_arg_dss
                              targ_ds
                              trees
                              min1_init
                              max0_init
                              sizes_here
                              lev
         (safe_fixes_at_this_size, live_fixes_at_this_size)
            = case last thisSizeInfo of SASL ss ls -> (ss, ls)
         final_domains
            = myZipWith2 saMkFunc final_arg_dss targ_ds
         finished_result
            = (if gave_up_early then [SAGiveUp names] else []) ++
              myZipWith3 SAResult names final_domains
                         (if     gave_up_early
                          then   finished_fixes_gave_up_early
                          else   prev_safe)
         finished_fixes_gave_up_early
            = saFinalExpansion statics
                               final_domains
                               prev_domains
                               prev_safe
         not_finished_result
            = init thisSizeInfo ++
              saFixMain statics
                        names
                        (saGetSeqTail sequences)
                        curr_arg_dss
                        targ_ds
                        final_arg_dss
                        safe_fixes_at_this_size
                        live_fixes_at_this_size
                        trees
                        (lev+1)
     in
         if     finished
         then   finished_result
         else   not_finished_result



-- ==========================================================--
--
saFixAtSizeLive :: StaticComponent ->
                   [Route] ->            -- live abstractions
                   [Naam] ->             -- names of fns in group
                   [Domain] ->           -- current domains of functions
                   [[Domain]] ->         -- arg domains at full size
                   [Domain] ->           -- target domains
                   [HExpr Naam] ->       -- the trees
                   [Route] ->            -- safe min1 inits (const for this latt)
                   [Route] ->            -- live max0 inits (const for this latt)
                   [Int] ->              -- size of arg lattices
                   Int ->
                   [SAInfo]              -- safe and live abstractions of fixpoint
saFixAtSizeLive
     statics
     live_abstractions
     names
     curr_domains
     big_argdss
     targ_ds
     trees
     min1_init
     max0_init
     sizes
     lev
   =
     let
         big_domains
            = myZipWith2 saMkFunc big_argdss targ_ds
         big_live_abstractions
            = myZipWith3 (acConc Live) big_domains curr_domains live_abstractions
         curr_live_beta 
            = myZip2 names big_live_abstractions
         trees_live 
            = map (saHSubst curr_live_beta) trees
         next_live_with_evals
            = myZipWith5 (fsMakeFrontierRep Live (lev==0))
                         trees_live
                         curr_domains
                         big_argdss
                         min1_init
                         live_abstractions --max0_init
         (next_live, next_live_evals) 
            = unzip2 next_live_with_evals
         got_fixed_point
            = myAndWith2 (\a b -> a == b) next_live live_abstractions
         fixed_point_result
            = work_here_commentary ++
              saFixAtSizeSafe statics
                              next_live
                              next_live
                              names
                              curr_domains
                              big_argdss
                              targ_ds
                              trees
                              min1_init
                              max0_init
                              sizes
                              lev
         work_here_commentary
            = myZipWith3 (SASearch Live) names sizes next_live_evals
         not_fixed_point_result
            = work_here_commentary ++              
              saFixAtSizeLive statics
                              next_live
                              names
                              curr_domains
                              big_argdss
                              targ_ds
                              trees
                              min1_init
                              max0_init
                              sizes
                              lev
     in
         if     got_fixed_point
         then   fixed_point_result
         else   not_fixed_point_result



-- ==========================================================--
--
saFixAtSizeSafe :: StaticComponent ->
                   [Route] ->            -- safe abstractions
                   [Route] ->            -- live abstractions
                   [Naam] ->             -- names of fns in group
                   [Domain] ->           -- current domains of functions
                   [[Domain]] ->         -- arg domains at full size
                   [Domain] ->           -- target domains
                   [HExpr Naam] ->       -- the trees
                   [Route] ->            -- safe min1 inits (const for this latt)
                   [Route] ->            -- live max0 inits (const for this latt)
                   [Int] ->              -- size of arg lattices
                   Int ->
                   [SAInfo]              -- safe and live abstractions of fixpoint
saFixAtSizeSafe
     statics
     safe_abstractions
     live_fixes
     names
     curr_domains
     big_argdss
     targ_ds
     trees
     min1_init
     max0_init
     sizes
     lev
   =
     let
         big_domains
            = myZipWith2 saMkFunc big_argdss targ_ds
         big_safe_abstractions
            = myZipWith3 (acConc Safe) big_domains curr_domains safe_abstractions
         curr_safe_beta 
            = myZip2 names big_safe_abstractions
         trees_safe 
            = map (saHSubst curr_safe_beta) trees
         next_safe_with_evals
            = myZipWith5 (fsMakeFrontierRep Safe (lev==0))
                         trees_safe
                         curr_domains
                         big_argdss
                         min1_init --safe_abstractions
                         safe_abstractions --live_fixes --max0_init
         (next_safe, next_safe_evals)
            = unzip2 next_safe_with_evals
         got_fixed_point
            = myAndWith2 (\a b -> a == b) next_safe safe_abstractions
         fixed_point_result
            = work_here_commentary ++
              [SASL safe_abstractions live_fixes]
         work_here_commentary
            = myZipWith3 (SASearch Safe) names sizes next_safe_evals
         not_fixed_point_result
            = work_here_commentary ++              
              saFixAtSizeSafe statics
                              next_safe
                              live_fixes
                              names
                              curr_domains
                              big_argdss
                              targ_ds
                              trees
                              min1_init
                              max0_init
                              sizes
                              lev
     in
         if     got_fixed_point
         then   fixed_point_result
         else   not_fixed_point_result



-- ==========================================================--
--
saFinalExpansion :: StaticComponent -> 
                    [Domain] ->
                    [Domain] ->
                    [Route] ->
                    [Route]
saFinalExpansion
     statics
     final_domains
     curr_domains
     safe_abstractions
   =
     let
        use_baraki
           = False --NoBaraki `notElem` (utSCflags statics)
        (poly_limit, mono_limit, lower_limit, upper_limit, scale_ratio)
           = utSClims statics
        (dexprs, dsubsts)
           = unzip2 (myZipWith2 dxDiff final_domains curr_domains)
        result
           = myZipWith3 (bcMakeInstance use_baraki mono_limit Safe)
                        dexprs dsubsts safe_abstractions
     in
        result


-- ==========================================================--
--
saIsResult :: SAInfo -> Bool

saIsResult (SAResult _ _ _)  = True
saIsResult anyElse           = False

saGetResult (SAResult name domain route) = route


-- ==========================================================--
--
saPrinter :: StaticComponent -> Bool -> SAInfo -> [Char]

saPrinter statics mi (SAResult name domain route)
   = prPrintFunction mi statics name (domain, route)

saPrinter statics mi (SASearch mode name size n)
   = "Evaluated at size " ++
     rjustify 7 (show size) ++
     " using " ++
     rjustify 4 (show n) ++
     " evals " ++
     (case mode of {Safe -> "safe"; Live -> "live"}) ++
     " \"" ++ name ++ "\"\n"

saPrinter statics mi (SASizes name useSizes noUseSizes)
   = "\nDomains for \"" ++ name ++ "\" are\n" ++ 
     saPrinter_aux True useSizes ++ saPrinter_aux False noUseSizes ++ "\n"

saPrinter statics mi (SAHExpr name tree)
   = "\nAbstract tree for \"" ++ name ++ "\" is\n\n" ++ show tree ++ "\n\n"

saPrinter statics mi (SAGiveUp names)
   = "Giving up on " ++ 
     interleave " and " (map (\n -> "\"" ++ n ++ "\"") names) ++ 
     ".\n"


saPrinter_aux use [] 
   = ""
saPrinter_aux use ((s,ds):sds)
   = rjustify 8 (show s) ++ " " ++
     (if use then " " else "*") ++ " " 
     ++ show ds ++ "\n" ++ saPrinter_aux use sds


-- ==========================================================--
--
saUndoCAFkludge :: [SAInfo] -> [SAInfo]

saUndoCAFkludge []
   = []
saUndoCAFkludge (saInfo:saInfos)
   = let rest
            = saUndoCAFkludge saInfos
         this
            = case saInfo of
                 SAResult name domain route
                    -> [SAResult name (saCAFkludgeInverse domain) route]
                 SASearch mode name size n
                    -> if size < 2 then [] else [saInfo]
                 SASizes name [(sizes,[])] []
                    -> []
                 SASizes name useSizes noUseSizes
                    -> [saInfo]
                 SAHExpr name tree
                    -> [saInfo]
                 SAGiveUp names
                    -> [saInfo]
     in
         this ++ rest


-- ==========================================================--
--
saCAFkludge, saCAFkludgeInverse :: Domain -> Domain

saCAFkludge (Func dss dt) = Func dss dt
saCAFkludge non_func_dom  = Func []  non_func_dom

saCAFkludgeInverse (Func []  dt) = dt
saCAFkludgeInverse (Func dss dt) = Func dss dt
saCAFkludgeInverse non_fn_dom    = non_fn_dom


-- ==========================================================--
--
saMkFunc :: [Domain] -> Domain -> Domain

saMkFunc []  dt = dt
saMkFunc dss dt = Func dss dt


-- ==========================================================--
--
saSequenceIsEmpty (use, noUse)       = null use
saGetNextRec      ((u:us), noUse)    = u
saGetNextNonRec   (([u]:us), noUse)  = u
saGetSeqTail      (u:us, noUse)      = (us, noUse)
saGivenUpEarly    (use, noUse)       = not (null noUse)


-- ==========================================================--
--
saGetArgs (Func dss dt) = dss
saGetRes  (Func dss dt) = dt


-- ==========================================================--
--
saMakeSizeInfo :: Sequence -> [Naam] -> [SAInfo]

saMakeSizeInfo (use, noUse) names
   = let useT = transpose use
         noUseT 
            = transpose noUse
         noUseT2 = (if null noUse then [[] | _ <- useT] else noUseT)
     in
         myZipWith3 SASizes names useT noUseT2


-- ==========================================================--
--
saHSubst :: RSubst ->
            HExpr Naam ->
            HExpr Naam

saHSubst fenv (HVar v@('_':_))  = HPoint (utSureLookup fenv "sa(8)" v)
saHSubst fenv (HVar v_other)    = HVar v_other
saHSubst fenv (HApp e1 e2)      = HApp (saHSubst fenv e1) (saHSubst fenv e2)
saHSubst fenv (HMeet es)        = HMeet (map (saHSubst fenv) es)
saHSubst fenv (HLam vs e)       = HLam vs (saHSubst fenv e)
saHSubst fenv (HPoint p)        = HPoint p
saHSubst fenv (HTable t)        = HTable (map2nd (saHSubst fenv) t)
saHSubst fenv (HVAp f es)       = HVAp (saHSubst fenv f) (map (saHSubst fenv) es)


-- ==========================================================--
--
saMkGroups :: AnnExpr Naam DExpr -> 
            DefnGroup (AnnDefn Naam DExpr)

saMkGroups (_, ALet rf subdefs rest) = (rf, subdefs):saMkGroups rest
saMkGroups (_, anyThingElse        ) = []


-- ==========================================================--
-- The strictness analyser proper: the magic function "S"
-- Now rather heavily modified (in version 0.300 and above)
-- and no longer bearing much relationship to the original
-- mathematics
--
sa :: StaticComponent ->
      AList Naam (HExpr Naam) ->
      AnnExpr Naam DExpr ->
      HExpr Naam

sa statics beta (dtau, AConstr _) 
   = panic "sa: AConstr encountered"

sa statics beta (dtau, ALet _ _ _)
   = panic "sa: ALet encountered"

sa statics beta (dtau, ANum n) 
   = HPoint One

sa statics beta (dtau, AAp e1 e2) 
   = HApp (sa statics beta e1) (sa statics beta e2)

sa statics beta (dtau, ALam vs e)
   = HLam vs (sa statics beta e)

sa statics beta (dtau, AVar v)
   {- This is complicated.  If it's a constructor, make up the
      constructor at the right instantiation and put in place.
      If it's a function which is accounted for in beta, do likewise.
      If it's a function which is not accounted for in beta, ignore it,
      since it must be a call to the current recursive group.
      If it's a variable, look it up in beta, and if it isn't there, 
      just leave alone.  Otherwise replace.  This allows the
      case-statement-algorithm to work properly.
   -}
   = let isConstructor
            = isUpper (head v)
         isVariable
            = isLower (head v)
         isFunction
            = head v == '_'
         v_dtype_simple
            = utSureLookup (utSCdexprs statics) "sa(5)" v
         v_instance
            = txGetInstantiations v_dtype_simple dtau
         v_lookup
            = utLookup beta v
         accounted_for
            = case v_lookup of {Just _ -> True; _ -> False}
         v_lookup_result
            = case v_lookup of {Just x -> x}
         v_lookup_point
            = case v_lookup_result of {HPoint p -> p}
         use_baraki
            = NoBaraki `notElem` (utSCflags statics)
         (pLim, mLim, lLim, uLim, scale_ratio)
            = utSClims statics
         f_at_instance 
            = bcMakeInstance use_baraki pLim Safe
                             v_dtype_simple v_instance v_lookup_point
         mindless_inv
            = SimpleInv `elem` (utSCflags statics)
         c_at_instance
            = coMakeConstructorInstance
                 mindless_inv
                 (utSureLookup (utSCconstrelems statics) "sa(7)" v)
                 v_dtype_simple v_instance
     in
         if    isConstructor
         then  HPoint c_at_instance
         else
         if    isVariable && accounted_for
         then  v_lookup_result
         else
         if    isVariable && not accounted_for
         then  HVar v
         else  
         if    isFunction && accounted_for
         then  HPoint f_at_instance
         else
         if    isFunction && not accounted_for
         then  HVar v
         else  panic "sa(var)"



sa statics beta (dtau, ACase (dtau_sw, expr_sw) alts)
   {- This is even more complicated.
      Get all the constructors in case.
      Make them all up at the relevant instance.
      Make all the points in dtau_sw.
      For each one, gather the maxinverses and constructors
         which give that point.  For each of these, make up an
         environment to augment beta with, "sa" the relevant
         alternative with that value and HMeet all the values
         together (yuck).
   -}
   = let 
         ----------------------------------------------------------
         -- check for special case of case-ing on a known value  --
         ----------------------------------------------------------

         caseOfKnownVal
            = case expr_sw of
                AVar v_sw -> isLower (head v_sw) && 
                             v_sw `elem` map first beta
                anyElse   -> False

         v_sw_pt = case utSureLookup beta "sa(??)" 
                        (case expr_sw of AVar v_sw -> v_sw)
                   of HPoint p -> p

         doCaseOpt = NoCaseOpt `notElem` (utSCflags statics)

         mindless_inv = SimpleInv `elem` (utSCflags statics)

         ----------------------------------------------------------
         -- to do with domains, and misc stuff                   --
         ----------------------------------------------------------

         sw_domain = dxApplyDSubst_2 dtau_sw

         all_sw_points = amAllRoutes sw_domain

         dtau_sw_top = avTopR sw_domain

         outDomainBottom = HPoint (avBottomR (dxApplyDSubst_2 dtau))

         unMkFrel (MkFrel xs) = xs

         ----------------------------------------------------------
         -- make a load of info about the alts                   --
         ----------------------------------------------------------

         constructorNames = map first alts

         constrSimpDTypes = map (utSureLookup (utSCdexprs statics) "sa(9)") 
                                constructorNames

         constrSimpDFinal = let getDxt (DXFunc _ dxt) = dxt
                                getDxt other_dx = other_dx
                            in  map getDxt constrSimpDTypes

         constrInstances  = map (\si -> txGetInstantiations si dtau_sw) 
                                constrSimpDFinal

         constrDomains = myZipWith2 dxApplyDSubst 
                         constrInstances constrSimpDTypes

         constrCElems     = map (utSureLookup (utSCconstrelems statics) "sa(10)") 
                            constructorNames

         constrActuals = myZipWith3 (coMakeConstructorInstance mindless_inv)
                         constrCElems constrSimpDTypes constrInstances

         conIsCAF con = case con of { Rep _ -> False; _ -> True}

         allConstrNumbers = 0 `myIntsFromTo` (length alts - 1)

         allAltInfo          
            = [(constrActuals ## n,             -- the constructor itself
                constrDomains ## n,             -- the constructor's domain
                conIsCAF (constrActuals ## n),  -- is-a-caf flag
                first (second (alts ## n)),     -- arguments on this alt
                second (second (alts ## n)))    -- rhs for this alt
                | n <- allConstrNumbers]

         ----------------------------------------------------------
         -- the maxInverse of a constructor at a point           --
         ----------------------------------------------------------

         maxInvsCon con cd isCAF pt
            = if     isCAF
              then   if pt == dtau_sw_top then [[]] else []
              else   map unMkFrel (inMaxInverse mindless_inv cd con pt)

         ----------------------------------------------------------
         -- make the table mapping switch expression definedness --
         -- to definedness of the entire case expression, OR,    --
         -- if we can do case-of-case optimisation, just compute --
         -- rhs-definedness based on the known value (v_sw_pt)   --
         -- of the switch expression.                            --
         ----------------------------------------------------------

         switch_hexpr = sa statics beta (dtau_sw, expr_sw)

         result
            = if     caseOfKnownVal && doCaseOpt
              then   second (outval v_sw_pt)
              else   HApp (HTable (map outval all_sw_points)) switch_hexpr

         ----------------------------------------------------------
         -- given a value for the switch expression, finds the   --
         -- definedness of the entire case expression (outval)   --
         ----------------------------------------------------------

         outval r
           = (r, aeMkMeet outDomainBottom (concat (map (f r) allConstrNumbers)))

         f pt cnum
           = let (con, cd, isCAF, params, rhs) = allAltInfo ## cnum
                 mis = map (map HPoint) (maxInvsCon con cd isCAF pt)
                 allenvs = map (myZip2 params) mis
                 doOneRhs :: [(Naam, HExpr Naam)] -> HExpr Naam
                 doOneRhs env = sa statics (env++beta) rhs
             in
                 (map doOneRhs allenvs) :: [HExpr Naam]

         ----------------------------------------------------------
         --                                                      --
         ----------------------------------------------------------
     in
         result


-- ==========================================================--
--
saMkCargs :: [TypeDef] -> AList Naam [ConstrElem]

saMkCargs [] = []
saMkCargs ((typename, tvars, calts):rest)
   = map doOne calts ++ saMkCargs rest
     where
        doOne (name, tdefexprs) = (name, map f tdefexprs)
        f (TDefVar v) = ConstrVar (find v tvars)
        f (TDefCons _ _) = ConstrRec
        find v (v2:vs) = if v == v2 then 0 else 1 + find v vs


-- ==========================================================--
-- === End                                   StrictAn6.hs ===--
-- ==========================================================--
