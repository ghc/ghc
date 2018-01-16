
-- ==========================================================--
-- === Constructor functions                              ===--
-- ===                                    Constructors.hs ===--
-- ==========================================================--

module Constructors where
import BaseDefs
import Utils
import MyUtils
import DomainExpr
import AbstractVals2
import SuccsAndPreds2
import AbstractMisc
import Inverse
import Apply

-- ==========================================================--
--
coMakeConstructorInstance :: Bool ->         -- True == use mindless inverse
                             [ConstrElem] -> -- tells about constructor args
                             DExpr ->        -- simplest instance expression
                             DSubst ->       -- domain of use
                             Route

coMakeConstructorInstance mi cargs simplest_init usage
   = let
        ----------------------------------------------------------------
        -- Find out whether the constructor has zero arity, and       --
        -- prepare a relevant domain expression for it.               --
        ----------------------------------------------------------------

        (doCAFkludge, simplest)
           = case simplest_init of
               dx@(DXFunc _ _) -> (False, dx)
               dx_CAF          -> (True, DXFunc [] dx_CAF)

        ----------------------------------------------------------------
        -- Find out if it is a recursive type.                        --
        ----------------------------------------------------------------

        recursive 
           = case simplest of
                DXFunc _  (DXLift1 _) -> False
                DXFunc _  (DXLift2 _) -> True
                anythingElse -> panic "coMakeConstructorInstance:recursive"

        actual
           = dxApplyDSubst usage simplest

        (actualSources, actualTarget)
           = case actual of Func dss dt -> (dss, dt)

        ----------------------------------------------------------------
        --                                                            --
        ----------------------------------------------------------------

        (target_domain_products, points_below_structure_point)
           = case (recursive, actualTarget) of
                (True,  Lift2 dts)    -> (dts,              [Stop2, Up2])
                (True,  Lift1 [Two])  -> (panic "cMCI(1)",  [Stop1, Up1 [Zero]])
                (False, Lift1 dts)    -> (dts,              [Stop1])
                (False, Two)          -> (panic "cMCI(2)",  [Zero])

        all_product_points
           = myCartesianProduct (map amAllRoutes target_domain_products)

        points_not_below_structure_point
           = case (recursive, actualTarget) of
                (True,  Lift2 dts)    -> map UpUp2 all_product_points
                (True,  Lift1 [Two])  -> [Up1 [One]]
                (False, Lift1 dts)    -> map Up1   all_product_points
                (False, Two)          -> [One]

        tagTable
           = [(p, arg_bottoms) 
             | p <- points_below_structure_point] ++
             [(p, [MkFrel (map (magic p) cargs)])
             | p <- points_not_below_structure_point]

        arg_bottoms
           = [MkFrel (map avBottomR actualSources)]

        ----------------------------------------------------------------
        --                                                            --
        ----------------------------------------------------------------

        magic p ConstrRec       = p
        magic p (ConstrVar n)   = xpts p ## n

        xpts p 
           | recursive   = case p of UpUp2 rs -> rs
           | otherwise   = case p of Up1 rs   -> rs

        ----------------------------------------------------------------
        --                                                            --
        ----------------------------------------------------------------

     in
        if    doCAFkludge
        then  apPapConst (coCGen_aux mi tagTable actual)
        else  Rep        (coCGen_aux mi tagTable actual)


-- ==========================================================--
--
coCGen_aux :: Bool ->
              AList Route [FrontierElem] -> -- the tag/value table
              Domain ->                     -- domain of the function to be made
              Rep

coCGen_aux mi tt (Func dss Two) 
   = let f1 = sort (utSureLookup tt "coCGen_aux(1)" One)
         f0 = spMax0FromMin1 dss f1
         ar = case head (f1 ++ f0) of MkFrel fels -> length fels
     in  RepTwo (Min1Max0 ar f1 f0)

coCGen_aux mi tt (Func dss (Lift1 dts))
   = let lf_f1 = sort (utSureLookup tt "coCGen_aux(2)" (Up1 (map avBottomR dts)))
         lf_f0 = spMax0FromMin1 dss lf_f1
         lf_ar = length dss
         newtt = [(rs, fels) | (Up1 rs, fels) <- tt]
     in
         Rep1 (Min1Max0 lf_ar lf_f1 lf_f0) 
              (coCGen_aux_cross mi newtt dss dts)

coCGen_aux mi tt (Func dss (Lift2 dts))
   = let lf_f1 = sort (utSureLookup tt "coCGen_aux(2)" Up2)
         lf_f0 = spMax0FromMin1 dss lf_f1
         mf_f1 = sort (utSureLookup tt "coCGen_aux(3)" (UpUp2 (map avBottomR dts)))
         mf_f0 = spMax0FromMin1 dss mf_f1
         lf_ar = length dss
         newtt = [(rs, fels) | (UpUp2 rs, fels) <- tt]
     in
         Rep2 (Min1Max0 lf_ar lf_f1 lf_f0)
              (Min1Max0 lf_ar mf_f1 mf_f0)
              (coCGen_aux_cross mi newtt dss dts)

coCGen_aux mi tt (Func dss gDomain@(Func dss2 dt))
   = let newtt = map makenewtt (amAllRoutes dt)
         makenewtt x
            = (x, 
               avMinfrel [MkFrel (xs++ys) 
                          | (g, min_args_to_get_g) <- tt,
                            MkFrel xs <- min_args_to_get_g,
                            MkFrel ys <- inMinInverse mi gDomain g x] )
               -- *** don't know if the avMinfrel is really necessary *** --
     in  coCGen_aux mi newtt (Func (dss++dss2) dt)


-- ==========================================================--
--
coCGen_aux_cross :: Bool -> 
                    AList [Route] [FrontierElem] -> 
                    [Domain] -> 
                    [Domain] -> 
                    [Rep]

coCGen_aux_cross mi tt dss dts
   = let numberOfDimensions
            = length dts
         doOneDimension n
            = coCGen_aux mi (fixtt n) (Func dss (dts ## n))
                              --- ** DENORMALISATION ** ---
         fixtt n
            = let thisDimPoints 
                     = taddall [] tt

                  taddall acc []
                     = acc
                  taddall acc ((rs,fel):rest)
                     = taddall (tadd (rs ## n) fel acc) rest

                  tadd :: Route -> 
                          [FrontierElem] -> 
                          AList Route [[FrontierElem]] ->
                          AList Route [[FrontierElem]]
                  tadd r fel []
                     = [(r, [fel])]
                  tadd r fel (this@(rr, fels):rest)
                     | r == rr    = (rr, fel:fels):rest
                     | otherwise  = this : tadd r fel rest

                  fixedtt 
                     = map2nd 
                          (foldr avLUBmin1frontier [MkFrel (map avTopR dss)])
                          thisDimPoints
              in 
                  fixedtt
     in  
         map doOneDimension (0 `myIntsFromTo` (numberOfDimensions-1))




-- ==========================================================--
-- === end                                Constructors.hs ===--
-- ==========================================================--
