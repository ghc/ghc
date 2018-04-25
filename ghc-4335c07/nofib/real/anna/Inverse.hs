
-- ==========================================================--
-- === Computes inverses of function applications.        ===--
-- ===                                         Inverse.hs ===--
-- ==========================================================--

module Inverse where
import BaseDefs
import Utils
import MyUtils
import AbstractVals2
import SuccsAndPreds2
import AbstractMisc
import Apply


-- ==========================================================--
--
inMinInverse :: Bool -> Domain -> Route -> Route -> [FrontierElem]

inMinInverse mindless fDomain (Rep f) res
   | mindless   = second (inMMI_mindless fDomain f res)
   | otherwise  = second (inMMI fDomain f res)


-- ==========================================================--
--
inMaxInverse :: Bool -> Domain -> Route -> Route -> [FrontierElem]

inMaxInverse mindless fDomain (Rep f) res
   | mindless   = first (inMMI_mindless fDomain f res)
   | otherwise  = first (inMMI fDomain f res)


-- ==========================================================--
--
inMMI_mindless :: Domain -> Rep -> Route -> ([FrontierElem], [FrontierElem])

inMMI_mindless (Func dss dt) f a 
   = let totalInverseImage = inInverse_mindless dss f a
     in (avMaxfrel totalInverseImage, avMinfrel totalInverseImage)


-- ==========================================================--
--
inNormalise :: [FrontierElem] -> 
               [FrontierElem] -> 
               ([FrontierElem], [FrontierElem])

inNormalise max min
   = let new_max = filter (`avAboveMin1frel`     min) max
         new_min = filter (`avBelowMax0frel` new_max) min
     in
         (new_max, new_min)


-- ==========================================================--
--
inIntersect :: ([FrontierElem], [FrontierElem]) ->
               ([FrontierElem], [FrontierElem]) ->
               ([FrontierElem], [FrontierElem])

inIntersect (max1, min1) (max2, min2)
   = let new_max = avMaxfrel [ x `avGLBfrel` y | x <- max1, y <- max2 ]
         new_min = avMinfrel [ x `avLUBfrel` y | x <- min1, y <- min2 ]
     in  inNormalise new_max new_min


-- ==========================================================--
--
inMMI :: Domain -> Rep -> Route -> ([FrontierElem], [FrontierElem])

inMMI (Func dss dt) (RepTwo (Min1Max0 ar f1 f0)) Zero
   = (f0,
      if null f0 then [] else [MkFrel (map avBottomR dss)])

inMMI (Func dss Two) (RepTwo (Min1Max0 ar f1 f0)) One
   = (if null f1 then [] else [MkFrel (map avTopR dss)],
      f1)

inMMI (Func dss (Lift1 dts)) (Rep1 (Min1Max0 ar lf_f1 lf_f0) hfs) Stop1
   = (lf_f0, 
      if null lf_f0 then [] else [MkFrel (map avBottomR dss)])

-- special case because this happens extremely frequently
inMMI (Func dss (Lift1 [dt])) (Rep1 (Min1Max0 ar lf_f1 lf_f0) [hf]) (Up1 [r])
   = let (hf_maxI, hf_minI) = inMMI (avUncurry dss dt) hf r
         min2 = avMinfrel [ x `avLUBfrel` y | x <- hf_minI, y <- lf_f1 ]
     in  inNormalise hf_maxI min2

inMMI (Func dss (Lift1 dts)) (Rep1 (Min1Max0 ar lf_f1 lf_f0) hfs) (Up1 rs)
   = let hf_domains = map (avUncurry dss) dts
         hf_MMIs = myZipWith3 inMMI hf_domains hfs rs
         (hf_maxI, hf_minI) = foldr1 inIntersect hf_MMIs
         min2 = avMinfrel [ x `avLUBfrel` y | x <- hf_minI, y <- lf_f1 ]
     in  inNormalise hf_maxI min2

-- cheat on this one, because I'm lazy
inMMI (Func dss (Lift2 dts)) (Rep2 lf mf hfs) a
   = let isoD              = Func dss (Lift1 [Lift1 dts])
         isoR              = Rep1 lf [Rep1 mf hfs]
         isoA Stop2        = Stop1
         isoA Up2          = Up1 [Stop1]
         isoA (UpUp2 rs)   = Up1 [Up1 rs]
     in
         inMMI isoD isoR (isoA a)

--inMMI (DFunc dss (Cross dts), RFunc (RepCross reps)) 
--    (Cross dts2, Split rs)
--    | dts == dts2 {-INVARIANT-}
--    = let doOne n 
--             = inMMI (avUncurryDomain (DFunc dss (dts##n)), 
--                      RFunc (reps##n)) (dts##n, rs##n)
--      in foldr1 inIntersect (map doOne [0 .. length reps - 1])
--
---- special case for partial applications of [... -> 2]
--inMMI f@(DFunc dssf Two, RFunc (RepTwo (Min1Max0 arf f1 f0)))
--      g@(DFunc dssg Two, RFunc (RepTwo (Min1Max0 arg g1 g0)))
--    | arf > arg && {-INVARIANT-}
--      dssg == drop (length dssf - length dssg) dssf {-INVARIANT-}
--    = let (g1Max, g1Min) = inMMI g (Two, One)
--          (g0Max, g0Min) = inMMI g (Two, Zero)
--          fMaxs = [inPapL f fels | MkFrel fels <- g1Min]
--          fMins = [inPapL f fels | MkFrel fels <- g0Max]
--          iMaxs = [inMMI fn (Two, One)  | fn <- fMaxs]
--          iMins = [inMMI fn (Two, Zero) | fn <- fMins]
--      in foldr1 inIntersect (iMaxs ++ iMins)
--
--inMMI f@(DFunc dssf (Lift dtf), RFunc (RepLift lff hff))
--      g@(DFunc dssg (Lift dtg), RFunc (RepLift lfg hfg))
--    | dtf == dtg && {-INVARIANT-}
--      dssg == drop (length dssf - length dssg) dssf {-INVARIANT-}
--    = let lofac_f = (DFunc dssf Two, RFunc (RepTwo lff))
--          lofac_g = (DFunc dssg Two, RFunc (RepTwo lfg))
--          hifac_f = (avUncurryDomain (DFunc dssf dtf), RFunc hff)
--          hifac_g = (avUncurryDomain (DFunc dssg dtg), RFunc hfg)
--      in  inIntersect (inMMI lofac_f lofac_g) (inMMI hifac_f hifac_g)
--
--inMMI f@(DFunc dssf (Cross dtfs), RFunc (RepCross repsf))
--      g@(DFunc dssg (Cross dtgs), RFunc (RepCross repsg))
--    = let doOne n 
--             = inMMI (avUncurryDomain (DFunc dssf (dtfs##n)), RFunc (repsf##n))
--                     (avUncurryDomain (DFunc dssg (dtgs##n)), RFunc (repsg##n))
--          in foldr1 inIntersect (map doOne [0 :: Int .. length repsf - 1])

-- otherwise, give up and call the mindless method
inMMI dss f a 
   = inMMI_mindless dss f a


-- ==========================================================--
----
--inPapL :: Point -> [Point] -> Point
--
--inPapL (DFunc dss Two, RFunc (RepTwo (Min1Max0 ar f1 f0))) args
--   = let argCount = length args
--         argDomainAfter = revDrop argCount dss
--         revDrop n = reverse . drop n . reverse
--         revTake n = reverse . take n . reverse
--         newf1 = sort (avMinfrel [MkFrel (revDrop argCount fel) |
--                                  MkFrel fel <- f1,
--                                  and (myZipWith2 avBelowEQ
--                                     (revTake argCount fel) args)])
--         newf0 = sort (avMaxfrel [MkFrel (revDrop argCount fel) |
--                                  MkFrel fel <- f0,
--                                  and (myZipWith2 avBelowEQ
--                                     args (revTake argCount fel))])
--     in (DFunc argDomainAfter Two,
--         RFunc (RepTwo (Min1Max0 (ar-argCount) newf1 newf0)))
--

-- ==========================================================--
--
inInverse_mindless :: [Domain] -> Rep -> Route -> [FrontierElem]

inInverse_mindless argDomains f a
   = let isPartialApp 
            = case a of { Rep _ -> True; _ -> False }
         aRep
            = case a of { Rep r -> r }
         actualArgDomains 
            = if     isPartialApp 
              then   take (amRepArity f - amRepArity aRep) argDomains
              else   argDomains
         allArgs
            = myCartesianProduct (map amAllRoutes actualArgDomains)
     in
         [MkFrel args | args <- allArgs, apApply (Rep f) args == a]

--inTrace :: Bool -> Bool
--inTrace x = x

-- ==========================================================--
-- === end                                     Inverse.hs ===--
-- ==========================================================--

