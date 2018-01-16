 
-- ==========================================================--
-- === Miscellaneous operations in the Abstract value     ===--
-- === world.                             AbstractMisc.hs ===--
-- ==========================================================--

module AbstractMisc where
import BaseDefs
import Utils
import MyUtils
import AbstractVals2
import SuccsAndPreds2

import Data.List(nub) -- 1.3

-- ==========================================================--
--
amIAboves :: Domain -> Route -> [Route]

amIAboves d r = map (r \/) (spSuccsR d r)


-- ==========================================================--
--
amIBelows :: Domain -> Route -> [Route]

amIBelows d r = map (r /\) (spPredsR d r)


-- ==========================================================--
--
amPushUpFF :: Domain -> [Route] -> [Route]

amPushUpFF d [] = []
amPushUpFF d xs = nub (concat (map (amIAboves d) xs))


-- ==========================================================--
--
amPushDownFF :: Domain -> [Route] -> [Route]

amPushDownFF d [] = []
amPushDownFF d xs = nub (concat (map (amIBelows d) xs))


-- ==========================================================--
--
amAllUpSlices :: Domain -> [[Route]]

amAllUpSlices d
   = takeWhile (not.null) (iterate (amPushUpFF d) [avBottomR d])


-- ==========================================================--
--
amAllDownSlices :: Domain -> [[Route]]

amAllDownSlices d
   = takeWhile (not.null) (iterate (amPushDownFF d) [avTopR d])


-- ==========================================================--
--
amAllRoutes :: Domain -> [Route]

amAllRoutes Two 
   = [Zero, One]

amAllRoutes (Lift1 dss)
   = Stop1 : map Up1 (myCartesianProduct (map amAllRoutes dss))

amAllRoutes (Lift2 dss)
   = Stop2 : Up2 : map UpUp2 (myCartesianProduct (map amAllRoutes dss))

amAllRoutes (Func dss dt)
   = concat (amAllUpSlices (Func dss dt))


-- ==========================================================--
--
amUpCloseOfMinf :: Domain -> [Route] -> [Route]

amUpCloseOfMinf d [] 
   = []
amUpCloseOfMinf d q@(x:_) 
   = x : (amUpCloseOfMinf d 
            (avMinR [ y \/ z | y <- q, z <- spSuccsR d x ]))


-- ==========================================================--
--
amDownCloseOfMaxf :: Domain -> [Route] -> [Route]

amDownCloseOfMaxf d [] 
   = []
amDownCloseOfMaxf d q@(x:_) 
   = x : (amDownCloseOfMaxf d
            (avMaxR [ y /\ z | y <- q, z <- spPredsR d x ]))


-- ==========================================================--
--
amAllRoutesMinusTopJONES :: Domain -> [Route]

amAllRoutesMinusTopJONES d
   = amDownCloseOfMaxf d (spPredsR d (avTopR d))


-- ==========================================================--
--
--amAllRoutesMinusTopMINE :: Domain -> [Route]
--
--amAllRoutesMinusTopMINE d
--   = let sliceJustBelowTop 
--            = spPredsR d (avTopR d)
--         allSlices
--            = takeWhile (not.null) 
--                        (iterate (amPushDownFF d) sliceJustBelowTop)
--     in
--         concat allSlices


-- ==========================================================--
--
amEqualPoints :: Point -> Point -> Bool

amEqualPoints (d1, r1) (d2, r2)
   = if     d1 == d2 
     then   r1 == r2 
     else   panic "Comparing points in different domains."


-- ==========================================================--
--
amIsaHOF :: Domain -> Bool

amIsaHOF (Func dss dt) 
   = amContainsFunctionSpace dt ||
     myAny amContainsFunctionSpace dss


-- ==========================================================--
--
amContainsFunctionSpace :: Domain -> Bool

amContainsFunctionSpace Two           = False
amContainsFunctionSpace (Lift1 dss)   = myAny amContainsFunctionSpace dss
amContainsFunctionSpace (Lift2 dss)   = myAny amContainsFunctionSpace dss
amContainsFunctionSpace (Func _ _)    = True


-- ==========================================================--
--
amIsDataFn :: Domain -> Bool

amIsDataFn (Func _ dt) = not (amContainsFunctionSpace dt)


-- ==========================================================--
--
amRepArity :: Rep -> Int

amRepArity (RepTwo (Min1Max0 ar f1 f0))                 = ar
amRepArity (Rep1 (Min1Max0 lf_ar lf_f1 lf_f0) hfs)      = lf_ar
amRepArity (Rep2 (Min1Max0 lf_ar lf_f1 lf_f0) mf hfs)   = lf_ar


-- ==========================================================--
--
amStrongNormalise :: Domain -> Domain

amStrongNormalise Two 
   = Two

amStrongNormalise (Lift1 ds)
   = Lift1 (map amStrongNormalise ds)

amStrongNormalise (Lift2 ds)
   = Lift2 (map amStrongNormalise ds)

amStrongNormalise (Func dss (Func dss2 dt))
   = amStrongNormalise (Func (dss++dss2) dt)

amStrongNormalise (Func dss non_func_res) 
   = Func (map amStrongNormalise dss) (amStrongNormalise non_func_res)


-- ==========================================================--
--
amMeetIRoutes :: Domain -> [Route]

amMeetIRoutes Two 
   = [Zero]
amMeetIRoutes (Lift1 ds)
   = Stop1 :
     map Up1 (myListVariants (map avTopR ds) (map amMeetIRoutes ds))
amMeetIRoutes (Lift2 ds)
   = Stop2 :
     Up2   :
     map UpUp2 (myListVariants (map avTopR ds) (map amMeetIRoutes ds))


-- ==========================================================--
-- === end                                AbstractMisc.hs ===--
-- ==========================================================--

