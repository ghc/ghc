 
-- ==========================================================--
-- === Successors and predecessors of a point in a        ===--
-- === finite lattice.                  SuccsAndPreds2.hs ===--
-- ==========================================================--

module SuccsAndPreds2 where
import BaseDefs
import Utils
import MyUtils
import AbstractVals2


-- ==========================================================--
-- ===                                                    ===--
-- === "succs" and "preds" of a point, where:             ===--
-- ===                                                    ===--
-- ===    succs(x) = Min (complement (downclose (x)))     ===--
-- ===    preds(x) = Max (complement (upclose (x)))       ===--
-- ===                                                    ===--
-- ==========================================================--

-- ==========================================================--
--
spSuccs :: Point -> [Point]

spSuccs (d1, r1) = [(d1, r) | r <- spSuccsR d1 r1]


-- ==========================================================--
--
spSuccsR :: Domain -> Route -> [Route]

spSuccsR Two Zero   = [One]
spSuccsR Two One    = []

spSuccsR (Lift1 ds) Stop1 
   = [Up1 (map avBottomR ds)]
spSuccsR (Lift1 [d]) (Up1 [r])
   = map (\rs -> Up1 [rs]) (spSuccsR d r)    {-OPTIONAL-}
spSuccsR (Lift1 ds) (Up1 rs)
   = map Up1 (myListVariants (map avBottomR ds) (myZipWith2 spSuccsR ds rs))

spSuccsR (Lift2 ds) Stop2 
   = [Up2]
spSuccsR (Lift2 ds) Up2 
   = [UpUp2 (map avBottomR ds)]
spSuccsR (Lift2 [d]) (UpUp2 [r])
   = map (\rs -> UpUp2 [rs]) (spSuccsR d r)  {-OPTIONAL-}
spSuccsR (Lift2 ds) (UpUp2 rs)
   = map UpUp2 (myListVariants (map avBottomR ds) (myZipWith2 spSuccsR ds rs))

spSuccsR d@(Func _ _) (Rep r)
   = map Rep (spSuccsRep d r)


-- ==========================================================--
--
spSuccsRep :: Domain -> Rep -> [Rep]

spSuccsRep (Func dss Two) (RepTwo (Min1Max0 ar f1 f0))
   = [RepTwo (Min1Max0 ar [x] (sort (spPredsFrel dss x))) | x <- f0]

spSuccsRep (Func dss (Lift1 dts)) (Rep1 lf hfs)
   = let hfDomains = map (avUncurry dss) dts
         hfBottoms = map avBottomR_aux hfDomains
         initTops = map avTopR dss
         s1 = [spLEmb hfBottoms h
              | RepTwo h <- spSuccsRep (Func dss Two) (RepTwo lf)]
         s2 = [spLLift initTops dss hfDomains hfs2
              | hfs2 <- myListVariants hfBottoms 
                        (myZipWith2 spSuccsRep hfDomains hfs)]
     in
         avMinrep (s1 ++ s2)

spSuccsRep (Func dss (Lift2 dts)) (Rep2 lf mf hfs)
   = let isoDomain = Func dss (Lift1 [Lift1 dts])
         isoRoute  = Rep1 lf [Rep1 mf hfs]
         isoSuccs  = spSuccsRep isoDomain isoRoute
         isoRouteInv (Rep1 lfi [Rep1 mfi hfsi])
            = Rep2 lfi mfi hfsi
     in
         map isoRouteInv isoSuccs


-- ==========================================================--
--
spSuccsFrel :: [Domain] -> FrontierElem -> [FrontierElem]

spSuccsFrel [d] (MkFrel [r])
   = map (\rs -> MkFrel [rs]) (spSuccsR d r)           {-OPTIONAL-}

spSuccsFrel ds (MkFrel rs)
   = map MkFrel (myListVariants (map avBottomR ds) (myZipWith2 spSuccsR ds rs))


-- ==========================================================--
--
spLEmb :: [Rep] -> Frontier -> Rep

spLEmb hfBottoms h 
   = Rep1 h hfBottoms


-- ==========================================================--
--
spLLift :: [Route] -> [Domain] -> [Domain] -> [Rep] -> Rep

spLLift initTops initDss hfDomains hfs_reps
   = let lf_arity = length initTops
         zapped_hfs = myZipWith2 (spLLift_aux lf_arity initTops initDss) 
                              hfDomains hfs_reps
         new_lf = case foldr1 avLUBrep zapped_hfs of
                         RepTwo fr -> fr
     in
         Rep1 new_lf hfs_reps


-- ==========================================================--
--
spLLift_aux :: Int -> [Route] -> [Domain] -> Domain -> Rep -> Rep

spLLift_aux des_arity initTops initDss (Func dss Two) fr
   = spLLift_reduce_arity_as_top des_arity initTops initDss fr
spLLift_aux des_arity initTops initDss (Func dss (Lift1 dts)) (Rep1 lf hfs)
   = spLLift_reduce_arity_as_top des_arity initTops initDss (RepTwo lf)
spLLift_aux des_arity initTops initDss (Func dss (Lift2 dts)) (Rep2 lf mf hfs)
   = spLLift_reduce_arity_as_top des_arity initTops initDss (RepTwo lf)


-- ==========================================================--
--
spLLift_reduce_arity_as_top :: Int -> [Route] -> [Domain] -> Rep -> Rep

spLLift_reduce_arity_as_top des_arity initTops initDss 
                            f@(RepTwo (Min1Max0 ar f1 f0))
   | ar == des_arity
   = f
   | ar >  des_arity
   = let shorten (MkFrel rs) = MkFrel (take des_arity rs)
         new_f1 = map shorten f1
         new_f0 = spMax0FromMin1_aux initTops initDss new_f1
     in
         RepTwo (Min1Max0 des_arity new_f1 new_f0)


-- ==========================================================--
--
spPreds :: Point -> [Point]

spPreds (d1, r1) = [(d1, r) | r <- spPredsR d1 r1]


-- ==========================================================--
--
spPredsR :: Domain -> Route -> [Route]

spPredsR Two Zero   = []
spPredsR Two One    = [Zero]

spPredsR (Lift1 ds) Stop1 
   = []
spPredsR (Lift1 [d]) (Up1 [r])
   = if   avIsBottomR r
     then [Stop1]
     else map (\rp -> Up1 [rp]) (spPredsR d r)     {-OPTIONAL-}
spPredsR (Lift1 ds) (Up1 rs)
   = if    myAll avIsBottomR rs
     then  [Stop1]
     else  map Up1 (myListVariants (map avTopR ds) (myZipWith2 spPredsR ds rs))

spPredsR (Lift2 ds) Stop2 
   = []
spPredsR (Lift2 ds) Up2 
   = [Stop2]
spPredsR (Lift2 [d]) (UpUp2 [r])
   = if   avIsBottomR r
     then [Up2]
     else map (\rp -> UpUp2 [rp]) (spPredsR d r)   {-OPTIONAL-}
spPredsR (Lift2 ds) (UpUp2 rs)
   = if    myAll avIsBottomR rs
     then  [Up2]
     else  map UpUp2 (myListVariants (map avTopR ds) (myZipWith2 spPredsR ds rs))

spPredsR d@(Func _ _) (Rep r)
   = map Rep (spPredsRep d r)


-- ==========================================================--
--
spPredsRep :: Domain -> Rep -> [Rep]

spPredsRep (Func dss Two) (RepTwo (Min1Max0 ar f1 f0))
   = [RepTwo (Min1Max0 ar (sort (spSuccsFrel dss x)) [x]) | x <- f1]

spPredsRep (Func dss (Lift1 dts)) (Rep1 lf hfs)
   = let hfDomains = map (avUncurry dss) dts
         hfTops = map avTopR_aux hfDomains
         lfDomain = Func dss Two
         lfTop = avTopR_aux_2 dss
         p1 = [spGEmb h dts
              | RepTwo h <- spPredsRep lfDomain (RepTwo lf)]
         p2 = [spGLift lfTop hfs2
              | hfs2 <- myListVariants hfTops
                        (myZipWith2 spPredsRep hfDomains hfs)]
     in
         avMaxrep (p1 ++ p2)

spPredsRep (Func dss (Lift2 dts)) (Rep2 lf mf hfs)
   = let isoDomain = Func dss (Lift1 [Lift1 dts])
         isoRoute  = Rep1 lf [Rep1 mf hfs]
         isoPreds  = spPredsRep isoDomain isoRoute
         isoRouteInv (Rep1 lfi [Rep1 mfi hfsi])
            = Rep2 lfi mfi hfsi
     in
         map isoRouteInv isoPreds


-- ==========================================================--
--
spPredsFrel :: [Domain] -> FrontierElem -> [FrontierElem]

spPredsFrel [d] (MkFrel [r])
   = map (\rp -> MkFrel [rp]) (spPredsR d r)           {-OPTIONAL-}

spPredsFrel ds (MkFrel rs)
   = map MkFrel (myListVariants (map avTopR ds) (myZipWith2 spPredsR ds rs))


-- ==========================================================--
--
spGLift :: Frontier -> [Rep] -> Rep

spGLift lfTop hfs2 = Rep1 lfTop hfs2


-- ==========================================================--
--
spGEmb :: Frontier -> [Domain] -> Rep

spGEmb lf hfTargDs = Rep1 lf (map (spGEmb_aux lf) hfTargDs)


-- ==========================================================--
--
spGEmb_aux :: Frontier -> Domain -> Rep

spGEmb_aux lf Two 
   = RepTwo lf

spGEmb_aux lf (Lift1 dss) 
   = Rep1 lf (map (spGEmb_aux lf) dss)

spGEmb_aux lf (Lift2 dss) 
   = Rep2 lf lf (map (spGEmb_aux lf) dss)

spGEmb_aux lf (Func dss dt)
   = spGEmb_aux 
        (case spGEmb_increase_arity_ignore lf dss of
           RepTwo re -> re) 
        dt


-- ==========================================================--
--
spGEmb_increase_arity_ignore :: Frontier -> [Domain] -> Rep

spGEmb_increase_arity_ignore f [] 
   = RepTwo f  {-OPTIONAL-}

spGEmb_increase_arity_ignore (Min1Max0 ar f1 f0) dss
   = let tops = map avTopR dss
         bottoms = map avBottomR dss
         extend_top (MkFrel rs) = MkFrel (rs++tops)
         extend_bottom (MkFrel rs) = MkFrel (rs++bottoms)
         new_f1 = map extend_bottom f1
         new_f0 = map extend_top f0
     in
         RepTwo (Min1Max0 (ar + length dss) new_f1 new_f0)


-- ==========================================================--
--
spMax0FromMin1 :: [Domain] -> [FrontierElem] -> [FrontierElem]

spMax0FromMin1 dss f1 
   = spMax0FromMin1_aux (map avTopR dss) dss f1

spMax0FromMin1_aux tops dss f1
   = sort (foldr avLUBmax0frontier [MkFrel tops] 
                 (map (spPredsFrel dss) f1))


-- ==========================================================--
--
spMin1FromMax0 :: [Domain] -> [FrontierElem] -> [FrontierElem]

spMin1FromMax0 dss f0
   = spMin1FromMax0_aux (map avBottomR dss) dss f0

spMin1FromMax0_aux bottoms dss f0
   = sort (foldr avGLBmin1frontier [MkFrel bottoms]
                 (map (spSuccsFrel dss) f0))


-- ==========================================================--
-- === end                              SuccsAndPreds2.hs ===--
-- ==========================================================--
