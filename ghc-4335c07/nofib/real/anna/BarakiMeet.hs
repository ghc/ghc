
-- ==========================================================--
-- === Specialised meet to speed up calculation of meets  ===--
-- === in Gebre's polymorphic generalisation system       ===--
-- ===                                                    ===--
-- ===                                      BarakiMeet.hs ===--
-- ==========================================================--


module BarakiMeet where
import BaseDefs
import MyUtils
import Utils
import AbstractVals2
import SuccsAndPreds2


infix 9 %%


-- ==========================================================--
--
bmNorm :: Domain -> Route -> Route

bmNorm Two         r              =  r
bmNorm (Lift1 ds)  r@ Stop1       =  r
bmNorm (Lift1 ds)    (Up1 rs)     =  Up1 (myZipWith2 bmNorm ds rs)
bmNorm (Lift2 ds)  r@ Stop2       =  r
bmNorm (Lift2 ds)  r@ Up2         =  r
bmNorm (Lift2 ds)    (UpUp2 rs)   =  UpUp2 (myZipWith2 bmNorm ds rs)
bmNorm d             (Rep rep)    =  Rep (bmNorm_rep d rep)


bmNorm_rep (Func dss Two) (RepTwo fr)
   = RepTwo (bmNorm_2 dss fr)

bmNorm_rep (Func dss (Lift1 dts)) (Rep1 lf hfs)
   = let hf_domains = map (avUncurry dss) dts
     in
         Rep1 (bmNorm_2 dss lf)
              (myZipWith2 bmNorm_rep hf_domains hfs)

bmNorm_rep (Func dss (Lift2 dts)) (Rep2 lf mf hfs)
   = let hf_domains = map (avUncurry dss) dts
     in
         Rep2 (bmNorm_2 dss lf) (bmNorm_2 dss mf)
              (myZipWith2 bmNorm_rep hf_domains hfs)


bmNorm_2 dss (Min1Max0 ar f1 f0)
   = let norm_f0 = sort (map (bmNorm_frel dss) f0)
         norm_f1 = spMin1FromMax0 dss f0
     in
         Min1Max0 ar norm_f1 norm_f0

bmNorm_frel dss (MkFrel fels) 
   = MkFrel (myZipWith2 bmNorm dss fels)


-- ==========================================================--
--
bmGLB :: Route -> Route -> Route

bmGLB (Rep rep1) (Rep rep2) = Rep (bmGLBrep rep1 rep2)


-- ==========================================================--
--
bmGLBrep :: Rep -> Rep -> Rep

bmGLBrep (RepTwo fr1) (RepTwo fr2)
   = RepTwo (bmGLBfrontier fr1 fr2)
bmGLBrep (Rep1 lf1 hfs1) (Rep1 lf2 hfs2)
   = Rep1 (bmGLBfrontier lf1 lf2) (myZipWith2 bmGLBrep hfs1 hfs2)
bmGLBrep (Rep2 lf1 mf1 hfs1) (Rep2 lf2 mf2 hfs2)
   = Rep2 (bmGLBfrontier lf1 lf2) (bmGLBfrontier mf1 mf2)
          (myZipWith2 bmGLBrep hfs1 hfs2)


-- ==========================================================--
--
bmGLBfrontier :: Frontier -> Frontier -> Frontier

bmGLBfrontier (Min1Max0 ar1 _ f0a) (Min1Max0 ar2 _ f0b)
   --
   -- | ar1 == ar2  {-INVARIANT-}
   --
   = Min1Max0 ar1 [] (bmGLBmax0frontier f0a f0b)


-- ==========================================================--
--
bmGLBmax0frontier :: [FrontierElem] -> [FrontierElem] -> [FrontierElem]

bmGLBmax0frontier f0a f0b
   = {-sort-} (foldr bmMaxAddPtfrel f0a f0b)  {-OPTIMISE-}

bmMaxAddPtfrel x ys
   | x `bmBelowMax0frel` ys = ys
   | otherwise = x:[y | y <- ys, not (y `bmBelowEQfrel` x)]

pt `bmBelowMax0frel` f = myAny (pt `bmBelowEQfrel`) f


-- ==========================================================--
--
bmBelowEQfrel :: FrontierElem -> FrontierElem -> Bool

bmBelowEQfrel (MkFrel rs1) (MkFrel rs2)
   = myAndWith2 (%%) rs1 rs2


-- ==========================================================--
--
(%%) :: Route -> Route -> Bool

Zero         %%   _           = True
One          %%   One         = True
One          %%   Zero        = False

Stop1        %%   _           = True
Up1 rs1      %%   Up1 rs2     = myAndWith2 (%%) rs1 rs2
Up1 rs1      %%   _           = False    

Stop2        %%   _           = True
Up2          %%   Stop2       = False
Up2          %%   _           = True
UpUp2 rs1    %%   UpUp2 rs2   = myAndWith2 (%%) rs1 rs2
UpUp2 rs1    %%   _           = False

Rep rep1     %%   Rep rep2    = bmBelowEQrep rep1 rep2


-- ==========================================================--
--
bmBelowEQrep :: Rep -> Rep -> Bool

bmBelowEQrep (RepTwo fr1) (RepTwo fr2)
   = bmBelowEQfrontier fr1 fr2

bmBelowEQrep (Rep1 lf1 hfs1) (Rep1 lf2 hfs2)
   = bmBelowEQfrontier lf1 lf2 &&
     myAndWith2 bmBelowEQrep hfs1 hfs2

bmBelowEQrep (Rep2 lf1 mf1 hfs1) (Rep2 lf2 mf2 hfs2)
   = bmBelowEQfrontier lf1 lf2 &&
     bmBelowEQfrontier mf1 mf2 &&
     myAndWith2 bmBelowEQrep hfs1 hfs2


-- ==========================================================--
--
bmBelowEQfrontier :: Frontier -> Frontier -> Bool

bmBelowEQfrontier (Min1Max0 ar1 _ f0a) (Min1Max0 ar2 _ f0b)
   --
   -- | ar1 == ar2 {-INVARIANT-}
   -- = myAnd [myOr [p `bmBelowEQfrel` q | q <- f0a] | p <- f0b]
   --
   -- Tail recursive special
   --
   = let outer []        = True
         outer (x:xs)    = if inner x f0a then outer xs else False
         inner y []      = False
         inner y (z:zs)  = if y `bmBelowEQfrel` z then True else inner y zs
     in
         outer f0b

-- ==========================================================--
-- === end                                  BarakiMeet.hs ===--
-- ==========================================================--
