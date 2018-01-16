 
-- ==========================================================--
-- === Revised domain operations for HO analysis          ===--
-- ===                                   AbstractVals2.hs ===--
-- ==========================================================--

module AbstractVals2 where
import BaseDefs
import Utils
import MyUtils


infix 9 <<   -- Below-or-equal for Routes
infix 9 /\   -- Binary GLB for routes
infix 9 \/   -- Binary LUB for routes


-- ==========================================================--
-- ===                                                    ===--
-- === Top and bottom points of domains.                  ===--
-- ===                                                    ===--
-- ==========================================================--

-- ==========================================================--
--
avUncurry :: [Domain] -> Domain -> Domain

avUncurry dss (Func dss2 dt) = Func (dss++dss2) dt
avUncurry dss non_func_dom   = Func dss non_func_dom


-- ==========================================================--
--
avTopR :: Domain -> Route

avTopR Two               = One
avTopR (Lift1 ds)        = Up1 (map avTopR ds)
avTopR (Lift2 ds)        = UpUp2 (map avTopR ds)
avTopR d@(Func dss dt)   = Rep (avTopR_aux d)


-- ==========================================================--
--
avTopR_aux_2 :: [Domain] -> Frontier

avTopR_aux_2 dss 
   = Min1Max0 (length dss) [MkFrel (map avBottomR dss)] []


-- ==========================================================--
--
avTopR_aux :: Domain -> Rep

avTopR_aux (Func dss Two)
   = RepTwo (avTopR_aux_2 dss)

avTopR_aux (Func dss (Lift1 dts))
   = let lf = avTopR_aux_2 dss
         hf_domains = map (avUncurry dss) dts
         hfs = map avTopR_aux hf_domains
     in
         Rep1 lf hfs

avTopR_aux (Func dss (Lift2 dts))
   = let lf = avTopR_aux_2 dss
         hf_domains = map (avUncurry dss) dts
         hfs = map avTopR_aux hf_domains
     in
         Rep2 lf lf hfs


-- ==========================================================--
--
avBottomR :: Domain -> Route

avBottomR Two               = Zero
avBottomR (Lift1 ds)        = Stop1
avBottomR (Lift2 ds)        = Stop2
avBottomR d@(Func dss dt)   = Rep (avBottomR_aux d)


-- ==========================================================--
--
avBottomR_aux_2 :: [Domain] -> Frontier

avBottomR_aux_2 dss 
   = Min1Max0 (length dss) [] [MkFrel (map avTopR dss)]


-- ==========================================================--
--
avBottomR_aux :: Domain -> Rep

avBottomR_aux (Func dss Two)
   = RepTwo (avBottomR_aux_2 dss)

avBottomR_aux (Func dss (Lift1 dts))
   = let lf = avBottomR_aux_2 dss 
         hf_domains = map (avUncurry dss) dts
         hfs = map avBottomR_aux hf_domains
     in
         Rep1 lf hfs

avBottomR_aux (Func dss (Lift2 dts))
   = let lf = avBottomR_aux_2 dss
         hf_domains = map (avUncurry dss) dts
         hfs = map avBottomR_aux hf_domains
     in
         Rep2 lf lf hfs


-- ==========================================================--
--
avIsBottomR :: Route -> Bool

avIsBottomR Zero        = True
avIsBottomR Stop1       = True
avIsBottomR Stop2       = True
avIsBottomR One         = False
avIsBottomR (Up1 _)     = False
avIsBottomR Up2         = False
avIsBottomR (UpUp2 _)   = False
avIsBottomR (Rep r)     = avIsBottomRep r


-- ==========================================================--
--
avIsBottomRep :: Rep -> Bool

avIsBottomRep (RepTwo (Min1Max0 ar f1 f0))
   = null f1
avIsBottomRep (Rep1 (Min1Max0 lf_ar lf_f1 lf_f0) hfs)
   = null lf_f1
avIsBottomRep (Rep2 (Min1Max0 lf_ar lf_f1 lf_f0) mf hfs)   
   = null lf_f1


-- ==========================================================--
-- Is this correct?  I think so.
--
avIsTopR :: Route -> Bool

avIsTopR Zero         = False
avIsTopR One          = True
avIsTopR Stop1        = False
avIsTopR (Up1 rs)     = myAll avIsTopR rs
avIsTopR Stop2        = False
avIsTopR Up2          = False
avIsTopR (UpUp2 rs)   = myAll avIsTopR rs
avIsTopR (Rep r)      = avIsTopRep r


-- ==========================================================--
--
avIsTopRep :: Rep -> Bool

avIsTopRep (RepTwo (Min1Max0 ar f1 f0))
   = null f0
avIsTopRep (Rep1 lf hfs)
   = myAll avIsTopRep hfs
avIsTopRep (Rep2 lf mf hfs)
   = myAll avIsTopRep hfs


-- ==========================================================--
-- ===                                                    ===--
-- === Partial ordering predicates for points in domains. ===--
-- ===                                                    ===--
-- ==========================================================--

-- ==========================================================--
--
(<<) :: Route -> Route -> Bool

Zero         <<   _           = True
One          <<   One         = True
One          <<   Zero        = False

Stop1        <<   _           = True
Up1 rs1      <<   Up1 rs2     = avLEQR_list rs1 rs2
Up1 rs1      <<   _           = False    

Stop2        <<   _           = True
Up2          <<   Stop2       = False
Up2          <<   _           = True
UpUp2 rs1    <<   UpUp2 rs2   = avLEQR_list rs1 rs2
UpUp2 rs1    <<   _           = False

Rep rep1     <<   Rep rep2    = avBelowEQrep rep1 rep2


-- ==========================================================--
-- A little bit of Cordy-style loop unrolling
-- although not actually tail-strict :-)
--
avLEQR_list :: [Route] -> [Route] -> Bool

avLEQR_list [] []
   = True

avLEQR_list (a1:[]) (b1:[]) 
   = a1 << b1

avLEQR_list (a1:a2:[]) (b1:b2:[])
   = if      a1 << b1 
     then    a2 << b2 
     else    False

avLEQR_list (a1:a2:a3:[]) (b1:b2:b3:[])
   = if      a1 << b1 
     then    if      a2 << b2 
             then    a3 << b3
             else    False
     else    False

avLEQR_list (a1:a2:a3:a4:[]) (b1:b2:b3:b4:[])
   = if      a1 << b1
     then    if      a2 << b2
             then    if      a3 << b3
                     then    a4 << b4
                     else    False
             else    False
     else    False

-- OLD: avLEQR_list (a1:a2:a3:a4:as) (b1:b2:b3:b4:bs)
avLEQR_list (a1:a2:a3:a4:as@(_:_)) (b1:b2:b3:b4:bs@(_:_))
   = if      a1 << b1
     then    if      a2 << b2
             then    if      a3 << b3
                     then    if      a4 << b4
                             then    avLEQR_list as bs
                             else    False
                     else    False
             else    False
     else    False


--avLEQR_list (a:as) (b:bs)  = if a << b then avLEQR_list as bs else False
avLEQR_list _ _            = panic "avLEQR_list: unequal lists"


-- ==========================================================--
--
avBelowEQfrel :: FrontierElem -> FrontierElem -> Bool

avBelowEQfrel (MkFrel rs1) (MkFrel rs2)
   = avLEQR_list rs1 rs2


-- ==========================================================--
--
avBelowEQfrontier :: Frontier -> Frontier -> Bool

avBelowEQfrontier (Min1Max0 ar1 f1a f0a) (Min1Max0 ar2 f1b f0b)
   --
   -- | ar1 == ar2 {-INVARIANT-}
   --
   -- = myAnd [myOr [q `avBelowEQfrel` p | q <- f1b] | p <- f1a]
   --
   -- Tail recursive special
   -- 
   = let outer []        = True
         outer (x:xs)    = if inner x f1b then outer xs else False
         inner y []      = False
         inner y (z:zs)  = if z `avBelowEQfrel` y then True else inner y zs
     in
         outer f1a



-- ==========================================================--
--
avBelowEQrep :: Rep -> Rep -> Bool

avBelowEQrep (RepTwo fr1) (RepTwo fr2)
   = avBelowEQfrontier fr1 fr2

avBelowEQrep (Rep1 lf1 hfs1) (Rep1 lf2 hfs2)
   = avBelowEQfrontier lf1 lf2 &&
     myAndWith2 avBelowEQrep hfs1 hfs2

avBelowEQrep (Rep2 lf1 mf1 hfs1) (Rep2 lf2 mf2 hfs2)
   = avBelowEQfrontier lf1 lf2 &&
     avBelowEQfrontier mf1 mf2 &&
     myAndWith2 avBelowEQrep hfs1 hfs2


-- ==========================================================--
-- ===                                                    ===--
-- === LUB and GLB operations for Points.                 ===--
-- ===                                                    ===--
-- ==========================================================--

-- ==========================================================--
--
(\/) :: Route -> Route -> Route

p@ Zero        \/  q              = q
p@ One         \/  q              = p

p@ Stop1       \/  q              = q
p@(Up1 rs1)    \/  Stop1          = p
p@(Up1 rs1)    \/  Up1 rs2        = Up1 (myZipWith2 (\/) rs1 rs2)

p@ Stop2       \/  q              = q
p@ Up2         \/  Stop2          = p
p@ Up2         \/  q              = q
p@(UpUp2 rs1)  \/  UpUp2 rs2      = UpUp2 (myZipWith2 (\/) rs1 rs2)
p@(UpUp2 rs1)  \/  q              = p

p@(Rep rep1)   \/  q@(Rep rep2)   = Rep (avLUBrep rep1 rep2)


-- ==========================================================--
--
avLUBfrel :: FrontierElem -> FrontierElem -> FrontierElem

avLUBfrel (MkFrel rs1) (MkFrel rs2) 
   = MkFrel (myZipWith2 (\/) rs1 rs2)


-- ==========================================================--
--
avLUBfrontier :: Frontier -> Frontier -> Frontier

avLUBfrontier (Min1Max0 ar1 f1a f0a) (Min1Max0 ar2 f1b f0b)
   -- | ar1 == ar2  {-INVARIANT-}
   = Min1Max0 ar1 (avLUBmin1frontier f1a f1b) (avLUBmax0frontier f0a f0b)


-- ==========================================================--
--
avLUBrep :: Rep -> Rep -> Rep

avLUBrep (RepTwo fr1) (RepTwo fr2)
   = RepTwo (avLUBfrontier fr1 fr2)
avLUBrep (Rep1 lf1 hfs1) (Rep1 lf2 hfs2)
   = Rep1 (avLUBfrontier lf1 lf2) (myZipWith2 avLUBrep hfs1 hfs2)
avLUBrep (Rep2 lf1 mf1 hfs1) (Rep2 lf2 mf2 hfs2)
   = Rep2 (avLUBfrontier lf1 lf2) (avLUBfrontier mf1 mf2)
          (myZipWith2 avLUBrep hfs1 hfs2)


-- ==========================================================--
--
avLUBmin1frontier :: [FrontierElem] -> [FrontierElem] -> [FrontierElem]

avLUBmin1frontier f1a f1b
   = sort (foldr avMinAddPtfrel f1a f1b)  {-OPTIMISE-}


-- ==========================================================--
--
avLUBmax0frontier :: [FrontierElem] -> [FrontierElem] -> [FrontierElem]

avLUBmax0frontier f0a f0b
   = sort (avMaxfrel [ x `avGLBfrel` y | x <- f0a, y <- f0b ])


-- ==========================================================--
--
(/\) :: Route -> Route -> Route

p@ Zero        /\  q                = p
p@ One         /\  q                = q

p@ Stop1       /\  q                = p
p@(Up1 rs1)    /\  (Up1 rs2)        = Up1 (myZipWith2 (/\) rs1 rs2)
p@(Up1 rs1)    /\  q                = q

p@ Stop2       /\  q                = p
p@ Up2         /\  q@ Stop2         = q
p@ Up2         /\  q                = p
p@(UpUp2 rs1)  /\  q@(UpUp2 rs2)    = UpUp2 (myZipWith2 (/\) rs1 rs2)
p@(UpUp2 rs1)  /\  q                = q

p@(Rep rep1)   /\  q@(Rep rep2)     = Rep (avGLBrep rep1 rep2)


-- ==========================================================--
--
avGLBfrel :: FrontierElem -> FrontierElem -> FrontierElem

avGLBfrel (MkFrel rs1) (MkFrel rs2) 
   = MkFrel (myZipWith2 (/\) rs1 rs2)


-- ==========================================================--
--
avGLBfrontier :: Frontier -> Frontier -> Frontier

avGLBfrontier (Min1Max0 ar1 f1a f0a) (Min1Max0 ar2 f1b f0b)
   -- | ar1 == ar2  {-INVARIANT-}
   = Min1Max0 ar1 (avGLBmin1frontier f1a f1b) (avGLBmax0frontier f0a f0b)


-- ==========================================================--
--
avGLBrep :: Rep -> Rep -> Rep

avGLBrep (RepTwo fr1) (RepTwo fr2)
   = RepTwo (avGLBfrontier fr1 fr2)
avGLBrep (Rep1 lf1 hfs1) (Rep1 lf2 hfs2)
   = Rep1 (avGLBfrontier lf1 lf2) (myZipWith2 avGLBrep hfs1 hfs2)
avGLBrep (Rep2 lf1 mf1 hfs1) (Rep2 lf2 mf2 hfs2)
   = Rep2 (avGLBfrontier lf1 lf2) (avGLBfrontier mf1 mf2)
          (myZipWith2 avGLBrep hfs1 hfs2)


-- ==========================================================--
--
avGLBmax0frontier :: [FrontierElem] -> [FrontierElem] -> [FrontierElem]

avGLBmax0frontier f0a f0b
   = sort (foldr avMaxAddPtfrel f0a f0b)  {-OPTIMISE-}


-- ==========================================================--
--
avGLBmin1frontier :: [FrontierElem] -> [FrontierElem] -> [FrontierElem]

avGLBmin1frontier f1a f1b
   = sort (avMinfrel [ x `avLUBfrel` y | x <- f1a, y <- f1b ])


-- ==========================================================--
-- ===                                                    ===--
-- === Min and Max operations for frontiers.              ===--
-- === Note avBelowMax0/avAboveMin1 expect Frel's to be   ===--
-- === of the same length.                                ===--
-- ===                                                    ===--
-- ==========================================================--

pt `avBelowMax0frel` f = myAny (pt `avBelowEQfrel`) f

pt `avAboveMin1frel` f = myAny (`avBelowEQfrel` pt) f

avMinAddPtfrel x ys
   | x `avAboveMin1frel` ys = ys
   | otherwise = x:[y | y <- ys, not (x `avBelowEQfrel` y)]

avMaxAddPtfrel x ys
   | x `avBelowMax0frel` ys = ys
   | otherwise = x:[y | y <- ys, not (y `avBelowEQfrel` x)]

avMinfrel = foldr avMinAddPtfrel []

avMaxfrel = foldr avMaxAddPtfrel []


-- ==========================================================--
-- ===                                                    ===--
-- === Min and Max operations for Routes                  ===--
-- ===                                                    ===--
-- ==========================================================--

pt `avBelowMax0R` f = myAny (pt <<) f

pt `avAboveMin1R` f = myAny (<< pt) f

avMinAddPtR x ys
   | x `avAboveMin1R` ys = ys
   | otherwise = x:[y | y <- ys, not (x << y)]

avMaxAddPtR x ys
   | x `avBelowMax0R` ys = ys
   | otherwise = x:[y | y <- ys, not (y << x)]

avMinR = foldr avMinAddPtR [] :: [Route] -> [Route]

avMaxR = foldr avMaxAddPtR [] :: [Route] -> [Route]


-- ==========================================================--
-- ===                                                    ===--
-- === Min and Max operations for Reps                    ===--
-- ===                                                    ===--
-- ==========================================================--

pt `avBelowMax0rep` f = myAny (pt `avBelowEQrep`) f

pt `avAboveMin1rep` f = myAny (`avBelowEQrep` pt) f

avMinAddPtrep x ys
   | x `avAboveMin1rep` ys = ys
   | otherwise = x:[y | y <- ys, not (x `avBelowEQrep` y)]

avMaxAddPtrep x ys
   | x `avBelowMax0rep` ys = ys
   | otherwise = x:[y | y <- ys, not (y `avBelowEQrep` x)]

avMinrep = foldr avMinAddPtrep [] :: [Rep] -> [Rep]

avMaxrep = foldr avMaxAddPtrep [] :: [Rep] -> [Rep]


-- ==========================================================--
-- === end                               AbstractVals2.hs ===--
-- ==========================================================--
