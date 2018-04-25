
-- ==========================================================--
-- === Application of function points to                  ===--
-- === argument points.                          Apply.hs ===--
-- ==========================================================--

module Apply where
import BaseDefs
import Utils
import MyUtils
import AbstractVals2

-- ==========================================================--
--
apApply :: Route -> [Route] -> Route

apApply (Rep func) args = apPapConst (apPap func args)


-- ==========================================================--
--
apPap_2 :: Int -> Frontier -> [Route] -> Frontier

apPap_2 argCount (Min1Max0 ar f1 f0) args
   = let newf1 
            = sort (avMinfrel 
                   [MkFrel (drop argCount fel) 
                   | MkFrel fel <- f1, 
                     myAndWith2 (<<) (take argCount fel) args
                   ])
         newf0 
            = sort (avMaxfrel 
                   [MkFrel (drop argCount fel) 
                   | MkFrel fel <- f0,
                     myAndWith2 (<<) args (take argCount fel)
                   ])
         result = Min1Max0 (ar-argCount) newf1 newf0
     in
         if argCount <= ar then result else panic "apPap_2"


-- ==========================================================--
--
apPap :: Rep -> [Route] -> Rep

apPap (RepTwo fr) args
   = let argCount = length args
     in
         RepTwo (apPap_2 argCount fr args)
                        
apPap (Rep1 lf hfs) args
   = let argCount = length args
         new_lf = apPap_2 argCount lf args
         new_hfs = map (flip apPap args) hfs
     in
         Rep1 new_lf new_hfs

apPap (Rep2 lf mf hfs) args
   = let argCount = length args
         new_lf = apPap_2 argCount lf args
         new_mf = apPap_2 argCount mf args
         new_hfs = map (flip apPap args) hfs
     in
         Rep2 new_lf new_mf new_hfs


-- ==========================================================--
--
apPapConst :: Rep -> Route

apPapConst rep@(RepTwo (Min1Max0 ar f1 f0))
   | ar > 0                           = Rep rep
   | null f1 && not (null f0)         = Zero
   | not (null f1) && null f0         = One
   | otherwise                        = panic "apPapConst(1)"

apPapConst rep@(Rep1 (Min1Max0 lf_ar lf_f1 lf_f0) hfs)
   | lf_ar > 0 = Rep rep
   | null lf_f1 && not (null lf_f0)   = Stop1
   | not (null lf_f1) && null lf_f0   = Up1 (map apPapConst hfs)
   | otherwise                        = panic "apPapConst(2)"

apPapConst rep@(Rep2 (Min1Max0 lf_ar lf_f1 lf_f0) (Min1Max0 mf_ar mf_f1 mf_f0) hfs)
   | lf_ar > 0 = Rep rep
   | null lf_f1 && not (null lf_f0)   = Stop2
   | null mf_f1 && not (null mf_f0)   = Up2
   | not (null mf_f1) && null mf_f0   = UpUp2 (map apPapConst hfs)
   | otherwise                        = panic "apPapConst(3)"


-- ==========================================================--
-- === end                                       Apply.hs ===--
-- ==========================================================--
