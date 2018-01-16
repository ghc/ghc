
-- ==========================================================--
-- === Simplification of abstract expressions ...         ===--
-- ===                                        Simplify.hs ===--
-- ==========================================================--

module Simplify where
import BaseDefs
import Utils
import MyUtils
import AbstractVals2
import AbstractEval2
import Apply

-- ==========================================================--
--
siVectorise :: HExpr Naam -> HExpr Naam

siVectorise (HLam vs1 (HLam vs2 e)) 
   = siVectorise (HLam (vs1++vs2) e)
siVectorise (HLam vs e) 
   = HLam vs (siVectorise e)
siVectorise (HApp (HTable t) e) 
   = HApp (HTable (map2nd siVectorise t)) (siVectorise e)
siVectorise (HApp f a)
   = case siVectorise f of
        HVAp fn args -> HVAp fn (args++[siVectorise a])
        HPoint p     -> HVAp (HPoint p) [siVectorise a]
        HVar v       -> HVAp (HVar v) [siVectorise a]
        non_vap      -> HApp non_vap (siVectorise a)
siVectorise h@(HVar _) = h
siVectorise h@(HPoint _) = h
siVectorise (HMeet es) = HMeet (map siVectorise es)


-- ==========================================================--
--
siSimplify :: HExpr Naam -> HExpr Naam

siSimplify hexpr
  = 
    let hexpr_after_one_cycle = siHOpt hexpr
    in
        if    hexpr == hexpr_after_one_cycle
        then  hexpr
        else  siSimplify hexpr_after_one_cycle


-- ==========================================================--
--
siHOpt :: HExpr Naam -> HExpr Naam

siHOpt (HMeet es)     = siHOpt_meet es
siHOpt (HApp  h1 h2)  = siHOpt_app (siHOpt h1) (siHOpt h2)
siHOpt p@(HPoint _)   = p
siHOpt v@(HVar _)     = v
siHOpt (HLam vs e)    = HLam vs (siHOpt e)
siHOpt (HTable t)     = HTable (map2nd siHOpt t)


-- ==========================================================--
-- meet-literal simplification
--
siHOpt_meet :: [HExpr Naam] -> HExpr Naam

siHOpt_meet es
   = let presimplified = map siHOpt es
         litsplit (lits, nonlits) (HPoint p) = (p:lits, nonlits)
         litsplit (lits, nonlits) other      = (lits, other:nonlits)
         (lits, nonlits) = foldl litsplit ([],[]) presimplified
         onelit = foldr1 (\/) lits
     in
     if          null lits
     then        HMeet presimplified  -- can't do anything
     else if     avIsTopR onelit
     then        HPoint onelit
     else if     avIsBottomR onelit
     then        aeMkMeet (HPoint onelit) nonlits
     else        aeMkMeet (HPoint onelit) ((HPoint onelit):nonlits)


-- ==========================================================--
-- case-of-case simplification
-- literal-function-applied-to-literal simplification
--
siHOpt_app :: HExpr Naam -> HExpr Naam -> HExpr Naam

siHOpt_app (HTable t) (HPoint p) 
  = siHOpt (utSureLookup t "siHOpt_app" p)

siHOpt_app (HPoint p1) (HPoint p2) 
  = HPoint (apApply p1 [p2])

siHOpt_app h1_other h2_other = HApp h1_other h2_other


-- ==========================================================--
-- === end                                    Simplify.hs ===--
-- ==========================================================--
