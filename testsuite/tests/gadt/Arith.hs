{-# LANGUAGE GADTs #-}

module Arith where 

data E a b = E (a -> b) (b -> a) 

eqRefl :: E a a 
eqRefl = E id id 

-- just to construct unique strings
data W 
data M a 

-- terms
data Var a where 
  VarW :: Var W 
  VarM :: Var (M a) 

-- expose s in the type level making sure it is a string
data Abs s e1  where 
  Abs :: (Var s) -> e1 -> Abs (Var s) e1 

data App e1 e2 = App e1 e2 
data Lit       = Lit 

data TyBase      = TyBase
data TyArr t1 t2 = TyArr t1 t2 

-- (x:ty) in G
data IN g p where 
  INOne :: IN (g,(x,ty)) (x,ty)
  INShift :: IN g0 (x,ty) -> IN (g0,a) (x,ty) 

data INEX g x where 
  INEX :: IN g (x,v) -> INEX g x 


-- G1 subseteq G2 
type SUP g1 g2 = forall a. IN g1 a -> IN g2 a 
 
-- typing derivations 
data DER g a ty where 
  DVar :: IN (g,g0) ((Var a),ty) -> DER (g,g0) (Var a) ty -- the g,g0 makes sure that env is non-empty
  DApp :: DER g a1 (TyArr ty1 ty2) -> 
            DER g a2 ty1 -> DER g (App a1 a2) ty2 
  DAbs :: DER (g,(Var a,ty1)) e ty2 -> 
            DER g (Abs (Var a) e) (TyArr ty1 ty2) 
  DLit :: DER g Lit TyBase 

-- G |- \x.x : a -> a 
test1 :: DER g (Abs (Var W) (Var W)) (TyArr ty ty) 
test1 = DAbs (DVar INOne)

-- G |- (\x.x) Lit : Lit 
test2 :: DER g (App (Abs (Var W) (Var W)) Lit) TyBase
test2 = DApp (DAbs (DVar INOne)) DLit 

-- G |- \x.\y. x y : (C -> C) -> C -> C 
test3 :: DER g (Abs (Var W) (Abs (Var (M W)) (App (Var W) (Var (M W))))) (TyArr (TyArr ty ty) (TyArr ty ty)) 
test3 = DAbs (DAbs (DApp (DVar (INShift INOne)) (DVar INOne))) 

data ISVAL e where 
 ISVALAbs :: ISVAL (Abs (Var v) e)
 ISVALLit :: ISVAL Lit 

data React e1 e2 where 
  SUBSTReact :: React (Abs (Var y) e) v 

-- evaluation 
data EDER e1 e2 where 
 -- EVar    :: IN (a,val) -> ISVAL val -> EDER c a val 
 EApp1   :: EDER e1 e1' -> EDER (App e1 e2) (App e1' e2) 
 EApp2   :: ISVAL v1 -> EDER e2 e2' -> EDER (App v1 e2) (App v1 e2')
 EAppAbs :: ISVAL v2 -> React (Abs (Var v) e) v2 -> EDER (App (Abs (Var v) e) v2) e1

-- (\x.x) 3 -> 3 
-- test4 :: EDER (App (Abs (Var W) (Var W)) Lit) Lit 
-- test4 = EAppAbs ISVALLit SUBSTEqVar 

 
-- existential 
data REDUCES e1 where 
   REDUCES :: EDER e1 e2 -> REDUCES e1 

-- data WFEnv x c g  where
--  WFOne ::  ISVAL v -> DER g v ty -> WFEnv (Var x) (c,(Var x,v)) (g,(Var x,ty))
--  WFShift :: WFEnv v c0 g0 -> WFEnv v (c0,(y,y1)) (g0,(z,z1))

-- data WFENVWRAP c g where 
--    WFENVWRAP :: (forall v ty . IN g (v,ty) -> WFEnv v c g) -> WFENVWRAP c g  


-- data INEXVAL c x where 
--   INEXVAL :: IN c (x,v) -> ISVAL v -> INEXVAL c x 

-- -- the first cool theorem! 
-- fromTEnvToEnv :: IN g (x,ty) -> WFEnv x c g ->  INEXVAL c x
-- fromTEnvToEnv INOne (WFOne isv _)  = INEXVAL INOne isv
-- fromTEnvToEnv (INShift ind1) (WFShift ind2) = 
--           case (fromTEnvToEnv ind1 ind2) of 
--              INEXVAL i isv -> INEXVAL (INShift i) isv


data ISLAMBDA v where ISLAMBDA :: ISLAMBDA (Abs (Var x) e)
data ISLIT v where ISLIT :: ISLIT Lit 

data EXISTAbs where 
  EXISTSAbs :: (Abs (Var x) e) -> EXISTAbs 

bot = bot 

canFormsLam :: ISVAL v -> DER g v (TyArr ty1 ty2) -> ISLAMBDA v 
canFormsLam ISVALAbs _ = ISLAMBDA 
-- canFormsLam ISVALLit _ = bot         <== unfortunately I cannot catch this ... requires some exhaustiveness check :-( 

canFormsLit :: ISVAL v -> DER g v TyBase -> ISLIT v 
canFormsLit ISVALLit _ = ISLIT 

data NULL

progress :: DER NULL e ty -> Either (ISVAL e) (REDUCES e) 

progress (DAbs prem)  = Left ISVALAbs 
progress (DLit)       = Left ISVALLit 
-- progress (DVar iw)    = bot             <== here is the cool trick! I cannot even wite this down! 
progress (DApp e1 e2)  = 
     case (progress e1) of 
         Right (REDUCES r1) -> Right (REDUCES (EApp1 r1))
         Left  isv1         -> case (progress e2) of 
                                    Right (REDUCES r2) -> Right (REDUCES (EApp2 isv1 r2)) 
                                    Left isv2 -> case (canFormsLam isv1 e1) of 
                                                    ISLAMBDA -> Right (REDUCES (EAppAbs isv2 SUBSTReact))


--    case fromTEnvToEnv iw (f iw) of 
--      INEXVAL i isv -> Right (REDUCES (EVar i isv)) 
-- progress (WFENVWRAP f) (DApp e1 e2) = 
--       case (progress (WFENVWRAP f) e1) of 
--          Right (REDUCES r1) -> Right (REDUCES (EApp1 r1))
--          Left  isv1         -> case (progress (WFENVWRAP f) e2) of 
--                                  Right (REDUCES r2) -> Right (REDUCES (EApp2 isv1 r2)) 
--                                  Left isv2 -> case (canFormsLam isv1 e1) of 
--                                                ISLAMBDA -> EAppAbs isv2 e1 
                              

                                  
