module Core where

import List (elemIndex)

data Module 
 = Module AnMname [Tdef] [Vdefg]

data Tdef 
  = Data (Qual Tcon) [Tbind] [Cdef]
  | Newtype (Qual Tcon) [Tbind] (Maybe Ty)

data Cdef 
  = Constr (Qual Dcon) [Tbind] [Ty]

data Vdefg 
  = Rec [Vdef]
  | Nonrec Vdef

newtype Vdef = Vdef (Qual Var,Ty,Exp)

data Exp 
  = Var (Qual Var)
  | Dcon (Qual Dcon)
  | Lit Lit
-- Why were type apps and value apps distinguished,
-- but not type lambdas and value lambdas?
  | App Exp Exp
  | Appt Exp Ty
  | Lam Bind Exp 	  
  | Let Vdefg Exp
-- Ty is new
  | Case Exp Vbind Ty [Alt] {- non-empty list -}
-- Renamed to Cast; switched order
  | Cast Exp Ty
  | Note String Exp
  | External String Ty

data Bind 
  = Vb Vbind
  | Tb Tbind

data Alt 
  = Acon (Qual Dcon) [Tbind] [Vbind] Exp
  | Alit Lit Exp
  | Adefault Exp

type Vbind = (Var,Ty)
type Tbind = (Tvar,Kind)

data Ty 
  = Tvar Tvar
  | Tcon (Qual Tcon)
  | Tapp Ty Ty
  | Tforall Tbind Ty 

data Kind 
  = Klifted
  | Kunlifted
  | Kopen
  | Karrow Kind Kind
  deriving (Eq)

data Lit 
  = Lint Integer Ty
  | Lrational Rational Ty
  | Lchar Char Ty
  | Lstring String Ty
  deriving (Eq)  -- with nearlyEqualTy 

-- new: Pnames
-- this requires at least one module name,
-- and possibly other hierarchical names
-- an alternative would be to flatten the
-- module namespace, either when printing out
-- Core or (probably preferably) in a 
-- preprocessor.
-- Maybe because the empty module name is a module name (represented as
-- Nothing.)

type Mname = Maybe AnMname
type AnMname = (Pname, [Id], Id)
type Pname = Id
type Var = Id
type Tvar = Id
type Tcon = Id
type Dcon = Id

type Qual t = (Mname,t)

qual :: AnMname -> t -> Qual t
qual mn t = (Just mn, t)

unqual :: t -> Qual t
unqual = (,) Nothing

type Id = String

--- tjc: I haven't looked at the rest of this file. ---

{- Doesn't expand out fully applied newtype synonyms
   (for which an environment is needed). -}
nearlyEqualTy t1 t2 =  eqTy [] [] t1 t2 
  where eqTy e1 e2 (Tvar v1) (Tvar v2) =
	     case (elemIndex v1 e1,elemIndex v2 e2) of
               (Just i1, Just i2) -> i1 == i2
               (Nothing, Nothing)  -> v1 == v2
               _ -> False
	eqTy e1 e2 (Tcon c1) (Tcon c2) = c1 == c2
        eqTy e1 e2 (Tapp t1a t1b) (Tapp t2a t2b) =
	      eqTy e1 e2 t1a t2a && eqTy e1 e2 t1b t2b
        eqTy e1 e2 (Tforall (tv1,tk1) t1) (Tforall (tv2,tk2) t2) =
	      tk1 == tk2 && eqTy (tv1:e1) (tv2:e2) t1 t2 
	eqTy _ _ _ _ = False
instance Eq Ty where (==) = nearlyEqualTy


subKindOf :: Kind -> Kind -> Bool
_ `subKindOf` Kopen = True
k1 `subKindOf` k2 = k1 == k2  -- doesn't worry about higher kinds

instance Ord Kind where (<=) = subKindOf

baseKind :: Kind -> Bool
baseKind (Karrow _ _ ) = False
baseKind _ = True

isPrimVar (Just mn,_) = mn == primMname
isPrimVar _ = False

primMname = mkBaseMname "Prim"
errMname  = mkBaseMname "Err"
mkBaseMname :: Id -> AnMname
mkBaseMname mn = (basePkg, ghcPrefix, mn)
basePkg = "base"
mainPkg = "main"
ghcPrefix = ["GHC"]
mainPrefix = []
baseMname = mkBaseMname "Base"
mainVar = qual mainMname "main"
mainMname = (mainPkg, mainPrefix, "Main")

tcArrow :: Qual Tcon
tcArrow = (Just primMname, "ZLzmzgZR")

tArrow :: Ty -> Ty -> Ty
tArrow t1 t2 = Tapp (Tapp (Tcon tcArrow) t1) t2


ktArrow :: Kind
ktArrow = Karrow Kopen (Karrow Kopen Klifted)

{- Unboxed tuples -}

-- tjc: not sure whether anything that follows is right

maxUtuple :: Int
maxUtuple = 100

tcUtuple :: Int -> Qual Tcon
tcUtuple n = (Just primMname,"Z"++ (show n) ++ "H")

ktUtuple :: Int -> Kind
ktUtuple n = foldr Karrow Kunlifted (replicate n Kopen)

tUtuple :: [Ty] -> Ty
tUtuple ts = foldl Tapp (Tcon (tcUtuple (length ts))) ts 

isUtupleTy :: Ty -> Bool
isUtupleTy (Tapp t _) = isUtupleTy t
isUtupleTy (Tcon tc) = tc `elem` [tcUtuple n | n <- [1..maxUtuple]]
isUtupleTy _ = False

dcUtuple :: Int -> Qual Dcon
dcUtuple n = (Just primMname,"ZdwZ" ++ (show n) ++ "H")

isUtupleDc :: Qual Dcon -> Bool
isUtupleDc dc = dc `elem` [dcUtuple n | n <- [1..maxUtuple]]

dcUtupleTy :: Int -> Ty
dcUtupleTy n = 
     foldr ( \tv t -> Tforall (tv,Kopen) t)
           (foldr ( \tv t -> tArrow (Tvar tv) t)
		  (tUtuple (map Tvar tvs)) tvs) 
           tvs
     where tvs = map ( \i -> ("a" ++ (show i))) [1..n] 

utuple :: [Ty] -> [Exp] -> Exp
utuple ts es = foldl App (foldl Appt (Dcon (dcUtuple (length es))) ts) es


