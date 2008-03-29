module Core where

import List (elemIndex)

data Module 
 = Module AnMname [Tdef] [Vdefg]

data Tdef 
  = Data (Qual Tcon) [Tbind] [Cdef]
  | Newtype (Qual Tcon) [Tbind] Axiom (Maybe Ty)

data Cdef 
  = Constr (Qual Dcon) [Tbind] [Ty]

-- Newtype coercion
type Axiom = (Qual Tcon, Kind)

data Vdefg 
  = Rec [Vdef]
  | Nonrec Vdef

newtype Vdef = Vdef (Qual Var,Ty,Exp)

data Exp 
  = Var (Qual Var)
  | Dcon (Qual Dcon)
  | Lit Lit
  | App Exp Exp
  | Appt Exp Ty
  | Lam Bind Exp 	  
  | Let Vdefg Exp
  | Case Exp Vbind Ty [Alt] {- non-empty list -}
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
  | Keq Ty Ty

data Lit = Literal CoreLit Ty
  deriving Eq   -- with nearlyEqualTy 

data CoreLit = Lint Integer
  | Lrational Rational
  | Lchar Char
  | Lstring String 
  deriving Eq

-- Right now we represent module names as triples:
-- (package name, hierarchical names, leaf name)
-- An alternative to this would be to flatten the
-- module namespace, either when printing out
-- Core or (probably preferably) in a 
-- preprocessor.
-- The empty module name (as in an unqualified name)
-- is represented as Nothing.

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

eqKind :: Kind -> Kind -> Bool
eqKind Klifted Klifted = True
eqKind Kunlifted Kunlifted = True
eqKind Kopen Kopen = True
eqKind (Karrow k1 k2) (Karrow l1 l2) = k1 `eqKind` l1
                                   &&  k2 `eqKind` l2
eqKind _ _ = False -- no Keq kind is ever equal to any other...
                   -- maybe ok for now?

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
	      tk1 `eqKind` tk2 && eqTy (tv1:e1) (tv2:e2) t1 t2 
	eqTy _ _ _ _ = False
instance Eq Ty where (==) = nearlyEqualTy


subKindOf :: Kind -> Kind -> Bool
_ `subKindOf` Kopen = True
k1 `subKindOf` k2 = k1 `eqKind` k2  -- doesn't worry about higher kinds

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


