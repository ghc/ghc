{-# OPTIONS -fno-warn-missing-signatures #-}
module Language.Core.Core where

import Language.Core.Encoding

import Data.Generics
import Data.List (elemIndex)
import Data.Char

data Module 
 = Module AnMname [Tdef] [Vdefg]
  deriving (Data, Typeable)

data Tdef 
  = Data (Qual Tcon) [Tbind] [Cdef]
    -- type constructor; coercion name; type arguments; type rep
    -- If we have: (Newtype tc co tbs (Just t))
    -- there is an implicit axiom:
    --  co tbs :: tc tbs :=: t
  | Newtype (Qual Tcon) (Qual Tcon) [Tbind] Ty
 deriving (Data, Typeable)

data Cdef 
  = Constr (Qual Dcon) [Tbind] [Ty]
  deriving (Data, Typeable)

data Vdefg 
  = Rec [Vdef]
  | Nonrec Vdef
  deriving (Data, Typeable)

newtype Vdef = Vdef (Qual Var,Ty,Exp)
  deriving (Data, Typeable)

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
  deriving (Data, Typeable)

data Bind 
  = Vb Vbind
  | Tb Tbind
  deriving (Data, Typeable)

data Alt 
  = Acon (Qual Dcon) [Tbind] [Vbind] Exp
  | Alit Lit Exp
  | Adefault Exp
  deriving (Data, Typeable)

type Vbind = (Var,Ty)
type Tbind = (Tvar,Kind)

data Ty 
  = Tvar Tvar
  | Tcon (Qual Tcon)
  | Tapp Ty Ty
  | Tforall Tbind Ty 
-- Wired-in coercions:
-- These are primitive tycons in GHC, but in ext-core,
-- we make them explicit, to make the typechecker
-- somewhat more clear. 
  | TransCoercion Ty Ty
  | SymCoercion Ty
  | UnsafeCoercion Ty Ty
  | InstCoercion Ty Ty
  | LeftCoercion Ty
  | RightCoercion Ty
  deriving (Data, Typeable)

data Kind 
  = Klifted
  | Kunlifted
  | Kopen
  | Karrow Kind Kind
  | Keq Ty Ty
  deriving (Data, Typeable)

-- A CoercionKind isn't really a Kind at all, but rather,
-- corresponds to an arbitrary user-declared axiom.
-- A tycon whose CoercionKind is (DefinedCoercion <tbs> (from, to))
-- represents a tycon with arity (length tbs), whose kind is
-- (from :=: to) (modulo substituting type arguments.
-- It's not a Kind because a coercion must always be fully applied:
-- whenever we see a tycon that has such a CoercionKind, it must
-- be fully applied if it's to be assigned an actual Kind.
-- So, a CoercionKind *only* appears in the environment (mapping
-- newtype axioms onto CoercionKinds).
-- Was that clear??
data CoercionKind = 
   DefinedCoercion [Tbind] (Ty,Ty)

-- The type constructor environment maps names that are
-- either type constructors or coercion names onto either
-- kinds or coercion kinds.
data KindOrCoercion = Kind Kind | Coercion CoercionKind

data Lit = Literal CoreLit Ty
  deriving (Data, Typeable, Eq)

data CoreLit = Lint Integer
  | Lrational Rational
  | Lchar Char
  | Lstring String 
  deriving (Data, Typeable, Eq)

-- Right now we represent module names as triples:
-- (package name, hierarchical names, leaf name)
-- An alternative to this would be to flatten the
-- module namespace, either when printing out
-- Core or (probably preferably) in a 
-- preprocessor.
-- We represent the empty module name (as in an unqualified name)
-- with Nothing.

type Mname = Maybe AnMname
newtype AnMname = M (Pname, [Id], Id)
  deriving (Eq, Ord, Data, Typeable)
newtype Pname = P Id
  deriving (Eq, Ord, Data, Typeable)
type Var = Id
type Tvar = Id
type Tcon = Id
type Dcon = Id

type Qual t = (Mname,t)

qual :: AnMname -> t -> Qual t
qual mn t = (Just mn, t)

unqual :: t -> Qual t
unqual = (,) Nothing

getModule :: Qual t -> Mname
getModule = fst

type Id = String

eqKind :: Kind -> Kind -> Bool
eqKind Klifted Klifted = True
eqKind Kunlifted Kunlifted = True
eqKind Kopen Kopen = True
eqKind (Karrow k1 k2) (Karrow l1 l2) = k1 `eqKind` l1
                                   &&  k2 `eqKind` l2
eqKind (Keq t1 t2) (Keq u1 u2) = t1 == u1
                              && t2 == u2
eqKind _ _ = False

splitTyConApp_maybe :: Ty -> Maybe (Qual Tcon,[Ty])
splitTyConApp_maybe (Tvar _) = Nothing
splitTyConApp_maybe (Tcon t) = Just (t,[])
splitTyConApp_maybe (Tapp rator rand) = 
   case (splitTyConApp_maybe rator) of
      Just (r,rs) -> Just (r,rs++[rand])
      Nothing     -> case rator of
                       Tcon tc -> Just (tc,[rand])
                       _       -> Nothing
splitTyConApp_maybe (Tforall _ _) = Nothing
-- coercions
splitTyConApp_maybe _ = Nothing

-- This used to be called nearlyEqualTy, but now that
-- we don't need to expand newtypes anymore, it seems
-- like equality to me!
equalTy :: Ty -> Ty -> Bool
equalTy t1 t2 =  eqTy [] [] t1 t2 
  where eqTy e1 e2 (Tvar v1) (Tvar v2) =
	     case (elemIndex v1 e1,elemIndex v2 e2) of
               (Just i1, Just i2) -> i1 == i2
               (Nothing, Nothing)  -> v1 == v2
               _ -> False
	eqTy _ _ (Tcon c1) (Tcon c2) = c1 == c2
        eqTy e1 e2 (Tapp t1a t1b) (Tapp t2a t2b) =
	      eqTy e1 e2 t1a t2a && eqTy e1 e2 t1b t2b
        eqTy e1 e2 (Tforall (tv1,tk1) b1) (Tforall (tv2,tk2) b2) =
	      tk1 `eqKind` tk2 && eqTy (tv1:e1) (tv2:e2) b1 b2 
	eqTy _ _ _ _ = False
instance Eq Ty where (==) = equalTy


subKindOf :: Kind -> Kind -> Bool
_ `subKindOf` Kopen = True
(Karrow a1 r1) `subKindOf` (Karrow a2 r2) = 
  a2 `subKindOf` a1 && (r1 `subKindOf` r2)
k1 `subKindOf` k2 = k1 `eqKind` k2  -- doesn't worry about higher kinds

baseKind :: Kind -> Bool
baseKind (Karrow _ _ ) = False
baseKind _ = True

isPrimVar (Just mn,_) = mn == primMname
isPrimVar _ = False

primMname = mkPrimMname "Prim"
errMname  = mkBaseMname "Err"
mkBaseMname,mkPrimMname :: Id -> AnMname
mkBaseMname mn = M (basePkg, ghcPrefix, mn)
mkPrimMname mn = M (primPkg, ghcPrefix, mn)
basePkg = P "base"
mainPkg = P "main"
primPkg = P $ zEncodeString "ghc-prim"
ghcPrefix = ["GHC"]
mainPrefix = []
baseMname = error "Somebody called baseMname!" -- mkBaseMname "Base"
boolMname = mkPrimMname "Bool"
mainVar = qual mainMname "main"
wrapperMainVar = qual wrapperMainMname "main"
mainMname = M (mainPkg, mainPrefix, "Main")
wrapperMainMname = M (mainPkg, mainPrefix, "ZCMain")
wrapperMainAnMname = Just wrapperMainMname

dcTrue :: Dcon
dcTrue = "True"
dcFalse :: Dcon
dcFalse = "False"

tcArrow :: Qual Tcon
tcArrow = (Just primMname, "ZLzmzgZR")

tArrow :: Ty -> Ty -> Ty
tArrow t1 t2 = Tapp (Tapp (Tcon tcArrow) t1) t2

mkFunTy :: Ty -> Ty -> Ty
mkFunTy randTy resultTy =
  Tapp (Tapp (Tcon tcArrow) randTy) resultTy

ktArrow :: Kind
ktArrow = Karrow Kopen (Karrow Kopen Klifted)

{- Unboxed tuples -}

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
isUtupleTy (Tcon tc) = 
  case tc of
    (Just pm, 'Z':rest) | pm == primMname && last rest == 'H' -> 
       let mid = take ((length rest) - 1) rest in
         all isDigit mid && (let num = read mid in
                               1 <= num && num <= maxUtuple)
    _ -> False
-- The above is ugly, but less ugly than this:
--tc `elem` [tcUtuple n | n <- [1..maxUtuple]]
isUtupleTy _ = False

dcUtuple :: Int -> Qual Dcon
-- TODO: Seems like Z2H etc. appears in ext-core files,
-- not $wZ2H etc. Is this right?
dcUtuple n = (Just primMname,"Z" ++ (show n) ++ "H")

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

---- snarfed from GHC's CoreSyn
flattenBinds :: [Vdefg] -> [Vdef]	-- Get all the lhs/rhs pairs
flattenBinds (Nonrec vd : binds) = vd : flattenBinds binds
flattenBinds (Rec prs1   : binds) = prs1 ++ flattenBinds binds
flattenBinds []			  = []

unitMname :: AnMname
unitMname = mkPrimMname "Unit"
