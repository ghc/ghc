{-# OPTIONS -fglasgow-exts #-}
	-- Need GlaExts for the nested forall in defn of Q
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Syntax
-- Copyright   :  (c) The University of Glasgow 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Abstract syntax definitions for Template Haskell.
--
-----------------------------------------------------------------------------

module Language.Haskell.TH.Syntax(
	Quasi(..), Lift(..), 

	Q, runQ, 
	report,	recover, reify,
	currentModule, runIO,

	-- Names
	Name(..), mkName, newName, nameBase,

	-- The algebraic data types
	Dec(..), Exp(..), Con(..), Type(..), Cxt, Match(..), 
	Clause(..), Body(..), Stmt(..), Range(..),
	Lit(..), Pat(..), FieldExp, FieldPat, 
	Strict(..), Foreign(..), Callconv(..), Safety(..),
	StrictType, VarStrictType, 
	Info(..), 
	Fixity(..), FixityDirection(..), defaultFixity, maxPrecedence,

	-- Internal functions
	returnQ, bindQ, sequenceQ,
	NameFlavour(..), NameSpace (..), 
	mkNameG_v, mkNameG_d, mkNameG_tc, mkNameU,
	OccName, mkOccName, occString,
	ModName, mkModName, modString
    ) where

import Data.PackedString
import GHC.Base		( Int(..), Int#, (<#), (==#) )

import IO		( hPutStrLn, stderr )
import Data.IORef
import GHC.IOBase	( unsafePerformIO )

-----------------------------------------------------
--
--		The Quasi class
--
-----------------------------------------------------

class Monad m => Quasi m where
	-- Fresh names
  qNewName :: String -> m Name

	-- Error reporting and recovery
  qReport  :: Bool -> String -> m ()	-- Report an error (True) or warning (False)
					-- ...but carry on; use 'fail' to stop
  qRecover :: m a -> m a -> m a		-- Recover from the monadic 'fail'
					-- The first arg is the error handler
 
	-- Inspect the type-checker's environment
  qReify :: Name -> m Info
  qCurrentModule :: m String

	-- Input/output (dangerous)
  qRunIO :: IO a -> m a


-----------------------------------------------------
--	The IO instance of Quasi
-- 
--  This instance is used only when running a Q
--  computation in the IO monad, usually just to
--  print the result.  There is no interesting
--  type environment, so reification isn't going to
--  work.
--
-----------------------------------------------------

instance Quasi IO where
  qNewName s = do { n <- readIORef counter
                 ; writeIORef counter (n+1)
                 ; return (mkNameU s n) }

  qReport True  msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)
  qReport False msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)

  qReify v       = badIO "reify"
  qCurrentModule = badIO "currentModule"
  qRecover a b   = badIO "recover"	-- Maybe we could fix this?

  qRunIO m = m
  
badIO :: String -> IO a
badIO op = do	{ qReport True ("Can't do `" ++ op ++ "' in the IO monad")
		; fail "Template Haskell failure" }

-- Global variable to generate unique symbols
counter :: IORef Int
{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0)


-----------------------------------------------------
--
--		The Q monad
--
-----------------------------------------------------

newtype Q a = Q { unQ :: forall m. Quasi m => m a }

runQ :: Quasi m => Q a -> m a
runQ (Q m) = m

instance Monad Q where
  return x   = Q (return x)
  Q m >>= k  = Q (m >>= \x -> unQ (k x))
  Q m >> Q n = Q (m >> n)
  fail s     = Q (fail s)

----------------------------------------------------
-- Packaged versions for the programmer, hiding the Quasi-ness
newName :: String -> Q Name
newName s = Q (qNewName s)

report  :: Bool -> String -> Q ()
report b s = Q (qReport b s)

recover :: Q a -> Q a -> Q a
recover (Q r) (Q m) = Q (qRecover r m)

reify :: Name -> Q Info
reify v = Q (qReify v)

currentModule :: Q String
currentModule = Q qCurrentModule

runIO :: IO a -> Q a
runIO m = Q (qRunIO m)

instance Quasi Q where
  qNewName        = newName
  qReport 	 = report
  qRecover  	 = recover 
  qReify    	 = reify
  qCurrentModule = currentModule
  qRunIO         = runIO


----------------------------------------------------
-- The following operations are used solely in DsMeta when desugaring brackets
-- They aren't necessary for the user, who can use ordinary return and (>>=) etc

returnQ :: a -> Q a
returnQ = return

bindQ :: Q a -> (a -> Q b) -> Q b
bindQ = (>>=)

sequenceQ :: [Q a] -> Q [a]
sequenceQ = sequence


-----------------------------------------------------
--
--		The Lift class
--
-----------------------------------------------------

class Lift t where
  lift :: t -> Q Exp
  
instance Lift Integer where
  lift x = return (LitE (IntegerL x))

instance Lift Int where
  lift x= return (LitE (IntegerL (fromIntegral x)))

instance Lift Char where
  lift x = return (LitE (CharL x))

instance Lift Bool where
  lift True  = return (ConE trueName)
  lift False = return (ConE falseName)

instance Lift a => Lift [a] where
  lift xs = do { xs' <- mapM lift xs; return (ListE xs') }

-- TH has a special form for literal strings,
-- which we should take advantage of.
-- NB: the lhs of the rule has no args, so that
--     the rule will apply to a 'lift' all on its own
--     which happens to be the way the type checker 
--     creates it.
{-# RULES "TH:liftString" lift = \s -> return (LitE (StringL s)) #-}


trueName, falseName :: Name
trueName  = mkNameG DataName "GHC.Base" "True"
falseName = mkNameG DataName "GHC.Base" "Frue"


-----------------------------------------------------
--		Names and uniques 
-----------------------------------------------------

type ModName = PackedString	-- Module name
mkModName :: String -> ModName
mkModName s = packString s

modString :: ModName -> String
modString m = unpackPS m

-----------------------------------------------------
--		OccName
-----------------------------------------------------

-- An OccName (occurrence name) records which name space it is from
type OccName = PackedString


mkOccName :: String -> OccName
mkOccName s = packString s

occString :: OccName -> String
occString occ = unpackPS occ



-----------------------------------------------------
--		 Names
-----------------------------------------------------

-- For "global" names (NameG) we need a totally unique name,
-- so we must include the name-space of the thing
--
-- For unique-numbered things (NameU), we've got a unique reference
-- anyway, so no need for name space
--
-- For dynamically bound thing (NameS) we probably want them to 
-- in a context-dependent way, so again we don't want the name
-- space.  For example:
--	let v = mkName "T" in [| data $v = $v |]
-- Here we use the same Name for both type constructor and data constructor

data Name = Name OccName NameFlavour

data NameFlavour
  = NameS 			-- Just a string; dynamically bound
  | NameU Int#			-- A unique local name
  | NameG NameSpace ModName	-- An original name (occurrences only, not binders)
				-- Need the namespace too to be sure which 
				-- thing we are naming

data NameSpace = VarName	-- Variables
	       | DataName	-- Data constructors 
	       | TcClsName	-- Type constructors and classes; Haskell has them
				-- in the same name space for now.
	       deriving( Eq, Ord )

type Uniq = Int

nameBase :: Name -> String
nameBase (Name occ _) = occString occ

mkName :: String -> Name
mkName s = Name (mkOccName s) NameS

mkNameU :: String -> Uniq -> Name	-- Only used internally
mkNameU s (I# u) = Name (mkOccName s) (NameU u)

mkNameG :: NameSpace -> String -> String -> Name	-- Used for 'x etc, but not available
mkNameG ns mod occ 				-- to the programmer
  = Name (mkOccName occ) (NameG ns (mkModName mod))

mkNameG_v  = mkNameG VarName
mkNameG_tc = mkNameG TcClsName
mkNameG_d  = mkNameG DataName

instance Eq Name where
  v1 == v2 = cmpEq (v1 `compare` v2)

instance Ord Name where
  (Name o1 f1) `compare` (Name o2 f2) = (f1 `compare` f2)   `thenCmp`
				        (o1 `compare` o2)

instance Eq NameFlavour where
  f1 == f2 = cmpEq (f1 `compare` f2)

instance Ord NameFlavour where
  NameS `compare` NameS = EQ
  NameS `compare` other = LT

  (NameU _)  `compare` NameS = GT
  (NameU u1) `compare` (NameU u2) | u1  <# u2 = LT
				  | u1 ==# u2 = EQ
				  | otherwise = GT
  (NameU _)  `compare` other = LT

  (NameG ns1 m1) `compare` (NameG ns2 m2)  = (ns1 `compare` ns2) `thenCmp`
					     (m1 `compare` m2)
  (NameG _ _)    `compare` other	   = GT

instance Show Name where
  show (Name occ (NameU u))    = occString occ ++ "_" ++ show (I# u)
  show (Name occ NameS)        = occString occ
  show (Name occ (NameG ns m)) = modString m ++ "." ++ occString occ


-----------------------------------------------------
--
--	The Info returned by reification
--
-----------------------------------------------------

data Info 
  = ClassI Dec
  | ClassOpI
	Name	-- The class op itself
	Type 	-- Type of the class-op (fully polymoprhic)
	Name 	-- Name of the parent class
	Fixity

  | TyConI Dec
  | DataConI 
	Name	-- The data con itself
	Type 	-- Type of the constructor (fully polymorphic)
	Name 	-- Name of the parent TyCon
	Fixity

  | VarI 
	Name	-- The variable itself
	Type 
	(Maybe Dec)	-- Nothing for lambda-bound variables, and 
			-- for anything else TH can't figure out
			-- E.g. [| let x = 1 in $(do { d <- reify 'x; .. }) |]
	Fixity

  | TyVarI 	-- Scoped type variable
	Name
	Type	-- What it is bound to

data Fixity 	     = Fixity Int FixityDirection deriving( Eq )
data FixityDirection = InfixL | InfixR | InfixN   deriving( Eq )

maxPrecedence = (9::Int)
defaultFixity = Fixity maxPrecedence InfixL


-----------------------------------------------------
--
--	The main syntax data types
--
-----------------------------------------------------

data Lit = CharL Char 
         | StringL String 
         | IntegerL Integer     -- Used for overloaded and non-overloaded
                                -- literals. We don't have a good way to
                                -- represent non-overloaded literals at
                                -- the moment. Maybe that doesn't matter?
         | RationalL Rational   -- Ditto
         | IntPrimL Integer
         | FloatPrimL Rational
         | DoublePrimL Rational
    deriving( Show, Eq )

    -- We could add Int, Float, Double etc, as we do in HsLit, 
    -- but that could complicate the
    -- suppposedly-simple TH.Syntax literal type

data Pat 
  = LitP Lit                      -- { 5 or 'c' }
  | VarP Name                   -- { x }
  | TupP [Pat]                    -- { (p1,p2) }
  | ConP Name [Pat]             -- data T1 = C1 t1 t2; {C1 p1 p1} = e 
  | TildeP Pat                    -- { ~p }
  | AsP Name Pat                -- { x @ p }
  | WildP                         -- { _ }
  | RecP Name [FieldPat]        -- f (Pt { pointx = x }) = g x
  | ListP [ Pat ]                 -- { [1,2,3] }
  deriving( Show, Eq )

type FieldPat = (Name,Pat)

data Match = Match Pat Body [Dec]
                                    -- case e of { pat -> body where decs } 
    deriving( Show, Eq )
data Clause = Clause [Pat] Body [Dec]
                                    -- f { p1 p2 = body where decs }
    deriving( Show, Eq )
 
data Exp 
  = VarE Name                        -- { x }
  | ConE Name                        -- data T1 = C1 t1 t2; p = {C1} e1 e2  
  | LitE Lit                           -- { 5 or 'c'}
  | AppE Exp Exp                       -- { f x }

  | InfixE (Maybe Exp) Exp (Maybe Exp) -- {x + y} or {(x+)} or {(+ x)} or {(+)}
    -- It's a bit gruesome to use an Exp as the
    -- operator, but how else can we distinguish
    -- constructors from non-constructors?
    -- Maybe there should be a var-or-con type?
    -- Or maybe we should leave it to the String itself?

  | LamE [Pat] Exp                     -- { \ p1 p2 -> e }
  | TupE [Exp]                         -- { (e1,e2) }  
  | CondE Exp Exp Exp                  -- { if e1 then e2 else e3 }
  | LetE [Dec] Exp                     -- { let x=e1;   y=e2 in e3 }
  | CaseE Exp [Match]                  -- { case e of m1; m2 }
  | DoE [Stmt]                         -- { do { p <- e1; e2 }  }
  | CompE [Stmt]                       -- { [ (x,y) | x <- xs, y <- ys ] }
  | ArithSeqE Range                    -- { [ 1 ,2 .. 10 ] }
  | ListE [ Exp ]                      -- { [1,2,3] }
  | SigE Exp Type                      -- e :: t
  | RecConE Name [FieldExp]            -- { T { x = y, z = w } }
  | RecUpdE Exp [FieldExp]             -- { (f x) { z = w } }
  deriving( Show, Eq )

type FieldExp = (Name,Exp)

-- Omitted: implicit parameters

data Body
  = GuardedB [(Exp,Exp)]     -- f p { | e1 = e2 | e3 = e4 } where ds
  | NormalB Exp              -- f p { = e } where ds
  deriving( Show, Eq )

data Stmt
  = BindS Pat Exp
  | LetS [ Dec ]
  | NoBindS Exp
  | ParS [[Stmt]]
  deriving( Show, Eq )

data Range = FromR Exp | FromThenR Exp Exp
           | FromToR Exp Exp | FromThenToR Exp Exp Exp
          deriving( Show, Eq )
  
data Dec 
  = FunD Name [Clause]            -- { f p1 p2 = b where decs }
  | ValD Pat Body [Dec]           -- { p = b where decs }
  | DataD Cxt Name [Name] 
         [Con] [Name]             -- { data Cxt x => T x = A x | B (T x)
                                  --       deriving (Z,W)}
  | NewtypeD Cxt Name [Name] 
         Con [Name]               -- { newtype Cxt x => T x = A (B x)
                                  --       deriving (Z,W)}
  | TySynD Name [Name] Type       -- { type T x = (x,x) }
  | ClassD Cxt Name [Name] [Dec]  -- { class Eq a => Ord a where ds }
  | InstanceD Cxt Type [Dec]      -- { instance Show w => Show [w]
                                  --       where ds }
  | SigD Name Type                -- { length :: [a] -> Int }
  | ForeignD Foreign
  deriving( Show, Eq )

data Foreign = ImportF Callconv Safety String Name Type
             | ExportF Callconv        String Name Type
         deriving( Show, Eq )

data Callconv = CCall | StdCall
          deriving( Show, Eq )

data Safety = Unsafe | Safe | Threadsafe
        deriving( Show, Eq )

type Cxt = [Type]    -- (Eq a, Ord b)

data Strict = IsStrict | NotStrict
         deriving( Show, Eq )

data Con = NormalC Name [StrictType]
         | RecC Name [VarStrictType]
         | InfixC StrictType Name StrictType
         deriving( Show, Eq )

type StrictType = (Strict, Type)
type VarStrictType = (Name, Strict, Type)

data Module = Module [ Dec ] 
             deriving( Show, Eq )

-- FIXME: Why this special status for "List" (even tuples might be handled
--      differently)? -=chak
data Type = ForallT [Name] Cxt Type   -- forall <vars>. <ctxt> -> <type>
          | VarT Name                 -- a
          | ConT Name                 -- T
          | TupleT Int                -- (,), (,,), etc.
          | ArrowT                    -- ->
          | ListT                     -- []
          | AppT Type Type            -- T a b
      deriving( Show, Eq )

-----------------------------------------------------
--		Internal helper functions
-----------------------------------------------------

cmpEq :: Ordering -> Bool
cmpEq EQ = True
cmpEq _  = False

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o2 = o2
thenCmp o1 o2 = o1

