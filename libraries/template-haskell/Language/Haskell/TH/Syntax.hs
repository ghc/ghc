{-# OPTIONS_GHC -fglasgow-exts #-}
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
	Name(..), mkName, newName, nameBase, nameModule,

	-- The algebraic data types
	Dec(..), Exp(..), Con(..), Type(..), Cxt, Match(..), 
	Clause(..), Body(..), Guard(..), Stmt(..), Range(..),
	Lit(..), Pat(..), FieldExp, FieldPat, 
	Strict(..), Foreign(..), Callconv(..), Safety(..),
	StrictType, VarStrictType, FunDep(..),
	Info(..), 
	Fixity(..), FixityDirection(..), defaultFixity, maxPrecedence,

	-- Internal functions
	returnQ, bindQ, sequenceQ,
	NameFlavour(..), NameSpace (..), 
	mkNameG_v, mkNameG_d, mkNameG_tc, mkNameL, mkNameU,
 	tupleTypeName, tupleDataName,
	OccName, mkOccName, occString,
	ModName, mkModName, modString
    ) where

import Data.PackedString
import GHC.Base		( Int(..), Int#, (<#), (==#) )

import IO		( hPutStrLn, stderr )
import Data.IORef
import GHC.IOBase	( unsafePerformIO )
import Control.Monad (liftM)

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

-- | 'reify' looks up information about the 'Name'
reify :: Name -> Q Info
reify v = Q (qReify v)

-- | 'currentModule' gives you the name of the module in which this 
-- computation is spliced.
currentModule :: Q String
currentModule = Q qCurrentModule

-- |The 'runIO' function lest you run an I\/O computation in the 'Q' monad.
-- Take care: you are guaranteed the ordering of calls to 'runIO' within 
-- a single 'Q' computation, but not about the order in which splices are run.  
--
-- Note: for various murky reasons, stdout and stderr handles are not 
-- necesarily flushed when the  compiler finishes running, so you should
-- flush them yourself.
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
-- They are not necessary for the user, who can use ordinary return and (>>=) etc

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

instance (Lift a, Lift b) => Lift (a, b) where
  lift (a, b)
    = liftM TupE $ sequence [lift a, lift b]

instance (Lift a, Lift b, Lift c) => Lift (a, b, c) where
  lift (a, b, c)
    = liftM TupE $ sequence [lift a, lift b, lift c]

instance (Lift a, Lift b, Lift c, Lift d) => Lift (a, b, c, d) where
  lift (a, b, c, d)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d]

instance (Lift a, Lift b, Lift c, Lift d, Lift e)
      => Lift (a, b, c, d, e) where
  lift (a, b, c, d, e)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e]

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f)
      => Lift (a, b, c, d, e, f) where
  lift (a, b, c, d, e, f)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e, lift f]

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g)
      => Lift (a, b, c, d, e, f, g) where
  lift (a, b, c, d, e, f, g)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e, lift f, lift g]

-- TH has a special form for literal strings,
-- which we should take advantage of.
-- NB: the lhs of the rule has no args, so that
--     the rule will apply to a 'lift' all on its own
--     which happens to be the way the type checker 
--     creates it.
{-# RULES "TH:liftString" lift = \s -> return (LitE (StringL s)) #-}


trueName, falseName :: Name
trueName  = mkNameG DataName "GHC.Base" "True"
falseName = mkNameG DataName "GHC.Base" "False"


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
  = NameS 			-- An unqualified name; dynamically bound
  | NameQ ModName		-- A qualified name; dynamically bound

  | NameU Int#			-- A unique local name

	-- The next two are for lexically-scoped names that
	-- are bound *outside* the TH syntax tree, 
	-- either globally (NameG) or locally (NameL)
	-- e.g. f x = $(h [| (map, x) |]
	--      The 'map' will be a NameG, and 'x' wil be a NameL
	-- These Names should never appear in a binding position in a TH syntax tree

  | NameL Int#			-- 
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

nameModule :: Name -> Maybe String
nameModule (Name _ (NameQ m))   = Just (modString m)
nameModule (Name _ (NameG _ m)) = Just (modString m)
nameModule other_name		= Nothing

mkName :: String -> Name
-- The string can have a '.', thus "Foo.baz",
-- giving a dynamically-bound qualified name,
-- in which case we want to generate a NameQ
--
-- Parse the string to see if it has a "." in it
-- so we know whether to generate a qualified or unqualified name
-- It's a bit tricky because we need to parse 
--	Foo.Baz.x as Qual Foo.Baz x
-- So we parse it from back to front
mkName str
  = split [] (reverse str)
  where
    split occ []        = Name (mkOccName occ) NameS
    split occ ('.':rev)	= Name (mkOccName occ) (NameQ (mkModName (reverse rev)))
    split occ (c:rev)   = split (c:occ) rev

mkNameU :: String -> Uniq -> Name	-- Only used internally
mkNameU s (I# u) = Name (mkOccName s) (NameU u)

mkNameL :: String -> Uniq -> Name	-- Only used internally
mkNameL s (I# u) = Name (mkOccName s) (NameL u)

mkNameG :: NameSpace -> String -> String -> Name	-- Used for 'x etc, but not available
mkNameG ns mod occ 					-- to the programmer
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
	-- NameS < NameQ < NameU < NameL < NameG
  NameS `compare` NameS = EQ
  NameS `compare` other = LT

  (NameQ _)  `compare` NameS      = GT
  (NameQ m1) `compare` (NameQ m2) = m1 `compare` m2
  (NameQ _)  `compare` other      = LT

  (NameU _)  `compare` NameS      = GT
  (NameU _)  `compare` (NameQ _)  = GT
  (NameU u1) `compare` (NameU u2) | u1  <# u2 = LT
				  | u1 ==# u2 = EQ
				  | otherwise = GT
  (NameU _)  `compare` other = LT

  (NameL _)  `compare` NameS      = GT
  (NameL _)  `compare` (NameQ _)  = GT
  (NameL _)  `compare` (NameU _)  = GT
  (NameL u1) `compare` (NameL u2) | u1  <# u2 = LT
				  | u1 ==# u2 = EQ
				  | otherwise = GT
  (NameL _)  `compare` other      = LT

  (NameG ns1 m1) `compare` (NameG ns2 m2) = (ns1 `compare` ns2) `thenCmp`
					    (m1 `compare` m2)
  (NameG _ _)    `compare` other	  = GT

instance Show Name where
	-- For now, we make the NameQ and NameG print the same, 
	-- and ditto NameU and NameL.  We may well want to
	-- distinguish them in the end.
  show (Name occ NameS)        = occString occ
  show (Name occ (NameU u))    = occString occ ++ "_" ++ show (I# u)
  show (Name occ (NameQ m))    = modString m ++ "." ++ occString occ
  show (Name occ (NameL u))    = occString occ ++ "_" ++ show (I# u)
  show (Name occ (NameG ns m)) = modString m   ++ "." ++ occString occ


-- 	Tuple data and type constructors
tupleDataName  :: Int -> Name	-- Data constructor
tupleTypeName :: Int -> Name 	-- Type constructor

tupleDataName 0 = mk_tup_name 0 DataName 
tupleDataName 1 = error "tupleDataName 1"
tupleDataName n = mk_tup_name (n-1) DataName 

tupleTypeName 0 = mk_tup_name 0 TcClsName 
tupleTypeName 1 = error "tupleTypeName 1"
tupleTypeName n = mk_tup_name (n-1) TcClsName 

mk_tup_name n_commas space
  = Name occ (NameG space tup_mod)
  where
    occ = mkOccName ('(' : replicate n_commas ',' ++ ")")
    tup_mod = mkModName "Data.Tuple"




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

  | PrimTyConI 	-- Ones that can't be expressed with a data type 
		-- decl, such as (->), Int#
	Name 
	Int	-- Arity
	Bool	-- False => lifted type; True => unlifted

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
  | InfixP Pat Name Pat           -- foo ({x :+ y}) = e 
  | TildeP Pat                    -- { ~p }
  | AsP Name Pat                -- { x @ p }
  | WildP                         -- { _ }
  | RecP Name [FieldPat]        -- f (Pt { pointx = x }) = g x
  | ListP [ Pat ]                 -- { [1,2,3] }
  | SigP Pat Type                 -- p :: t
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
  = GuardedB [(Guard,Exp)]   -- f p { | e1 = e2 | e3 = e4 } where ds
  | NormalB Exp              -- f p { = e } where ds
  deriving( Show, Eq )

data Guard
  = NormalG Exp
  | PatG [Stmt]
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
  | ClassD Cxt Name [Name] [FunDep] [Dec]
                                  -- { class Eq a => Ord a where ds }
  | InstanceD Cxt Type [Dec]      -- { instance Show w => Show [w]
                                  --       where ds }
  | SigD Name Type                -- { length :: [a] -> Int }
  | ForeignD Foreign
  deriving( Show, Eq )

data FunDep = FunDep [Name] [Name]
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
         | ForallC [Name] Cxt Con
         deriving( Show, Eq )

type StrictType = (Strict, Type)
type VarStrictType = (Name, Strict, Type)

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

