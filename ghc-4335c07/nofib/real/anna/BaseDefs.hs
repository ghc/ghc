
-- ==========================================================--
-- === Base declarations                      BaseDefs.hs ===--
-- ==========================================================--

module BaseDefs where

----------------------------------------------------------
-- Useful polymorphic types                             --
----------------------------------------------------------

type AList a b = [(a, b)]

type DefnGroup a = [(Bool, [a])] 

type ST a b = b -> (a, b)

data ATree a b = ALeaf
               | ABranch (ATree a b) a b (ATree a b) Int
                 deriving (Eq)
     
--1.3:data Maybe a = Nothing 
--             | Just a 
--               deriving (Eq)

data Reply a b = Ok a
               | Fail b
                 deriving (Eq)


----------------------------------------------------------
-- Misc Utils stuff                                     --
----------------------------------------------------------

type NameSupply = Int

type Oseq = Int -> Int -> [Char]

type Iseq = Oseq -> Oseq

data Set a = MkSet [a]
             deriving (Eq)
  
type Bag a = [a]


----------------------------------------------------------
-- Flags                                                --
----------------------------------------------------------

data Flag = Typecheck      -- don't do strictness analysis
          | Simp           -- do HExpr-simplification (usually a bad idea)
          | NoCaseOpt      -- don't do case-of-case optimisation
          | ShowHExpr      -- print HExprs as they are generated
          | NoPretty       -- don't clean up the program after \-lifting
          | NoFormat       -- don't prettily format first order output
          | NoBaraki       -- don't use embedding-closure pairs
          | SimpleInv      -- use simplistic version of inverse
          | PolyLim Int    -- how hard to work in Baraki dept for
          | MonoLim Int    -- polymorphism and approx FPs respectively
          | ForceAll       -- force all thunks before analysis
          | DryRun         -- quick pass to check lattice table
          | LowerLim Int   -- lower threshold for approx fixed pts
          | UpperLim Int   -- upper threshold for approx fixed pts
          | ScaleUp Int    -- scale up target ratio
            deriving (Eq)

bdDefaultSettings 
   = [PolyLim 10000, MonoLim 10000, LowerLim 0, UpperLim 1000000, ScaleUp 20]

bdDryRunSettings
   = [NoBaraki, LowerLim 0, UpperLim 0, PolyLim 1, MonoLim 1, ScaleUp 20]


----------------------------------------------------------
-- Provides a way for the system to give a              --
-- running commentary about what it is doing            --
----------------------------------------------------------

data SAInfo = SAResult    String Domain Route
            | SASearch    ACMode String Int Int
            | SASizes     String [OneFuncSize] [OneFuncSize]
            | SAHExpr     String (HExpr Naam)
            | SASL        [Route] [Route]
            | SAGiveUp    [String]
              deriving ()


----------------------------------------------------------
-- Stuff for the Approx Fixed Pts business              --
----------------------------------------------------------

data ExceptionInt a = MkExInt Int [a]
                      deriving (Eq, Ord, Show{-was:Text-})

{- partain: moved from SmallerLattice.hs -}
instance (Show{-was:Text-} a, Ord a) => Num (ExceptionInt a) where

   (MkExInt i1 xs1) + (MkExInt i2 xs2) 
      = MkExInt (i1 + i2) (xs1 ++ xs2)

   (MkExInt i1 xs1) * (MkExInt i2 xs2) 
      = MkExInt (i1 * i2) (xs1 ++ xs2)

type DomainInt = ExceptionInt Domain

type DInt = (Domain, Int)

type OneFuncSize = (Int, [Domain])

type Sequence = ([[OneFuncSize]], [[OneFuncSize]])


----------------------------------------------------------
-- Basic syntax trees for Core programs                 --
----------------------------------------------------------

type Naam = [Char]

type Alter = AlterP Naam
type AlterP a = ([a],                  -- parameters to pattern-match on
                 CExprP a)             -- resulting expressions
     
type ScValue = ScValueP Naam
type ScValueP a = ([a],                -- list of arguments for the SC
                   CExprP a)           -- body of the SC
     
type CoreProgram = CoreProgramP Naam
type CoreProgramP a = ([TypeDef],      -- type definitions
                       [(Naam,         -- list of SC names ...
                         ScValueP a)]) --    and their definitions

type AtomicProgram = ([TypeDef],       -- exactly like a CoreProgram except
                      CExpr)           -- all the SCs are put into a letrec

type TypeDef = (Naam,                  -- the type's name
                [Naam],                -- schematic type variables
                [ConstrAlt])           -- constructor list
     
type ConstrAlt = (Naam,                -- constructor's name
                  [TDefExpr])          -- list of argument types

data TDefExpr                          -- type expressions for definitions
                = TDefVar Naam         -- type variables
                | TDefCons             -- constructed types
                     Naam              --    constructor's name
                     [TDefExpr]        --    constituent type expressions
                  deriving (Eq)


----------------------------------------------------------
-- Core expressions                                     --
----------------------------------------------------------

type CExpr = CExprP Naam

data CExprP a                              -- Core expressions
             = EVar Naam                   -- variables
             | ENum Int                    -- numbers
             | EConstr Naam                -- constructors
             | EAp (CExprP a) (CExprP a)   -- applications
             | ELet                        -- lets and letrecs
                  Bool                     -- True == recursive
                  [(a, CExprP a)] 
                  (CExprP a)
             | ECase                       -- case statements
                  (CExprP a) 
                  [(Naam, AlterP a)]
             | ELam                        -- lambda abstractions
                  [a]
                  (CExprP a)
               deriving (Eq)
     
     
----------------------------------------------------------
-- Annotated Core expressions                           --
----------------------------------------------------------
     
type AnnExpr a b = (b, AnnExpr' a b)

data AnnExpr' a b
        = AVar Naam
        | ANum Int
        | AConstr Naam
        | AAp (AnnExpr a b) (AnnExpr a b)
        | ALet Bool [AnnDefn a b] (AnnExpr a b)
        | ACase (AnnExpr a b) [AnnAlt a b]
        | ALam [a] (AnnExpr a b)
          deriving (Eq)

type AnnDefn a b = (a, AnnExpr a b)

type AnnAlt a b  = (Naam, ([a], (AnnExpr a b)))

type AnnProgram a b = [(Naam, [a], AnnExpr a b)]


----------------------------------------------------------
-- Stuff for the #*$*%*%* Lambda-Lifter                 --
----------------------------------------------------------

data Eqn = EqnNVC Naam (Set Naam) (Set Naam)
           deriving (Eq)


----------------------------------------------------------
-- Typechecker stuff                                    --
----------------------------------------------------------

type TVName = ([Int],[Int])
     
type Message = [Char]
     
data TExpr = TVar TVName
           | TArr TExpr TExpr
           | TCons [Char] [TExpr]
             deriving (Eq)

data TypeScheme = Scheme [TVName] TExpr
                  deriving (Eq)

type Subst = AList TVName TExpr
     
type TcTypeEnv = AList Naam TypeScheme
     
type TypeEnv = AList Naam TExpr
     
type TypeNameSupply = TVName
     
type TypeInfo = (Subst, TExpr, AnnExpr Naam TExpr)
     
type TypeDependancy = DefnGroup Naam
     
     
----------------------------------------------------------
-- Domain stuff                                         --
----------------------------------------------------------
-- Assumes that all Domain values passed are in         --
-- uncurried form, ie no (Func _ (Func _ _)).           --
-- Functions generating denormalised                    --
-- function Domains must normalise them themselves.     --
----------------------------------------------------------

type Point = (Domain, Route)

data FrontierElem = MkFrel [Route]
                    deriving (Eq, Ord, Show{-was:Text-})

data Frontier = Min1Max0 Int [FrontierElem] [FrontierElem]
                deriving (Eq, Ord, Show{-was:Text-})

data Domain = Two
            | Lift1 [Domain]
            | Lift2 [Domain]
            | Func  [Domain] Domain
              deriving (Eq, Ord, Show, Read)

data Route = Zero
           | One
           | Stop1
           | Up1 [Route]
           | Stop2
           | Up2
           | UpUp2 [Route]
           | Rep Rep
             deriving (Eq, Ord, Show{-was:Text-})

data Rep = RepTwo Frontier
         | Rep1 Frontier [Rep]
         | Rep2 Frontier Frontier [Rep]
           deriving (Eq, Ord, Show{-was:Text-})

data DExpr = DXTwo                        
           | DXLift1  [DExpr]         
           | DXLift2  [DExpr]
           | DXFunc   [DExpr] DExpr 
           | DXVar    String
             deriving (Eq)

type RSubst = AList String Route

type DSubst = AList String Domain

type DRRSubst = AList String (Domain, Route, Route)

type DExprEnv = AList String DExpr
     
data ConstrElem = ConstrRec
                | ConstrVar Int
                  deriving (Eq, Ord, Show{-was:Text-})


----------------------------------------------------------
-- Abs and Conc stuff                                   --
----------------------------------------------------------

data ACMode = Safe
            | Live
              deriving (Eq)

----------------------------------------------------------
-- Frontier search stuff                                --
----------------------------------------------------------

type MemoList = AList [Route] Route

data AppInfo = A2 
                   -- trivial case
             | ALo1 
                   -- low factor in function to Lift1
             | AHi1 Int Int Domain
                   -- a high factor in a function to Lift1.
                   -- First Int is arity of low factor, second is
                   -- the index of the high factor sought.
                   -- Domain is of the high factor sought.
             | ALo2
                   -- low factor in function to Lift2
             | AMid2
                   -- middle factor in function to Lift2
             | AHi2 Int Int Domain
                   -- a high factor in a function to Lift1.
                   -- First Int is arity of low & middle factors,
                   -- second is the index of the high factor sought.
                   -- Domain is of high factor sought.
               deriving (Eq)


----------------------------------------------------------
-- Abstract expression trees                            --
----------------------------------------------------------

data HExpr a = HApp (HExpr a) (HExpr a)
             | HVAp (HExpr a) [HExpr a]
             | HLam [a] (HExpr a)
             | HVar a
             | HMeet [HExpr a]  -- must be at least one in list
             | HPoint Route
             | HTable (AList Route (HExpr a))
               deriving (Eq, Show{-was:Text-})

     
----------------------------------------------------------
-- Prettyprinter stuff                                  --
----------------------------------------------------------
     
type PrPoint =  [Int]
     
type PrDomain =  [PrPoint]
     
     
----------------------------------------------------------
-- Parser stuff                                         --
----------------------------------------------------------
     
type Token =  (Int, [Char])

data PResult a = PFail [Token]
               | POk a [Token]
                 deriving (Eq)

type Parser a =  [Token] -> PResult a
     
data PartialExpr = NoOp 
                 | FoundOp Naam CExpr
                   deriving (Eq)


-- ===============================================================--
-- === Definition of the static component                      ===--
-- ===---------------------------------------------------------===--
-- === The static component carries around all information     ===--
-- === which remains unchanged throughout strictness analysis. ===--
-- === This avoids having to pass around vast hordes of        ===--
-- === parameters containing static information.               ===--
-- ===============================================================--
     
type StaticComponent 
    =  ( 
     	 DExprEnv,  
     	 -- == AList Naam DExpr, the program's types

         DSubst,
         -- == AList Naam Domain, the simplest domains of functions

         AList Naam [ConstrElem],
         -- information on constructors
   
         AList Naam [Naam],
         -- information on pseudo-params inserted to fix free vars

         [Flag],
         -- set of flags altering system operation

         (Int, Int, Int, Int, Int),
         -- polymorphic and monomorphic Baraki limits, 
         -- and lower and upper limits for lattice sizes
         -- and the scaleup ratio

         AList Domain Int
         -- the lattice size table
        )


-- ==========================================================--
-- === end                                    BaseDefs.hs ===--
-- ==========================================================--
