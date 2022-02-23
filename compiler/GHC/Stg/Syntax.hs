{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE UndecidableInstances #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Shared term graph (STG) syntax for spineless-tagless code generation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This data type represents programs just before code generation (conversion to
@Cmm@): basically, what we have is a stylised form of Core syntax, the style
being one that happens to be ideally suited to spineless tagless code
generation.
-}

module GHC.Stg.Syntax (
        StgArg(..),

        GenStgTopBinding(..), GenStgBinding(..), GenStgExpr(..), GenStgRhs(..),
        GenStgAlt(..), AltType(..),

        StgPass(..), BinderP, XRhsClosure, XLet, XLetNoEscape,
        NoExtFieldSilent, noExtFieldSilent,
        OutputablePass,

        UpdateFlag(..), isUpdatable,

        ConstructorNumber(..),

        -- a set of synonyms for the vanilla parameterisation
        StgTopBinding, StgBinding, StgExpr, StgRhs, StgAlt,

        -- a set of synonyms for the code gen parameterisation
        CgStgTopBinding, CgStgBinding, CgStgExpr, CgStgRhs, CgStgAlt,

        -- Same for taggedness
        TgStgTopBinding, TgStgBinding, TgStgExpr, TgStgRhs, TgStgAlt,

        -- a set of synonyms for the lambda lifting parameterisation
        LlStgTopBinding, LlStgBinding, LlStgExpr, LlStgRhs, LlStgAlt,

        -- a set of synonyms to distinguish in- and out variants
        InStgArg,  InStgTopBinding,  InStgBinding,  InStgExpr,  InStgRhs,  InStgAlt,
        OutStgArg, OutStgTopBinding, OutStgBinding, OutStgExpr, OutStgRhs, OutStgAlt,

        -- StgOp
        StgOp(..),

        -- utils
        stgRhsArity, freeVarsOfRhs,
        isDllConApp,
        stgArgType,
        stgCaseBndrInScope,

        -- ppr
        StgPprOpts(..),
        panicStgPprOpts, shortStgPprOpts,
        pprStgArg, pprStgExpr, pprStgRhs, pprStgBinding,
        pprGenStgTopBinding, pprStgTopBinding,
        pprGenStgTopBindings, pprStgTopBindings
    ) where

import GHC.Prelude

import GHC.Core     ( AltCon )
import GHC.Types.CostCentre ( CostCentreStack )
import Data.ByteString ( ByteString )
import Data.Data   ( Data )
import Data.List   ( intersperse )
import GHC.Core.DataCon
import GHC.Types.ForeignCall ( ForeignCall )
import GHC.Types.Id
import GHC.Types.Name        ( isDynLinkName )
import GHC.Types.Tickish     ( StgTickish )
import GHC.Types.Var.Set
import GHC.Types.Literal     ( Literal, literalType )
import GHC.Unit.Module       ( Module )
import GHC.Utils.Outputable
import GHC.Platform
import GHC.Core.Ppr( {- instances -} )
import GHC.Builtin.PrimOps ( PrimOp, PrimCall )
import GHC.Core.TyCon    ( PrimRep(..), TyCon )
import GHC.Core.Type     ( Type )
import GHC.Types.RepType ( typePrimRep1 )
import GHC.Utils.Panic.Plain

{-
************************************************************************
*                                                                      *
GenStgBinding
*                                                                      *
************************************************************************

As usual, expressions are interesting; other things are boring. Here are the
boring things (except note the @GenStgRhs@), parameterised with respect to
binder and occurrence information (just as in @GHC.Core@):
-}

-- | A top-level binding.
data GenStgTopBinding pass
-- See Note [Core top-level string literals]
  = StgTopLifted (GenStgBinding pass)
  | StgTopStringLit Id ByteString

data GenStgBinding pass
  = StgNonRec (BinderP pass) (GenStgRhs pass)
  | StgRec    [(BinderP pass, GenStgRhs pass)]

{-
************************************************************************
*                                                                      *
StgArg
*                                                                      *
************************************************************************
-}

data StgArg
  = StgVarArg  Id
  | StgLitArg  Literal

-- | Does this constructor application refer to anything in a different
-- *Windows* DLL?
-- If so, we can't allocate it statically
isDllConApp
  :: Platform
  -> Bool          -- is Opt_ExternalDynamicRefs enabled?
  -> Module
  -> DataCon
  -> [StgArg]
  -> Bool
isDllConApp platform ext_dyn_refs this_mod con args
 | not ext_dyn_refs    = False
 | platformOS platform == OSMinGW32
    = isDynLinkName platform this_mod (dataConName con) || any is_dll_arg args
 | otherwise = False
  where
    -- NB: typePrimRep1 is legit because any free variables won't have
    -- unlifted type (there are no unlifted things at top level)
    is_dll_arg :: StgArg -> Bool
    is_dll_arg (StgVarArg v) =  isAddrRep (typePrimRep1 (idType v))
                             && isDynLinkName platform this_mod (idName v)
    is_dll_arg _             = False

-- True of machine addresses; these are the things that don't work across DLLs.
-- The key point here is that VoidRep comes out False, so that a top level
-- nullary GADT constructor is False for isDllConApp
--
--    data T a where
--      T1 :: T Int
--
-- gives
--
--    T1 :: forall a. (a~Int) -> T a
--
-- and hence the top-level binding
--
--    $WT1 :: T Int
--    $WT1 = T1 Int (Coercion (Refl Int))
--
-- The coercion argument here gets VoidRep
isAddrRep :: PrimRep -> Bool
isAddrRep AddrRep     = True
isAddrRep LiftedRep   = True
isAddrRep UnliftedRep = True
isAddrRep _           = False

-- | Type of an @StgArg@
--
-- Very half baked because we have lost the type arguments.
stgArgType :: StgArg -> Type
stgArgType (StgVarArg v)   = idType v
stgArgType (StgLitArg lit) = literalType lit

-- | Given an alt type and whether the program is unarised, return whether the
-- case binder is in scope.
--
-- Case binders of unboxed tuple or unboxed sum type always dead after the
-- unariser has run. See Note [Post-unarisation invariants].
stgCaseBndrInScope :: AltType -> Bool {- ^ unarised? -} -> Bool
stgCaseBndrInScope alt_ty unarised =
    case alt_ty of
      AlgAlt _      -> True
      PrimAlt _     -> True
      MultiValAlt _ -> not unarised
      PolyAlt       -> True

{-
************************************************************************
*                                                                      *
STG expressions
*                                                                      *
************************************************************************

The @GenStgExpr@ data type is parameterised on binder and occurrence info, as
before.

************************************************************************
*                                                                      *
GenStgExpr
*                                                                      *
************************************************************************

An application is of a function to a list of atoms (not expressions).
Operationally, we want to push the arguments on the stack and call the function.
(If the arguments were expressions, we would have to build their closures
first.)

There is no constructor for a lone variable; it would appear as @StgApp var []@.
-}

data GenStgExpr pass
  = StgApp
        Id       -- function
        [StgArg] -- arguments; may be empty

{-
************************************************************************
*                                                                      *
StgConApp and StgPrimApp --- saturated applications
*                                                                      *
************************************************************************

There are specialised forms of application, for constructors, primitives, and
literals.
-}

  | StgLit      Literal

        -- StgConApp is vital for returning unboxed tuples or sums
        -- which can't be let-bound
  | StgConApp   DataCon
                ConstructorNumber
                [StgArg] -- Saturated
                [Type]   -- See Note [Types in StgConApp] in GHC.Stg.Unarise

  | StgOpApp    StgOp    -- Primitive op or foreign call
                [StgArg] -- Saturated.
                Type     -- Result type
                         -- We need to know this so that we can
                         -- assign result registers

{-
************************************************************************
*                                                                      *
GenStgExpr: case-expressions
*                                                                      *
************************************************************************

This has the same boxed/unboxed business as Core case expressions.
-}

  | StgCase
        (GenStgExpr pass) -- the thing to examine
        (BinderP pass) -- binds the result of evaluating the scrutinee
        AltType
        [GenStgAlt pass]
                    -- The DEFAULT case is always *first*
                    -- if it is there at all

{-
************************************************************************
*                                                                      *
GenStgExpr: let(rec)-expressions
*                                                                      *
************************************************************************

The various forms of let(rec)-expression encode most of the interesting things
we want to do.

-   let-closure x = [free-vars] [args] expr in e

  is equivalent to

    let x = (\free-vars -> \args -> expr) free-vars

  @args@ may be empty (and is for most closures). It isn't under circumstances
  like this:

    let x = (\y -> y+z)

  This gets mangled to

    let-closure x = [z] [y] (y+z)

  The idea is that we compile code for @(y+z)@ in an environment in which @z@ is
  bound to an offset from Node, and `y` is bound to an offset from the stack
  pointer.

  (A let-closure is an @StgLet@ with a @StgRhsClosure@ RHS.)

-   let-constructor x = Constructor [args] in e

  (A let-constructor is an @StgLet@ with a @StgRhsCon@ RHS.)

- Letrec-expressions are essentially the same deal as let-closure/
  let-constructor, so we use a common structure and distinguish between them
  with an @is_recursive@ boolean flag.

-   let-unboxed u = <an arbitrary arithmetic expression in unboxed values> in e

  All the stuff on the RHS must be fully evaluated. No function calls either!

  (We've backed away from this toward case-expressions with suitably-magical
  alts ...)

- Advanced stuff here! Not to start with, but makes pattern matching generate
  more efficient code.

    let-escapes-not fail = expr
    in e'

  Here the idea is that @e'@ guarantees not to put @fail@ in a data structure,
  or pass it to another function. All @e'@ will ever do is tail-call @fail@.
  Rather than build a closure for @fail@, all we need do is to record the stack
  level at the moment of the @let-escapes-not@; then entering @fail@ is just a
  matter of adjusting the stack pointer back down to that point and entering the
  code for it.

  Another example:

    f x y = let z = huge-expression in
            if y==1 then z else
            if y==2 then z else
            1

  (A let-escapes-not is an @StgLetNoEscape@.)

- We may eventually want:

    let-literal x = Literal in e

And so the code for let(rec)-things:
-}

  | StgLet
        (XLet pass)
        (GenStgBinding pass)    -- right hand sides (see below)
        (GenStgExpr pass)       -- body

  | StgLetNoEscape
        (XLetNoEscape pass)
        (GenStgBinding pass)    -- right hand sides (see below)
        (GenStgExpr pass)       -- body

{-
*************************************************************************
*                                                                      *
GenStgExpr: hpc, scc and other debug annotations
*                                                                      *
*************************************************************************

Finally for @hpc@ expressions we introduce a new STG construct.
-}

  | StgTick
    StgTickish
    (GenStgExpr pass)       -- sub expression

-- END of GenStgExpr

{-
************************************************************************
*                                                                      *
STG right-hand sides
*                                                                      *
************************************************************************

Here's the rest of the interesting stuff for @StgLet@s; the first flavour is for
closures:
-}

data GenStgRhs pass
  = StgRhsClosure
        (XRhsClosure pass) -- ^ Extension point for non-global free var
                           --   list just before 'CodeGen'.
        CostCentreStack    -- ^ CCS to be attached (default is CurrentCCS)
        !UpdateFlag        -- ^ 'ReEntrant' | 'Updatable' | 'SingleEntry'
        [BinderP pass]     -- ^ arguments; if empty, then not a function;
                           --   as above, order is important.
        (GenStgExpr pass)  -- ^ body

{-
An example may be in order.  Consider:

  let t = \x -> \y -> ... x ... y ... p ... q in e

Pulling out the free vars and stylising somewhat, we get the equivalent:

  let t = (\[p,q] -> \[x,y] -> ... x ... y ... p ...q) p q

Stg-operationally, the @[x,y]@ are on the stack, the @[p,q]@ are offsets from
@Node@ into the closure, and the code ptr for the closure will be exactly that
in parentheses above.

The second flavour of right-hand-side is for constructors (simple but
important):
-}

  | StgRhsCon
        CostCentreStack -- CCS to be attached (default is CurrentCCS).
                        -- Top-level (static) ones will end up with
                        -- DontCareCCS, because we don't count static
                        -- data in heap profiles, and we don't set CCCS
                        -- from static closure.
        DataCon         -- Constructor. Never an unboxed tuple or sum, as those
                        -- are not allocated.
        ConstructorNumber
        [StgTickish]
        [StgArg]        -- Args

-- | Like 'GHC.Hs.Extension.NoExtField', but with an 'Outputable' instance that
-- returns 'empty'.
data NoExtFieldSilent = NoExtFieldSilent
  deriving (Data, Eq, Ord)

instance Outputable NoExtFieldSilent where
  ppr _ = empty

-- | Used when constructing a term with an unused extension point that should
-- not appear in pretty-printed output at all.
noExtFieldSilent :: NoExtFieldSilent
noExtFieldSilent = NoExtFieldSilent
-- TODO: Maybe move this to GHC.Hs.Extension? I'm not sure about the
-- implications on build time...

stgRhsArity :: StgRhs -> Int
stgRhsArity (StgRhsClosure _ _ _ bndrs _)
  = assert (all isId bndrs) $ length bndrs
  -- The arity never includes type parameters, but they should have gone by now
stgRhsArity (StgRhsCon {}) = 0

freeVarsOfRhs :: (XRhsClosure pass ~ DIdSet) => GenStgRhs pass -> DIdSet
freeVarsOfRhs (StgRhsCon _ _ _ _ args) = mkDVarSet [ id | StgVarArg id <- args ]
freeVarsOfRhs (StgRhsClosure fvs _ _ _ _) = fvs

{-
************************************************************************
*                                                                      *
STG case alternatives
*                                                                      *
************************************************************************

Very like in Core syntax (except no type-world stuff).

The type constructor is guaranteed not to be abstract; that is, we can see its
representation. This is important because the code generator uses it to
determine return conventions etc. But it's not trivial where there's a module
loop involved, because some versions of a type constructor might not have all
the constructors visible. So mkStgAlgAlts (in CoreToStg) ensures that it gets
the TyCon from the constructors or literals (which are guaranteed to have the
Real McCoy) rather than from the scrutinee type.
-}

data GenStgAlt pass = GenStgAlt
  { alt_con          :: !AltCon            -- alts: data constructor,
  , alt_bndrs        :: ![BinderP pass]    -- constructor's parameters,
  , alt_rhs          :: !(GenStgExpr pass) -- right-hand side.
  }

data AltType
  = PolyAlt             -- Polymorphic (a boxed type variable, lifted or unlifted)
  | MultiValAlt Int     -- Multi value of this arity (unboxed tuple or sum)
                        -- the arity could indeed be 1 for unary unboxed tuple
                        -- or enum-like unboxed sums
  | AlgAlt      TyCon   -- Algebraic data type; the AltCons will be DataAlts
  | PrimAlt     PrimRep -- Primitive data type; the AltCons (if any) will be LitAlts

{-
************************************************************************
*                                                                      *
The Plain STG parameterisation
*                                                                      *
************************************************************************

  Note [STG Extension points]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  We now make use of extension points in STG for different passes which want
  to associate information with AST nodes.

  Currently the pipeline is roughly:

  CoreToStg: Core -> Stg
  StgSimpl: Stg -> Stg
  CodeGen: Stg -> Cmm

    As part of StgSimpl we run late lambda lifting (Ll).
    Late lambda lift:
    Stg -> FvStg -> LlStg -> Stg

  CodeGen:
    As part of CodeGen we run tag inference.
    Tag Inference:
      Stg -> Stg 'InferTaggedBinders` -> Stg

    And at a last step we add the free Variables:
      Stg -> CgStg

  Which finally CgStg being used to generate Cmm.

-}

type StgTopBinding = GenStgTopBinding 'Vanilla
type StgBinding    = GenStgBinding    'Vanilla
type StgExpr       = GenStgExpr       'Vanilla
type StgRhs        = GenStgRhs        'Vanilla
type StgAlt        = GenStgAlt        'Vanilla

type LlStgTopBinding = GenStgTopBinding 'LiftLams
type LlStgBinding    = GenStgBinding    'LiftLams
type LlStgExpr       = GenStgExpr       'LiftLams
type LlStgRhs        = GenStgRhs        'LiftLams
type LlStgAlt        = GenStgAlt        'LiftLams

type CgStgTopBinding = GenStgTopBinding 'CodeGen
type CgStgBinding    = GenStgBinding    'CodeGen
type CgStgExpr       = GenStgExpr       'CodeGen
type CgStgRhs        = GenStgRhs        'CodeGen
type CgStgAlt        = GenStgAlt        'CodeGen

type TgStgTopBinding = GenStgTopBinding 'CodeGen
type TgStgBinding    = GenStgBinding    'CodeGen
type TgStgExpr       = GenStgExpr       'CodeGen
type TgStgRhs        = GenStgRhs        'CodeGen
type TgStgAlt        = GenStgAlt        'CodeGen

{- Many passes apply a substitution, and it's very handy to have type
   synonyms to remind us whether or not the substitution has been applied.
   See GHC.Core for precedence in Core land
-}

type InStgTopBinding  = StgTopBinding
type InStgBinding     = StgBinding
type InStgArg         = StgArg
type InStgExpr        = StgExpr
type InStgRhs         = StgRhs
type InStgAlt         = StgAlt
type OutStgTopBinding = StgTopBinding
type OutStgBinding    = StgBinding
type OutStgArg        = StgArg
type OutStgExpr       = StgExpr
type OutStgRhs        = StgRhs
type OutStgAlt        = StgAlt

-- | When `-fdistinct-constructor-tables` is turned on then
-- each usage of a constructor is given an unique number and
-- an info table is generated for each different constructor.
data ConstructorNumber =
      NoNumber | Numbered Int

instance Outputable ConstructorNumber where
  ppr NoNumber = empty
  ppr (Numbered n) = text "#" <> ppr n

{-
Note Stg Passes
~~~~~~~~~~~~~~~
Here is a short summary of the STG pipeline and where we use the different
StgPass data type indexes:

  1. CoreToStg.Prep performs several transformations that prepare the desugared
     and simplified core to be converted to STG. One of these transformations is
     making it so that value lambdas only exist as the RHS of a binding.
     See Note [CorePrep Overview].

  2. CoreToStg converts the prepared core to STG, specifically GenStg*
     parameterised by 'Vanilla. See the GHC.CoreToStg Module.

  3. Stg.Pipeline does a number of passes on the generated STG. One of these is
     the lambda-lifting pass, which internally uses the 'LiftLams
     parameterisation to store information for deciding whether or not to lift
     each binding.
     See Note [Late lambda lifting in STG].

  4. Tag inference takes in 'Vanilla and produces 'InferTagged STG, while using
     the InferTaggedBinders annotated AST internally.
     See Note [Tag Inference].

  5. Stg.FVs annotates closures with their free variables. To store these
     annotations we use the 'CodeGen parameterisation.
     See the GHC.Stg.FVs module.

  6. The Module Stg.StgToCmm generates Cmm from the CodeGen annotated STG.
-}


-- | Used as a data type index for the stgSyn AST
data StgPass
  = Vanilla
  | LiftLams -- ^ Use internally by the lambda lifting pass
  | InferTaggedBinders -- ^ Tag inference information on binders.
                       -- See Note [Tag inference passes] in GHC.Stg.InferTags
  | InferTagged -- ^ Tag inference information put on relevant StgApp nodes
                -- See Note [Tag inference passes] in GHC.Stg.InferTags
  | CodeGen

type family BinderP (pass :: StgPass)
type instance BinderP 'Vanilla = Id
type instance BinderP 'CodeGen = Id
type instance BinderP 'InferTagged = Id

type family XRhsClosure (pass :: StgPass)
type instance XRhsClosure 'Vanilla = NoExtFieldSilent
type instance XRhsClosure 'InferTagged = NoExtFieldSilent
-- | Code gen needs to track non-global free vars
type instance XRhsClosure 'CodeGen = DIdSet

type family XLet (pass :: StgPass)
type instance XLet 'Vanilla = NoExtFieldSilent
type instance XLet 'InferTagged = NoExtFieldSilent
type instance XLet 'CodeGen = NoExtFieldSilent

type family XLetNoEscape (pass :: StgPass)
type instance XLetNoEscape 'Vanilla = NoExtFieldSilent
type instance XLetNoEscape 'InferTagged = NoExtFieldSilent
type instance XLetNoEscape 'CodeGen = NoExtFieldSilent

{-

************************************************************************
*                                                                      *
UpdateFlag
*                                                                      *
************************************************************************

This is also used in @LambdaFormInfo@ in the @ClosureInfo@ module.

A @ReEntrant@ closure may be entered multiple times, but should not be updated
or blackholed. An @Updatable@ closure should be updated after evaluation (and
may be blackholed during evaluation). A @SingleEntry@ closure will only be
entered once, and so need not be updated but may safely be blackholed.
-}

data UpdateFlag = ReEntrant | Updatable | SingleEntry

instance Outputable UpdateFlag where
    ppr u = char $ case u of
                       ReEntrant   -> 'r'
                       Updatable   -> 'u'
                       SingleEntry -> 's'

isUpdatable :: UpdateFlag -> Bool
isUpdatable ReEntrant   = False
isUpdatable SingleEntry = False
isUpdatable Updatable   = True

{-
************************************************************************
*                                                                      *
StgOp
*                                                                      *
************************************************************************

An StgOp allows us to group together PrimOps and ForeignCalls. It's quite useful
to move these around together, notably in StgOpApp and COpStmt.
-}

data StgOp
  = StgPrimOp  PrimOp

  | StgPrimCallOp PrimCall

  | StgFCallOp ForeignCall Type
        -- The Type, which is obtained from the foreign import declaration
        -- itself, is needed by the stg-to-cmm pass to determine the offset to
        -- apply to unlifted boxed arguments in GHC.StgToCmm.Foreign. See Note
        -- [Unlifted boxed arguments to foreign calls]

{-
************************************************************************
*                                                                      *
Pretty-printing
*                                                                      *
************************************************************************

Robin Popplestone asked for semi-colon separators on STG binds; here's hoping he
likes terminators instead...  Ditto for case alternatives.
-}

type OutputablePass pass =
  ( Outputable (XLet pass)
  , Outputable (XLetNoEscape pass)
  , Outputable (XRhsClosure pass)
  , OutputableBndr (BinderP pass)
  )

-- | STG pretty-printing options
data StgPprOpts = StgPprOpts
   { stgSccEnabled :: !Bool -- ^ Enable cost-centres
   }

-- | STG pretty-printing options used for panic messages
panicStgPprOpts :: StgPprOpts
panicStgPprOpts = StgPprOpts
   { stgSccEnabled = True
   }

-- | STG pretty-printing options used for short messages
shortStgPprOpts :: StgPprOpts
shortStgPprOpts = StgPprOpts
   { stgSccEnabled = False
   }


pprGenStgTopBinding
  :: OutputablePass pass => StgPprOpts -> GenStgTopBinding pass -> SDoc
pprGenStgTopBinding opts b = case b of
   StgTopStringLit bndr str -> hang (hsep [pprBndr LetBind bndr, equals]) 4 (pprHsBytes str <> semi)
   StgTopLifted bind        -> pprGenStgBinding opts bind

pprGenStgBinding :: OutputablePass pass => StgPprOpts -> GenStgBinding pass -> SDoc
pprGenStgBinding opts b = case b of
   StgNonRec bndr rhs -> hang (hsep [pprBndr LetBind bndr, equals]) 4 (pprStgRhs opts rhs <> semi)
   StgRec pairs       -> vcat [ text "Rec {"
                              , vcat (intersperse blankLine (map ppr_bind pairs))
                              , text "end Rec }" ]
                         where
                           ppr_bind (bndr, expr)
                             = hang (hsep [pprBndr LetBind bndr, equals])
                                    4 (pprStgRhs opts expr <> semi)

instance OutputablePass pass => Outputable  (GenStgBinding pass) where
  ppr = pprGenStgBinding panicStgPprOpts

pprGenStgTopBindings :: (OutputablePass pass) => StgPprOpts -> [GenStgTopBinding pass] -> SDoc
pprGenStgTopBindings opts binds
  = vcat $ intersperse blankLine (map (pprGenStgTopBinding opts) binds)

pprStgBinding :: OutputablePass pass => StgPprOpts -> GenStgBinding pass -> SDoc
pprStgBinding = pprGenStgBinding

pprStgTopBinding :: OutputablePass pass => StgPprOpts -> GenStgTopBinding pass -> SDoc
pprStgTopBinding = pprGenStgTopBinding

pprStgTopBindings :: OutputablePass pass => StgPprOpts -> [GenStgTopBinding pass] -> SDoc
pprStgTopBindings = pprGenStgTopBindings

instance Outputable StgArg where
  ppr = pprStgArg

pprStgArg :: StgArg -> SDoc
pprStgArg (StgVarArg var) = ppr var
pprStgArg (StgLitArg con) = ppr con

instance OutputablePass pass => Outputable  (GenStgExpr pass) where
  ppr = pprStgExpr panicStgPprOpts

pprStgExpr :: OutputablePass pass => StgPprOpts -> GenStgExpr pass -> SDoc
pprStgExpr opts e = case e of
                           -- special case
   StgLit lit           -> ppr lit
                           -- general case
   StgApp func args
      | null args
      , Just sig <- idTagSig_maybe func
      -> ppr func <> ppr sig
      | otherwise -> hang (ppr func) 4 (interppSP args) -- TODO: Print taggedness
   StgConApp con n args _ -> hsep [ ppr con, ppr n, brackets (interppSP args) ]
   StgOpApp op args _   -> hsep [ pprStgOp op, brackets (interppSP args)]

-- special case: let v = <very specific thing>
--               in
--               let ...
--               in
--               ...
--
-- Very special!  Suspicious! (SLPJ)

{-
   StgLet srt (StgNonRec bndr (StgRhsClosure cc bi free_vars upd_flag args rhs))
                        expr@(StgLet _ _))
   -> ($$)
      (hang (hcat [text "let { ", ppr bndr, text " = ",
                          ppr cc,
                          pp_binder_info bi,
                          text " [", whenPprDebug (interppSP free_vars), text "] \\",
                          ppr upd_flag, text " [",
                          interppSP args, char ']'])
            8 (sep [hsep [ppr rhs, text "} in"]]))
      (ppr expr)
-}

   -- special case: let ... in let ...
   StgLet ext bind expr@StgLet{} -> ($$)
      (sep [hang (text "let" <+> ppr ext <+> text "{")
                2 (hsep [pprGenStgBinding opts bind, text "} in"])])
      (pprStgExpr opts expr)

   -- general case
   StgLet ext bind expr
      -> sep [ hang (text "let" <+> ppr ext <+> text "{")
                    2 (pprGenStgBinding opts bind)
             , hang (text "} in ") 2 (pprStgExpr opts expr)
             ]

   StgLetNoEscape ext bind expr
      -> sep [ hang (text "let-no-escape" <+> ppr ext <+> text "{")
                    2 (pprGenStgBinding opts bind)
             , hang (text "} in ") 2 (pprStgExpr opts expr)
             ]

   StgTick _tickish expr -> sdocOption sdocSuppressTicks $ \case
      True  -> pprStgExpr opts expr
      False -> pprStgExpr opts expr
        -- XXX sep [ ppr tickish, pprStgExpr opts expr ]

   -- Don't indent for a single case alternative.
   StgCase expr bndr alt_type [alt]
      -> sep [ sep [ text "case"
                   , nest 4 (hsep [ pprStgExpr opts expr
                                  , whenPprDebug (dcolon <+> ppr alt_type)
                                  ])
                   , text "of"
                   , pprBndr CaseBind bndr
                   , char '{'
                   ]
             , pprStgAlt opts False alt
             , char '}'
             ]

   StgCase expr bndr alt_type alts
      -> sep [ sep [ text "case"
                   , nest 4 (hsep [ pprStgExpr opts expr
                                  , whenPprDebug (dcolon <+> ppr alt_type)
                                  ])
                   , text "of"
                   , pprBndr CaseBind bndr, char '{'
                   ]
             , nest 2 (vcat (map (pprStgAlt opts True) alts))
             , char '}'
             ]


pprStgAlt :: OutputablePass pass => StgPprOpts -> Bool -> GenStgAlt pass -> SDoc
pprStgAlt opts indent GenStgAlt{alt_con, alt_bndrs, alt_rhs}
  | indent    = hang altPattern 4 (pprStgExpr opts alt_rhs <> semi)
  | otherwise = sep [altPattern, pprStgExpr opts alt_rhs <> semi]
    where
      altPattern = hsep [ ppr alt_con
                        , sep (map (pprBndr CasePatBind) alt_bndrs)
                        , text "->"
                        ]


pprStgOp :: StgOp -> SDoc
pprStgOp (StgPrimOp  op)   = ppr op
pprStgOp (StgPrimCallOp op)= ppr op
pprStgOp (StgFCallOp op _) = ppr op

instance Outputable StgOp where
  ppr = pprStgOp

instance Outputable AltType where
  ppr PolyAlt         = text "Polymorphic"
  ppr (MultiValAlt n) = text "MultiAlt" <+> ppr n
  ppr (AlgAlt tc)     = text "Alg"    <+> ppr tc
  ppr (PrimAlt tc)    = text "Prim"   <+> ppr tc

pprStgRhs :: OutputablePass pass => StgPprOpts -> GenStgRhs pass -> SDoc
pprStgRhs opts rhs = case rhs of
   StgRhsClosure ext cc upd_flag args body
      -> hang (hsep [ if stgSccEnabled opts then ppr cc else empty
                    , ppUnlessOption sdocSuppressStgExts (ppr ext)
                    , char '\\' <> ppr upd_flag, brackets (interppSP args)
                    ])
              4 (pprStgExpr opts body)

   StgRhsCon cc con mid _ticks args
      -> hcat [ ppr cc, space
              , case mid of
                  NoNumber -> empty
                  Numbered n -> hcat [ppr n, space]
              -- The bang indicates this is an StgRhsCon instead of an StgConApp.
              , ppr con, text "! ", brackets (sep (map pprStgArg args))]

instance OutputablePass pass => Outputable  (GenStgRhs pass) where
  ppr = pprStgRhs panicStgPprOpts
