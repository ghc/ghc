{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1998


This module contains definitions for the IdInfo for things that
have a standard form, namely:

- data constructors
- record selectors
- method and superclass selectors
- primitive operations
-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Builtin.WiredIn.Ids (
        wiredInIds, ghcPrimIds,
        realWorldPrimId,
        voidPrimId, voidArgId,
        nullAddrId, seqId, lazyId, lazyIdKey,
        coercionTokenId, coerceId,
        proxyHashId,
        nospecId, nospecIdName,
        noinlineId, noinlineIdName,
        noinlineConstraintId, noinlineConstraintIdName,
        coerceName, leftSectionName, rightSectionName,
        pcRepPolyId,

        unboxedUnitExpr,
        mkRepPolyIdConcreteTyVars,
    ) where

import GHC.Prelude

import GHC.Builtin.WiredIn.Prim
import GHC.Builtin.WiredIn.Types
import GHC.Builtin.KnownKeys
import GHC.Builtin.Modules( gHC_PRIM, gHC_MAGIC )

import GHC.Core
import GHC.Core.Opt.Arity( typeOneShot )
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Core.Coercion
import GHC.Core.Make
import GHC.Core.Utils   ( exprType )
import GHC.Core.Unfold.Make
import GHC.Core.DataCon

import GHC.Types.Literal
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.InlinePragma
import GHC.Types.Var (VarBndr(Bndr), visArgConstraintLike, tyVarName)

import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType as TcType

import GHC.Data.FastString


{-
************************************************************************
*                                                                      *
\subsection{Wired in Ids}
*                                                                      *
************************************************************************

Note [Wired-in Ids]
~~~~~~~~~~~~~~~~~~~
A "wired-in" Id can be referred to directly in GHC (e.g. 'voidPrimId')
rather than by looking it up its name in some environment or fetching
it from an interface file.

There are several reasons why an Id might appear in the wiredInIds:

* ghcPrimIds: see Note [ghcPrimIds (aka pseudoops)]

* magicIds: see Note [magicIds]

* errorIds, defined in GHC.Core.Make.
  These error functions (e.g. rUNTIME_ERROR_ID) are wired in
  because the desugarer generates code that mentions them directly

In all cases except ghcPrimIds, there is a definition site in a
library module, which may be called (e.g. in higher order situations);
but the wired-in version means that the details are never read from
that module's interface file; instead, the full definition is right
here.

Note [ghcPrimIds (aka pseudoops)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The ghcPrimIds

  * Are exported from GHC.Prim (see ghcPrimExports, used in ghcPrimInterface)
    See Note [GHC.Prim] in primops.txt.pp for the remaining items in GHC.Prim.

  * Can't be defined in Haskell, and hence no Haskell binding site,
    but have perfectly reasonable unfoldings in Core

  * Either have a CompulsoryUnfolding (hence always inlined), or
        of an EvaldUnfolding and void representation (e.g. realWorldPrimId)

  * Are (or should be) defined in primops.txt.pp as 'pseudoop'
    Reason: that's how we generate documentation for them

Note [magicIds]
~~~~~~~~~~~~~~~
The magicIds

  * Are exported from GHC.Magic

  * Can be defined in Haskell (and are, in ghc-prim:GHC/Magic.hs).
    This definition at least generates Haddock documentation for them.

  * May or may not have a CompulsoryUnfolding.

  * But have some special behaviour that can't be done via an
    unfolding from an interface file.

  * May have IdInfo that differs from what would be imported from GHC.Magic.hi.
    For example, 'lazy' gets a lazy strictness signature, per Note [lazyId magic].

  The two remaining identifiers in GHC.Magic, runRW# and inline, are not
  listed in magicIds: they have special behavior but they can be known-key and
  not wired-in.
  Similarly for GHC.Internal.IO.seq# and GHC.Internal.Exts.considerAccessible.
  runRW#:             see Note [Simplification of runRW#] in Prep,
                      runRW# code in Simplifier, Note [Linting of runRW#].
  seq#:               see Note [seq# magic]
  inline:             see Note [inlineId magic]
  considerAccessible: see Note [considerAccessible]
-}

wiredInIds :: [Id]
wiredInIds
  =  magicIds
  ++ ghcPrimIds
  ++ errorIds           -- Defined in GHC.Core.Make

magicIds :: [Id]    -- See Note [magicIds]
magicIds = [lazyId, oneShotId, noinlineId, noinlineConstraintId, nospecId]

ghcPrimIds :: [Id]  -- See Note [ghcPrimIds (aka pseudoops)]
ghcPrimIds
  = [ realWorldPrimId
    , voidPrimId
    , nullAddrId
    , seqId
    , coerceId
    , proxyHashId
    , leftSectionId
    , rightSectionId
    ]

{-
************************************************************************
*                                                                      *
               Wired in Ids
*                                                                      *
************************************************************************

These Ids can't be defined in Haskell.  They could be defined in
unfoldings in the wired-in GHC.Prim interface file, but we'd have to
ensure that they were definitely, definitely inlined, because there is
no curried identifier for them.  That's what mkCompulsoryUnfolding
does. Alternatively, we could add the definitions to mi_decls of ghcPrimIface
but it's not clear if this would be simpler.

coercionToken# is not listed in ghcPrimIds, since its type uses (~#)
which is not supposed to be used in expressions (GHC throws an assertion
failure when trying.)
-}


nullAddrName, seqName,
   realWorldName, voidPrimIdName, coercionTokenName,
   coerceName, proxyName,
   leftSectionName, rightSectionName :: Name
nullAddrName      = mkWiredInIdName gHC_PRIM  (fsLit "nullAddr#")      nullAddrIdKey      nullAddrId
seqName           = mkWiredInIdName gHC_PRIM  (fsLit "seq")            seqIdKey           seqId
realWorldName     = mkWiredInIdName gHC_PRIM  (fsLit "realWorld#")     realWorldPrimIdKey realWorldPrimId
voidPrimIdName    = mkWiredInIdName gHC_PRIM  (fsLit "void#")          voidPrimIdKey      voidPrimId
coercionTokenName = mkWiredInIdName gHC_PRIM  (fsLit "coercionToken#") coercionTokenIdKey coercionTokenId
coerceName        = mkWiredInIdName gHC_PRIM  (fsLit "coerce")         coerceKey          coerceId
proxyName         = mkWiredInIdName gHC_PRIM  (fsLit "proxy#")         proxyHashKey       proxyHashId
leftSectionName   = mkWiredInIdName gHC_PRIM  (fsLit "leftSection")    leftSectionKey     leftSectionId
rightSectionName  = mkWiredInIdName gHC_PRIM  (fsLit "rightSection")   rightSectionKey    rightSectionId

-- Names listed in magicIds; see Note [magicIds]
lazyIdName, oneShotName, nospecIdName :: Name
lazyIdName        = mkWiredInIdName gHC_MAGIC (fsLit "lazy")           lazyIdKey          lazyId
oneShotName       = mkWiredInIdName gHC_MAGIC (fsLit "oneShot")        oneShotKey         oneShotId
nospecIdName      = mkWiredInIdName gHC_MAGIC (fsLit "nospec")         nospecIdKey        nospecId

------------------------------------------------
proxyHashId :: Id
proxyHashId
  = pcMiscPrelId proxyName ty
       (noCafIdInfo `setUnfoldingInfo` evaldUnfolding) -- Note [evaldUnfoldings]
  where
    -- proxy# :: forall {k} (a:k). Proxy# k a
    --
    -- The visibility of the `k` binder is Inferred to match the type of the
    -- Proxy data constructor (#16293).
    [kv,tv] = mkTemplateKiTyVar liftedTypeKind (\x -> [x])
    kv_ty   = mkTyVarTy kv
    tv_ty   = mkTyVarTy tv
    ty      = mkInfForAllTy kv $ mkSpecForAllTy tv $ mkProxyPrimTy kv_ty tv_ty

------------------------------------------------
nullAddrId :: Id
-- nullAddr# :: Addr#
-- The reason it is here is because we don't provide
-- a way to write this literal in Haskell.
nullAddrId = pcMiscPrelId nullAddrName addrPrimTy info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding (Lit nullAddrLit)

------------------------------------------------
seqId :: Id     -- See Note [seqId magic]
seqId = pcRepPolyId seqName ty concs info
  where
    info = noCafIdInfo `setInlinePragInfo` inline_prag
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
                       `setArityInfo`      arity

    inline_prag
         = alwaysInlinePragma `setInlinePragmaActivation` activeAfter (Phase 0)
                  -- Make 'seq' not inline-always, so that simpleOptExpr
                  -- (see GHC.Core.Subst.simple_app) won't inline 'seq' on the
                  -- LHS of rules.  That way we can have rules for 'seq';
                  -- see Note [seqId magic]

    -- seq :: forall (r :: RuntimeRep) a (b :: TYPE r). a -> b -> b
    ty  =
      mkInfForAllTy runtimeRep2TyVar
      $ mkSpecForAllTys [alphaTyVar, openBetaTyVar]
      $ mkVisFunTyMany alphaTy (mkVisFunTyMany openBetaTy openBetaTy)

    [x,y] = mkTemplateLocals [alphaTy, openBetaTy]
    rhs = mkLams ([runtimeRep2TyVar, alphaTyVar, openBetaTyVar, x, y]) $
          Case (Var x) x openBetaTy [Alt DEFAULT [] (Var y)]

    concs = mkRepPolyIdConcreteTyVars
        [ ((openBetaTy, mkArgPos 2 Top), runtimeRep2TyVar)]

    arity = 2

------------------------------------------------
lazyId :: Id    -- See Note [lazyId magic]
lazyId = pcMiscPrelId lazyIdName ty info
  where
    info = noCafIdInfo
    ty  = mkSpecForAllTys [alphaTyVar] (mkVisFunTyMany alphaTy alphaTy)

------------------------------------------------
noinlineIdName, noinlineConstraintIdName :: Name
noinlineIdName           = mkWiredInIdName gHC_MAGIC (fsLit "noinline")
                                           noinlineIdKey noinlineId
noinlineConstraintIdName = mkWiredInIdName gHC_MAGIC (fsLit "noinlineConstraint")
                                           noinlineConstraintIdKey noinlineConstraintId

noinlineId :: Id -- See Note [noinlineId magic]
noinlineId = pcMiscPrelId noinlineIdName ty info
  where
    info = noCafIdInfo
    ty  = mkSpecForAllTys [alphaTyVar] $
          mkVisFunTyMany alphaTy alphaTy

noinlineConstraintId :: Id -- See Note [noinlineId magic]
noinlineConstraintId = pcMiscPrelId noinlineConstraintIdName ty info
  where
    info = noCafIdInfo
    ty   = mkSpecForAllTys [alphaConstraintTyVar] $
           mkFunTy visArgConstraintLike ManyTy alphaTy alphaConstraintTy

------------------------------------------------
nospecId :: Id -- See Note [nospecId magic]
nospecId = pcMiscPrelId nospecIdName ty info
  where
    info = noCafIdInfo
    ty  = mkSpecForAllTys [alphaTyVar] (mkVisFunTyMany alphaTy alphaTy)

oneShotId :: Id -- See Note [oneShot magic]
oneShotId = pcRepPolyId oneShotName ty concs info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
                       `setArityInfo`      arity
    -- oneShot :: forall {r1 r2} (a :: TYPE r1) (b :: TYPE r2). (a -> b) -> (a -> b)
    ty  = mkInfForAllTys  [ runtimeRep1TyVar, runtimeRep2TyVar ] $
          mkSpecForAllTys [ openAlphaTyVar, openBetaTyVar ]      $
          mkVisFunTyMany fun_ty fun_ty
    fun_ty = mkVisFunTyMany openAlphaTy openBetaTy
    [body, x] = mkTemplateLocals [fun_ty, openAlphaTy]
    x' = setOneShotLambda x  -- Here is the magic bit!
    rhs = mkLams [ runtimeRep1TyVar, runtimeRep2TyVar
                 , openAlphaTyVar, openBetaTyVar
                 , body, x'] $
          Var body `App` Var x'
    arity = 2

    concs = mkRepPolyIdConcreteTyVars
        [((openAlphaTy, mkArgPos 2 Top), runtimeRep1TyVar)]

----------------------------------------------------------------------
{- Note [Wired-in Ids for rebindable syntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The functions leftSectionId, rightSectionId are
wired in here ONLY because they are used in a representation-polymorphic way
by the rebindable syntax mechanism. See GHC.Rename.Expr
Note [Handling overloaded and rebindable constructs].

Alas, we can't currently give Haskell definitions for
representation-polymorphic functions.

They have Compulsory unfoldings, so that the representation polymorphism
does not linger for long.
-}

-- See Note [Left and right sections] in GHC.Rename.Expr
-- See Note [Wired-in Ids for rebindable syntax]
--   leftSection :: forall r1 r2 n (a::TYPE r1) (b::TYPE r2).
--                  (a %n-> b) -> a %n-> b
--   leftSection f x = f x
-- Important that it is eta-expanded, so that (leftSection undefined `seq` ())
--   is () and not undefined
-- Important that is is multiplicity-polymorphic (test linear/should_compile/OldList)
leftSectionId :: Id
leftSectionId = pcRepPolyId leftSectionName ty concs info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
                       `setArityInfo`      arity
    ty  = mkInfForAllTys  [runtimeRep1TyVar,runtimeRep2TyVar, multiplicityTyVar1] $
          mkSpecForAllTys [openAlphaTyVar,  openBetaTyVar]    $
          exprType body
    [f,x] = mkTemplateLocals [mkVisFunTy mult openAlphaTy openBetaTy, openAlphaTy]

    mult = mkTyVarTy multiplicityTyVar1 :: Mult
    xmult = setIdMult x mult

    rhs  = mkLams [ runtimeRep1TyVar, runtimeRep2TyVar, multiplicityTyVar1
                  , openAlphaTyVar,   openBetaTyVar   ] body
    body = mkLams [f,xmult] $ App (Var f) (Var xmult)
    arity = 2

    concs = mkRepPolyIdConcreteTyVars
            [((openAlphaTy, mkArgPos 2 Top), runtimeRep1TyVar)]

-- See Note [Left and right sections] in GHC.Rename.Expr
-- See Note [Wired-in Ids for rebindable syntax]
--   rightSection :: forall r1 r2 r3 n1 n2 (a::TYPE r1) (b::TYPE r2) (c::TYPE r3).
--                   (a %n1 -> b %n2-> c) -> b %n2-> a %n1-> c
--   rightSection f y x = f x y
-- Again, multiplicity polymorphism is important
rightSectionId :: Id
rightSectionId = pcRepPolyId rightSectionName ty concs info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
                       `setArityInfo`      arity
    ty  = mkInfForAllTys  [runtimeRep1TyVar,runtimeRep2TyVar,runtimeRep3TyVar
                          , multiplicityTyVar1, multiplicityTyVar2 ] $
          mkSpecForAllTys [openAlphaTyVar,  openBetaTyVar,   openGammaTyVar ]  $
          exprType body
    mult1 = mkTyVarTy multiplicityTyVar1
    mult2 = mkTyVarTy multiplicityTyVar2

    [f,x,y] = mkTemplateLocals [ mkScaledFunTys [ Scaled mult1 openAlphaTy
                                                , Scaled mult2 openBetaTy ] openGammaTy
                               , openAlphaTy, openBetaTy ]
    xmult = setIdMult x mult1
    ymult = setIdMult y mult2
    rhs  = mkLams [ runtimeRep1TyVar, runtimeRep2TyVar, runtimeRep3TyVar
                  , multiplicityTyVar1, multiplicityTyVar2
                  , openAlphaTyVar,   openBetaTyVar,    openGammaTyVar ] body
    body = mkLams [f,ymult,xmult] $ mkVarApps (Var f) [xmult,ymult]
    arity = 3

    concs =
      mkRepPolyIdConcreteTyVars
        [ ((openAlphaTy, mkArgPos 3 Top), runtimeRep1TyVar)
        , ((openBetaTy , mkArgPos 2 Top), runtimeRep2TyVar)]

--------------------------------------------------------------------------------

coerceId :: Id
-- Only wired-in because of a test in LintEtaExpand.hs
coerceId = pcRepPolyId coerceName ty concs info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
                       `setArityInfo`      2
    eqRTy     = mkTyConApp coercibleTyCon  [ tYPE_r,         a, b ]
    eqRPrimTy = mkTyConApp eqReprPrimTyCon [ tYPE_r, tYPE_r, a, b ]
    ty        = mkInvisForAllTys [ Bndr rv InferredSpec
                                 , Bndr av SpecifiedSpec
                                 , Bndr bv SpecifiedSpec ] $
                mkInvisFunTy eqRTy $
                mkVisFunTyMany a b

    bndrs@[rv,av,bv] = mkTemplateKiTyVar runtimeRepTy
                        (\r -> [mkTYPEapp r, mkTYPEapp r])

    [r, a, b] = mkTyVarTys bndrs
    tYPE_r    = mkTYPEapp r

    [eqR,x,eq] = mkTemplateLocals [eqRTy, a, eqRPrimTy]
    rhs = mkLams (bndrs ++ [eqR, x]) $
          mkWildCase (Var eqR) (unrestricted eqRTy) b $
          [Alt (DataAlt coercibleDataCon) [eq] (Cast (Var x) (mkCoVarCo eq))]

    concs = mkRepPolyIdConcreteTyVars
            [((mkTyVarTy av, mkArgPos 1 Top), rv)]

{-
Note [seqId magic]
~~~~~~~~~~~~~~~~~~
'GHC.Prim.seq' is special in several ways.

a) Its fixity is set in GHC.Iface.Load.ghcPrimIface

b) It has quite a bit of desugaring magic.
   See GHC.HsToCore.Utils Note [Desugaring seq] (1) and (2) and (3)

c) There is some special rule handing: Note [User-defined RULES for seq]

Historical note:
    In GHC.Tc.Gen.Expr we used to need a special typing rule for 'seq', to handle calls
    whose second argument had an unboxed type, e.g.  x `seq` 3#

    However, with representation polymorphism we can now give seq the type
    seq :: forall (r :: RuntimeRep) a (b :: TYPE r). a -> b -> b
    which handles this case without special treatment in the typechecker.

Note [User-defined RULES for seq]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Roman found situations where he had
      case (f n) of _ -> e
where he knew that f (which was strict in n) would terminate if n did.
Notice that the result of (f n) is discarded. So it makes sense to
transform to
      case n of _ -> e

Rather than attempt some general analysis to support this, I've added
enough support that you can do this using a rewrite rule:

  RULE "f/seq" forall n.  seq (f n) = seq n

You write that rule.  When GHC sees a case expression that discards
its result, it mentally transforms it to a call to 'seq' and looks for
a RULE.  (This is done in GHC.Core.Opt.Simplify.trySeqRules.)  As usual, the
correctness of the rule is up to you.

VERY IMPORTANT: to make this work, we give the RULE an arity of 1, not 2.
If we wrote
  RULE "f/seq" forall n e.  seq (f n) e = seq n e
with rule arity 2, then two bad things would happen:

  - The magical desugaring done in Note [seqId magic] item (b)
    for saturated application of 'seq' would turn the LHS into
    a case expression!

  - The code in GHC.Core.Opt.Simplify.rebuildCase would need to actually supply
    the value argument, which turns out to be awkward.

See also: Note [User-defined RULES for seq] in GHC.Core.Opt.Simplify.


Note [lazyId magic]
~~~~~~~~~~~~~~~~~~~
lazy :: forall a. a -> a

'lazy' is used to make sure that a sub-expression, and its free variables,
are truly used call-by-need, with no code motion.  Key examples:

* pseq:    pseq a b = a `seq` lazy b
  We want to make sure that the free vars of 'b' are not evaluated
  before 'a', even though the expression is plainly strict in 'b'.

* catch:   catch a b = catch# (lazy a) b
  Again, it's clear that 'a' will be evaluated strictly (and indeed
  applied to a state token) but we want to make sure that any exceptions
  arising from the evaluation of 'a' are caught by the catch (see
  #11555).

Implementing 'lazy' is a bit tricky:

* It must not have a strictness signature: by being a built-in Id,
  all the info about lazyId comes from here, not from GHC.Magic.hi.
  This is important, because the strictness analyser will spot it as
  strict!

* It must not have an unfolding: it gets "inlined" by a HACK in
  CorePrep. It's very important to do this inlining *after* unfoldings
  are exposed in the interface file.  Otherwise, the unfolding for
  (say) pseq in the interface file will not mention 'lazy', so if we
  inline 'pseq' we'll totally miss the very thing that 'lazy' was
  there for in the first place. See #3259 for a real world
  example.

* Suppose CorePrep sees (catch# (lazy e) b).  At all costs we must
  avoid using call by value here:
     case e of r -> catch# r b
  Avoiding that is the whole point of 'lazy'.  So in CorePrep (which
  generate the 'case' expression for a call-by-value call) we must
  spot the 'lazy' on the arg (in CorePrep.cpeApp), and build a 'let'
  instead.

* lazyId is defined in GHC.Base, so we don't *have* to inline it.  If it
  appears un-applied, we'll end up just calling it.

Note [noinlineId magic]
~~~~~~~~~~~~~~~~~~~~~~~
'noinline' is used to make sure that a function f is never inlined,
e.g., as in 'noinline f x'.  We won't inline f because we never inline
lone variables (see Note [Lone variables] in GHC.Core.Unfold

You might think that we could implement noinline like this:
   {-# NOINLINE #-}
   noinline :: forall a. a -> a
   noinline x = x

But actually we give 'noinline' a wired-in name for three distinct reasons:

1. We don't want to leave a (useless) call to noinline in the final program,
   to be executed at runtime. So we have a little bit of magic to
   optimize away 'noinline' after we are done running the simplifier.
   This is done in GHC.CoreToStg.Prep.cpeApp.

2. 'noinline' sometimes gets inserted automatically when we serialize an
   expression to the interface format, in GHC.CoreToIface.toIfaceVar.
   See Note [Inlining and hs-boot files] in GHC.CoreToIface

3. Given foo :: Eq a => [a] -> Bool, the expression
     noinline foo x xs
   where x::Int, will naturally desugar to
      noinline @Int (foo @Int dEqInt) x xs
   But now it's entirely possible that (foo @Int dEqInt) will inline foo,
   since 'foo' is no longer a lone variable -- see #18995

   Solution: in the desugarer, rewrite
      noinline (f x y)  ==>  noinline f x y
   This is done in the `noinlineId` case of `GHC.HsToCore.Expr.ds_app_var`
   This is only needed for noinlineId, not noInlineConstraintId (wrinkle
   (W1) below), because the latter never shows up in user code.

Wrinkles

(W1) Sometimes case (2) above needs to apply `noinline` to a type of kind
     Constraint; e.g.
                    noinline @(Eq Int) $dfEqInt
     We don't have type-or-kind polymorphism, so we simply have two `inline`
     Ids, namely `noinlineId` and `noinlineConstraintId`.

(W2) Note that noinline as currently implemented can hide some simplifications
     since it hides strictness from the demand analyser. Specifically, the
     demand analyser will treat 'noinline f x' as lazy in 'x', even if the
     demand signature of 'f' specifies that it is strict in its argument. We
     considered fixing this this by adding a special case to the demand
     analyser to address #16588. However, the special case seemed like a large
     and expensive hammer to address a rare case and consequently we rather
     opted to use a more minimal solution.

Note [nospecId magic]
~~~~~~~~~~~~~~~~~~~~~
The 'nospec' magic Id is used to ensure to make a value opaque to the typeclass
specialiser. In CorePrep, we inline 'nospec', turning (nospec e) into e.
Note that this happens *after* unfoldings are exposed in the interface file.
This is crucial: otherwise, we could import an unfolding in which
'nospec' has been inlined (= erased), and we would lose the benefit.

'nospec' is used:

* In the implementation of 'withDict': we insert 'nospec' so that the
  typeclass specialiser doesn't assume any two evidence terms of the
  same type are equal. See Note [withDict] in GHC.Tc.Instance.Class,
  and see test case T21575b for an example.

* To defeat the specialiser when we have incoherent instances.
  See Note [Coherence and specialisation: overview] in GHC.Core.InstEnv.

Note [seq# magic]
~~~~~~~~~~~~~~~~~
The purpose of the magic Id (See Note [magicIds])

  seq# :: forall a s . a -> State# s -> (# State# s, a #)

is to elevate evaluation of its argument `a` into an observable side effect.
This implies that GHC's optimisations must preserve the evaluation "exactly
here", in the state thread.

The main use of seq# is to implement `evaluate`

   evaluate :: a -> IO a
   evaluate a = IO $ \s -> seq# a s

Its (NOINLINE) definition in GHC.Magic is simply

   seq# a s = let !a' = lazy a in (# s, a' #)

Things to note

(SEQ1)
  It must be NOINLINE, because otherwise the eval !a' would be decoupled from
  the state token s, and GHC's optimisations, in particular strictness analysis,
  would happily move the eval around.

  However, we *do* inline saturated applications of seq# in CorePrep, where
  evaluation order is fixed; see the implementation notes below.
  This is one reason why we need seq# to be known-key.

(SEQ2)
  The use of `lazy` ensures that strictness analysis does not see the eval
  that takes place, so the final demand signature is <L><L>, not <1L><L>.
  This is important for a definition like

    foo x y = evaluate y >> evaluate x

  Although both y and x are ultimately evaluated, the user made it clear
  they want to evaluate y *before* x.
  But if strictness analysis sees the evals, it infers foo as strict in
  both parameters. This strictness would be exploited in the backend by
  picking a call-by-value calling convention for foo, one that would evaluate
  x *before* y. Nononono!

  Because the definition of seq# uses `lazy`, it must live in a different module
  (GHC.Internal.IO); otherwise strictness analysis uses its own strictness
  signature for the definition of `lazy` instead of the one we wire in.

(SEQ3)
  Why does seq# return the value? Consider
     let x = e in
     case seq# x s of (# _, x' #) -> ... x' ... case x' of __DEFAULT -> ...
  Here, we could simply use x instead of x', but doing so would
  introduce an unnecessary indirection and tag check at runtime;
  also we can attach an evaldUnfolding to x' to discard any
  subsequent evals such as the `case x' of __DEFAULT`.

(SEQ4)
  T15226 demonstrates that we want to discard ok-for-discard seq#s. That is,
  simplify `case seq# <ok-to-discard> s of (# s', _ #) -> rhs[s']` to `rhs[s]`.
  You might wonder whether the Simplifier could do this. But see the excellent
  example in #24334 (immortalised as test T24334) for why it should be done in
  CorePrep.

Implementing seq#.  The compiler has magic for `seq#` in

- GHC.CoreToStg.Prep.cpeRhsE: Implement (SEQ4).

- Simplify.addEvals records evaluated-ness for the result (cf. (SEQ3)); see
  Note [Adding evaluatedness info to pattern-bound variables]
  in GHC.Core.Opt.Simplify.Iteration

- GHC.Core.Opt.DmdAnal.exprMayThrowPreciseException:
  Historically, seq# used to be a primop, and the majority of primops
  should return False in exprMayThrowPreciseException, so we do the same
  for seq# for back compat.

- GHC.CoreToStg.Prep: Inline saturated applications to a Case, e.g.,

    seq# (f 13) s
    ==>
    case f 13 of sat of __DEFAULT -> (# s, sat #)

  This is implemented in `cpeApp`, not unlike Note [runRW magic].
  We are only inlining seq#, leaving opportunities for case-of-known-con
  behind that are easily picked up by Unarise:

    case seq# f 13 s of (# s', r #) -> rhs
    ==> {Prep}
    case f 13 of sat of __DEFAULT -> case (# s, sat #) of (# s', r #) -> rhs
    ==> {Unarise}
    case f 13 of sat of __DEFAULT -> rhs[s/s',sat/r]

  Note that CorePrep really allocates a CaseBound FloatingBind for `f 13`.
  That's OK, because the telescope of Floats always stays in the same order
  and won't be floated out of binders, so all guarantees of evaluation order
  provided by seq# are upheld.

Note [oneShot magic]
~~~~~~~~~~~~~~~~~~~~
In the context of making left-folds fuse somewhat okish (see ticket #7994
and Note [Left folds via right fold]) it was determined that it would be useful
if library authors could explicitly tell the compiler that a certain lambda is
called at most once. The oneShot function allows that.

'oneShot' is representation-polymorphic, i.e. the type variables can refer
to unlifted types as well (#10744); e.g.
   oneShot (\x:Int# -> x +# 1#)

Like most magic functions it has a compulsory unfolding, so there is no need
for a real definition somewhere. We have one in GHC.Magic for the convenience
of putting the documentation there.

It uses `setOneShotLambda` on the lambda's binder. That is the whole magic:

A typical call looks like
     oneShot (\y. e)
after unfolding the definition `oneShot = \f \x[oneshot]. f x` we get
     (\f \x[oneshot]. f x) (\y. e)
 --> \x[oneshot]. ((\y.e) x)
 --> \x[oneshot] e[x/y]
which is what we want.

Also see https://gitlab.haskell.org/ghc/ghc/wikis/one-shot.

Wrinkles:
(OS1)  It is only effective if the one-shot info survives as long as possible; in
       particular it must make it into the interface in unfoldings. See Note [Preserve
       OneShotInfo] in GHC.Core.Tidy.

(OS2) (oneShot (error "urk")) rewrites to
           \x[oneshot]. error "urk" x
      thereby hiding the `error` under a lambda, which might be surprising,
      particularly if you have `-fpedantic-bottoms` on.  See #24296.


-------------------------------------------------------------
@realWorld#@ used to be a magic literal, \tr{void#}.  If things get
nasty as-is, change it back to a literal (@Literal@).

voidArgId is a Local Id used simply as an argument in functions
where we just want an arg to avoid having a thunk of unlifted type.
E.g.
        x = \ void :: Void# -> (# p, q #)

This comes up in strictness analysis

Note [evaldUnfoldings]
~~~~~~~~~~~~~~~~~~~~~~
The evaldUnfolding makes it look that some primitive value is
evaluated, which in turn makes Simplify.interestingArg return True,
which in turn makes INLINE things applied to said value likely to be
inlined.
-}

realWorldPrimId :: Id   -- :: State# RealWorld
realWorldPrimId = pcMiscPrelId realWorldName id_ty
                     (noCafIdInfo `setUnfoldingInfo` evaldUnfolding    -- Note [evaldUnfoldings]
                                  `setOneShotInfo`   typeOneShot id_ty)
   where
     id_ty = realWorldStatePrimTy

voidPrimId :: Id     -- Global constant :: Void#
                     -- The type Void# is now the same as (# #) (ticket #18441),
                     -- this identifier just signifies the (# #) datacon
                     -- and is kept for backwards compatibility.
                     -- We cannot define it in normal Haskell, since it's
                     -- a top-level unlifted value.
voidPrimId  = pcMiscPrelId voidPrimIdName unboxedUnitTy
                (noCafIdInfo `setUnfoldingInfo` mkCompulsoryUnfolding unboxedUnitExpr)

unboxedUnitExpr :: CoreExpr
unboxedUnitExpr = Var (dataConWorkId unboxedUnitDataCon)

voidArgId :: Id       -- Local lambda-bound :: Void#
voidArgId = mkSysLocal (fsLit "void") voidArgIdKey ManyTy unboxedUnitTy

coercionTokenId :: Id         -- :: () ~# ()
coercionTokenId -- See Note [Coercion tokens] in "GHC.CoreToStg"
  = pcMiscPrelId coercionTokenName
                 (mkTyConApp eqPrimTyCon [liftedTypeKind, liftedTypeKind, unitTy, unitTy])
                 noCafIdInfo

pcMiscPrelId :: Name -> Type -> IdInfo -> Id
pcMiscPrelId name ty info
  = mkVanillaGlobalWithInfo name ty info

pcRepPolyId :: Name -> Type -> (Name -> ConcreteTyVars) -> IdInfo -> Id
pcRepPolyId name ty conc_tvs info =
  mkGlobalId (RepPolyId $ conc_tvs name) name ty info

-- | Directly specify which outer forall'd type variables of a
-- representation-polymorphic 'Id' such become concrete metavariables when
-- instantiated.
mkRepPolyIdConcreteTyVars :: [((Type, Position Neg), TyVar)]
                               -- ^ ((ty, pos), tv)
                               -- 'ty' is the type on which the representation-polymorphism
                               -- check is done
                               -- 'tv' is the type variable we are checking for concreteness
                               -- (usually the kind of 'ty')
                               -- 'pos' is the position of 'ty' in the
                               -- type of the 'Id'
                          -> Name -- ^ 'Name' of the rep-poly 'Id'
                          -> ConcreteTyVars
mkRepPolyIdConcreteTyVars vars nm =
  mkNameEnv [ (tyVarName tv, mk_conc_frr ty pos)
            | ((ty,pos), tv) <- vars ]
  where
    mk_conc_frr ty pos =
      ConcreteFRR $ FixedRuntimeRepOrigin ty
                  $ FRRRepPolyId nm RepPolyFunction pos
