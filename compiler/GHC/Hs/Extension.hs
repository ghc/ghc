{-# LANGUAGE AllowAmbiguousTypes     #-}      -- for pprIfTc, etc.
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveDataTypeable      #-}
{-# LANGUAGE EmptyDataDeriving       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableSuperClasses #-} -- for IsPass; see Note [NoGhcTc]
{-# LANGUAGE UndecidableInstances    #-} -- Wrinkle in Note [Trees That Grow]
                                         -- in module Language.Haskell.Syntax.Extension

module GHC.Hs.Extension where

-- This module captures the type families to precisely identify the extension
-- points for GHC.Hs syntax

import GHC.Prelude

import Data.Data hiding ( Fixity )
import Language.Haskell.Syntax.Extension
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.Var
import GHC.Utils.Outputable hiding ((<>))
import GHC.Types.SrcLoc (GenLocated(..), unLoc)
import GHC.Utils.Panic
import GHC.Parser.Annotation

{-
Note [IsPass]
~~~~~~~~~~~~~
One challenge with the Trees That Grow approach
is that we sometimes have different information in different passes.
For example, we have

  type instance XViaStrategy GhcPs = LHsSigType GhcPs
  type instance XViaStrategy GhcRn = LHsSigType GhcRn
  type instance XViaStrategy GhcTc = Type

This means that printing a DerivStrategy (which contains an XViaStrategy)
might need to print a LHsSigType, or it might need to print a type. Yet we
want one Outputable instance for a DerivStrategy, instead of one per pass. We
could have a large constraint, including e.g. (Outputable (XViaStrategy p),
Outputable (XViaStrategy GhcTc)), and pass that around in every context where
we might output a DerivStrategy. But a simpler alternative is to pass a
witness to whichever pass we're in. When we pattern-match on that (GADT)
witness, we learn the pass identity and can then print away. To wit, we get
the definition of GhcPass and the functions isPass. These allow us to do away
with big constraints, passing around all manner of dictionaries we might or
might not use. It does mean that we have to manually use isPass when printing,
but these places are few.

See Note [NoGhcTc] about the superclass constraint to IsPass.

Note [NoGhcTc]
~~~~~~~~~~~~~~
An expression is parsed into HsExpr GhcPs, renamed into HsExpr GhcRn, and
then type-checked into HsExpr GhcTc. Not so for types! These get parsed
into HsType GhcPs, renamed into HsType GhcRn, and then type-checked into
Type. We never build an HsType GhcTc. Why do this? Because we need to be
able to compare type-checked types for equality, and we don't want to do
this with HsType.

This causes wrinkles within the AST, where we normally think that the whole
AST travels through the GhcPs --> GhcRn --> GhcTc pipeline as one. So we
have the NoGhcTc type family, which just replaces GhcTc with GhcRn, so that
user-written types can be preserved (as HsType GhcRn) even in e.g. HsExpr GhcTc.

For example, this is used in ExprWithTySig:
    | ExprWithTySig
                (XExprWithTySig p)

                (LHsExpr p)
                (LHsSigWcType (NoGhcTc p))

If we have (e :: ty), we still want to be able to print that (with the :: ty)
after type-checking. So we retain the LHsSigWcType GhcRn, even in an
HsExpr GhcTc. That's what NoGhcTc does.

When we're printing the type annotation, we need to know
(Outputable (LHsSigWcType GhcRn)), even though we've assumed only that
(OutputableBndrId GhcTc). We thus must be able to prove OutputableBndrId (NoGhcTc p)
from OutputableBndrId p. The extra constraints in OutputableBndrId and
the superclass constraints of IsPass allow this. Note that the superclass
constraint of IsPass is *recursive*: it asserts that IsPass (NoGhcTcPass p) holds.
For this to make sense, we need -XUndecidableSuperClasses and the other constraint,
saying that NoGhcTcPass is idempotent.

-}

-- See Note [XRec and Anno in the AST] in GHC.Parser.Annotation
type instance XRec (GhcPass p) a = GenLocated (Anno a) a

type instance Anno RdrName = SrcSpanAnnN
type instance Anno Name    = SrcSpanAnnN
type instance Anno Id      = SrcSpanAnnN

type IsSrcSpanAnn p a = ( Anno (IdGhcP p) ~ SrcSpanAnn' (EpAnn a),
                          IsPass p)

instance UnXRec (GhcPass p) where
  unXRec = unLoc
instance MapXRec (GhcPass p) where
  mapXRec = fmap

-- instance WrapXRec (GhcPass p) a where
--   wrapXRec = noLocA

{-
Note [DataConCantHappen and strict fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently, any unused TTG extension constructor will generally look like the
following:

  type instance XXHsDecl (GhcPass _) = DataConCantHappen
  data HsDecl p
    = ...
    | XHsDecl !(XXHsDecl p)

The field of type `XXHsDecl p` is strict for a good reason: it allows the
pattern-match coverage checker to conclude that any matches against XHsDecl
are unreachable whenever `p ~ GhcPass _`. To see why this is the case, consider
the following function which consumes an HsDecl:

  ex :: HsDecl GhcPs -> HsDecl GhcRn
  ...
  ex (XHsDecl nec) = dataConCantHappen nec

Because `p` equals GhcPs (i.e., GhcPass 'Parsed), XHsDecl's field has the type
DataConCantHappen. But since (1) the field is strict and (2) DataConCantHappen
is an empty data type, there is no possible way to reach the right-hand side
of the XHsDecl case. As a result, the coverage checker concludes that
the XHsDecl case is inaccessible, so it can be removed.
(See Note [Strict argument type constraints] in GHC.HsToCore.Pmc.Solver for
more on how this works.)

Bottom line: if you add a TTG extension constructor that uses DataConCantHappen, make
sure that any uses of it as a field are strict.
-}

-- | Used as a data type index for the hsSyn AST; also serves
-- as a singleton type for Pass
data GhcPass (c :: Pass) where
  GhcPs :: GhcPass 'Parsed
  GhcRn :: GhcPass 'Renamed
  GhcTc :: GhcPass 'Typechecked

-- This really should never be entered, but the data-deriving machinery
-- needs the instance to exist.
instance Typeable p => Data (GhcPass p) where
  gunfold _ _ _ = panic "instance Data GhcPass"
  toConstr  _   = panic "instance Data GhcPass"
  dataTypeOf _  = panic "instance Data GhcPass"

data Pass = Parsed | Renamed | Typechecked
         deriving (Data)

-- Type synonyms as a shorthand for tagging
type GhcPs   = GhcPass 'Parsed      -- Output of parser
type GhcRn   = GhcPass 'Renamed     -- Output of renamer
type GhcTc   = GhcPass 'Typechecked -- Output of typechecker

-- | Allows us to check what phase we're in at GHC's runtime.
-- For example, this class allows us to write
-- >  f :: forall p. IsPass p => HsExpr (GhcPass p) -> blah
-- >  f e = case ghcPass @p of
-- >          GhcPs ->    ... in this RHS we have HsExpr GhcPs...
-- >          GhcRn ->    ... in this RHS we have HsExpr GhcRn...
-- >          GhcTc ->    ... in this RHS we have HsExpr GhcTc...
-- which is very useful, for example, when pretty-printing.
-- See Note [IsPass].
class ( NoGhcTcPass (NoGhcTcPass p) ~ NoGhcTcPass p
      , IsPass (NoGhcTcPass p)
      ) => IsPass p where
  ghcPass :: GhcPass p

instance IsPass 'Parsed where
  ghcPass = GhcPs
instance IsPass 'Renamed where
  ghcPass = GhcRn
instance IsPass 'Typechecked where
  ghcPass = GhcTc

type instance IdP (GhcPass p) = IdGhcP p

-- | Maps the "normal" id type for a given GHC pass
type family IdGhcP pass where
  IdGhcP 'Parsed      = RdrName
  IdGhcP 'Renamed     = Name
  IdGhcP 'Typechecked = Id

-- | Marks that a field uses the GhcRn variant even when the pass
-- parameter is GhcTc. Useful for storing HsTypes in GHC.Hs.Exprs, say, because
-- HsType GhcTc should never occur.
-- See Note [NoGhcTc]

-- Breaking it up this way, GHC can figure out that the result is a GhcPass
type instance NoGhcTc (GhcPass pass) = GhcPass (NoGhcTcPass pass)

type family NoGhcTcPass (p :: Pass) :: Pass where
  NoGhcTcPass 'Typechecked = 'Renamed
  NoGhcTcPass other        = other

-- |Constraint type to bundle up the requirement for 'OutputableBndr' on both
-- the @id@ and the 'NoGhcTc' of it. See Note [NoGhcTc].
type OutputableBndrId pass =
  ( OutputableBndr (IdGhcP pass)
  , OutputableBndr (IdGhcP (NoGhcTcPass pass))
  , Outputable (GenLocated (Anno (IdGhcP pass)) (IdGhcP pass))
  , Outputable (GenLocated (Anno (IdGhcP (NoGhcTcPass pass))) (IdGhcP (NoGhcTcPass pass)))
  , IsPass pass
  )

-- useful helper functions:
pprIfPs :: forall p. IsPass p => (p ~ 'Parsed => SDoc) -> SDoc
pprIfPs pp = case ghcPass @p of GhcPs -> pp
                                _     -> empty

pprIfRn :: forall p. IsPass p => (p ~ 'Renamed => SDoc) -> SDoc
pprIfRn pp = case ghcPass @p of GhcRn -> pp
                                _     -> empty

pprIfTc :: forall p. IsPass p => (p ~ 'Typechecked => SDoc) -> SDoc
pprIfTc pp = case ghcPass @p of GhcTc -> pp
                                _     -> empty

type instance Anno (HsToken tok) = TokenLocation

noHsTok :: GenLocated TokenLocation (HsToken tok)
noHsTok = L NoTokenLoc HsTok

type instance Anno (HsUniToken tok utok) = TokenLocation

noHsUniTok :: GenLocated TokenLocation (HsUniToken tok utok)
noHsUniTok = L NoTokenLoc HsNormalTok
