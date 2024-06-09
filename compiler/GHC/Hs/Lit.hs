{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable, OutputableBndrId

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Source-language literals
module GHC.Hs.Lit
  ( module Language.Haskell.Syntax.Lit
  , module GHC.Hs.Lit
  ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Hs.Expr( pprExpr )

import GHC.Types.Basic (PprPrec(..), topPrec )
import GHC.Core.Ppr ( {- instance OutputableBndr TyVar -} )
import GHC.Types.SourceText
import GHC.Core.Type
import GHC.Utils.Outputable
import GHC.Hs.Extension
import Language.Haskell.Syntax.Expr ( HsExpr )
import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Lit
import GHC.Utils.Panic (panic)

{-
************************************************************************
*                                                                      *
\subsection[HsLit]{Literals}
*                                                                      *
************************************************************************
-}

type instance XHsChar       (GhcPass _) = SourceText
type instance XHsCharPrim   (GhcPass _) = SourceText
type instance XHsString     (GhcPass _) = SourceText
type instance XHsStringPrim (GhcPass _) = SourceText
type instance XHsInt        (GhcPass _) = NoExtField
type instance XHsIntPrim    (GhcPass _) = SourceText
type instance XHsWordPrim   (GhcPass _) = SourceText
type instance XHsInt8Prim   (GhcPass _) = SourceText
type instance XHsInt16Prim  (GhcPass _) = SourceText
type instance XHsInt32Prim  (GhcPass _) = SourceText
type instance XHsInt64Prim  (GhcPass _) = SourceText
type instance XHsWord8Prim  (GhcPass _) = SourceText
type instance XHsWord16Prim (GhcPass _) = SourceText
type instance XHsWord32Prim (GhcPass _) = SourceText
type instance XHsWord64Prim (GhcPass _) = SourceText
type instance XHsInteger    (GhcPass _) = SourceText
type instance XHsRat        (GhcPass _) = NoExtField
type instance XHsFloatPrim  (GhcPass _) = NoExtField
type instance XHsDoublePrim (GhcPass _) = NoExtField
type instance XXLit         (GhcPass _) = DataConCantHappen

data OverLitRn
  = OverLitRn {
        ol_rebindable :: Bool,         -- Note [ol_rebindable]
        ol_from_fun   :: LIdP GhcRn    -- Note [Overloaded literal witnesses]
        }

data OverLitTc
  = OverLitTc {
        ol_rebindable :: Bool,         -- Note [ol_rebindable]
        ol_witness    :: HsExpr GhcTc, -- Note [Overloaded literal witnesses]
        ol_type :: Type }

{-
Note [Overloaded literal witnesses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

During renaming, the coercion function needed for a given HsOverLit is
resolved according to the current scope and RebindableSyntax (see Note
[ol_rebindable]). The result of this resolution *before* type checking
is the coercion function such as 'fromInteger' or 'fromRational',
stored in the ol_from_fun field of OverLitRn.

*After* type checking, the ol_witness field of the OverLitTc contains
the witness of the literal as HsExpr, such as (fromInteger 3) or
lit_78. This witness should replace the literal. Reason: it allows
commoning up of the fromInteger calls, which wouldn't be possible if
the desugarer made the application.

The ol_type in OverLitTc records the type the overloaded literal is
found to have.
-}

type instance XOverLit GhcPs = NoExtField
type instance XOverLit GhcRn = OverLitRn
type instance XOverLit GhcTc = OverLitTc

pprXOverLit :: GhcPass p -> XOverLit (GhcPass p) -> SDoc
pprXOverLit GhcPs noExt = ppr noExt
pprXOverLit GhcRn OverLitRn{ ol_from_fun = from_fun } = ppr from_fun
pprXOverLit GhcTc OverLitTc{ ol_witness = witness } = pprExpr witness

type instance XXOverLit (GhcPass _) = DataConCantHappen

overLitType :: HsOverLit GhcTc -> Type
overLitType (OverLit OverLitTc{ ol_type = ty } _) = ty

-- | @'hsOverLitNeedsParens' p ol@ returns 'True' if an overloaded literal
-- @ol@ needs to be parenthesized under precedence @p@.
hsOverLitNeedsParens :: PprPrec -> HsOverLit x -> Bool
hsOverLitNeedsParens p (OverLit { ol_val = olv }) = go olv
  where
    go :: OverLitVal -> Bool
    go (HsIntegral x)   = p > topPrec && il_neg x
    go (HsFractional x) = p > topPrec && fl_neg x
    go (HsIsString {})  = False
hsOverLitNeedsParens _ (XOverLit { }) = False

-- | @'hsLitNeedsParens' p l@ returns 'True' if a literal @l@ needs
-- to be parenthesized under precedence @p@.
--
-- See Note [Printing of literals in Core] in GHC.Types.Literal
-- for the reasoning.
hsLitNeedsParens :: PprPrec -> HsLit x -> Bool
hsLitNeedsParens p = go
  where
    go (HsChar {})        = False
    go (HsCharPrim {})    = False
    go (HsString {})      = False
    go (HsStringPrim {})  = False
    go (HsInt _ x)        = p > topPrec && il_neg x
    go (HsInteger _ x _)  = p > topPrec && x < 0
    go (HsRat _ x _)      = p > topPrec && fl_neg x
    go (HsFloatPrim {})   = False
    go (HsDoublePrim {})  = False
    go (HsIntPrim {})     = False
    go (HsInt8Prim {})    = False
    go (HsInt16Prim {})   = False
    go (HsInt32Prim {})   = False
    go (HsInt64Prim {})   = False
    go (HsWordPrim {})    = False
    go (HsWord8Prim {})   = False
    go (HsWord16Prim {})  = False
    go (HsWord64Prim {})  = False
    go (HsWord32Prim {})  = False
    go (XLit _)           = False

-- | Convert a literal from one index type to another
convertLit :: HsLit (GhcPass p1) -> HsLit (GhcPass p2)
convertLit (HsChar a x)       = HsChar a x
convertLit (HsCharPrim a x)   = HsCharPrim a x
convertLit (HsString a x)     = HsString a x
convertLit (HsStringPrim a x) = HsStringPrim a x
convertLit (HsInt a x)        = HsInt a x
convertLit (HsIntPrim a x)    = HsIntPrim a x
convertLit (HsWordPrim a x)   = HsWordPrim a x
convertLit (HsInt8Prim a x)   = HsInt8Prim a x
convertLit (HsInt16Prim a x)  = HsInt16Prim a x
convertLit (HsInt32Prim a x)  = HsInt32Prim a x
convertLit (HsInt64Prim a x)  = HsInt64Prim a x
convertLit (HsWord8Prim a x)  = HsWord8Prim a x
convertLit (HsWord16Prim a x) = HsWord16Prim a x
convertLit (HsWord32Prim a x) = HsWord32Prim a x
convertLit (HsWord64Prim a x) = HsWord64Prim a x
convertLit (HsInteger a x b)  = HsInteger a x b
convertLit (HsRat a x b)      = HsRat a x b
convertLit (HsFloatPrim a x)  = HsFloatPrim a x
convertLit (HsDoublePrim a x) = HsDoublePrim a x

{-
Note [ol_rebindable]
~~~~~~~~~~~~~~~~~~~~
The ol_rebindable field is True if this literal is actually
using rebindable syntax.  Specifically:

  False iff ol_from_fun / ol_witness is the standard one
  True  iff ol_from_fun / ol_witness is non-standard

Equivalently it's True if
  a) RebindableSyntax is on
  b) the witness for fromInteger/fromRational/fromString
     that happens to be in scope isn't the standard one
-}

-- Instance specific to GhcPs, need the SourceText
instance Outputable (HsLit (GhcPass p)) where
    ppr (HsChar st c)       = pprWithSourceText st (pprHsChar c)
    ppr (HsCharPrim st c)   = pprWithSourceText st (pprPrimChar c)
    ppr (HsString st s)     = pprWithSourceText st (pprHsString s)
    ppr (HsStringPrim st s) = pprWithSourceText st (pprHsBytes s)
    ppr (HsInt _ i)
      = pprWithSourceText (il_text i) (integer (il_value i))
    ppr (HsInteger st i _)  = pprWithSourceText st (integer i)
    ppr (HsRat _ f _)       = ppr f
    ppr (HsFloatPrim _ f)   = ppr f <> primFloatSuffix
    ppr (HsDoublePrim _ d)  = ppr d <> primDoubleSuffix
    ppr (HsIntPrim st i)    = pprWithSourceText st (pprPrimInt i)
    ppr (HsInt8Prim st i)   = pprWithSourceText st (pprPrimInt8 i)
    ppr (HsInt16Prim st i)  = pprWithSourceText st (pprPrimInt16 i)
    ppr (HsInt32Prim st i)  = pprWithSourceText st (pprPrimInt32 i)
    ppr (HsInt64Prim st i)  = pprWithSourceText st (pprPrimInt64 i)
    ppr (HsWordPrim st w)   = pprWithSourceText st (pprPrimWord w)
    ppr (HsWord8Prim st w)  = pprWithSourceText st (pprPrimWord8 w)
    ppr (HsWord16Prim st w) = pprWithSourceText st (pprPrimWord16 w)
    ppr (HsWord32Prim st w) = pprWithSourceText st (pprPrimWord32 w)
    ppr (HsWord64Prim st w) = pprWithSourceText st (pprPrimWord64 w)

-- in debug mode, print the expression that it's resolved to, too
instance OutputableBndrId p
       => Outputable (HsOverLit (GhcPass p)) where
  ppr (OverLit {ol_val=val, ol_ext=ext})
        = ppr val <+> (whenPprDebug (parens (pprXOverLit (ghcPass @p) ext)))

instance Outputable OverLitVal where
  ppr (HsIntegral i)     = pprWithSourceText (il_text i) (integer (il_value i))
  ppr (HsFractional f)   = ppr f
  ppr (HsIsString st s)  = pprWithSourceText st (pprHsString s)

-- | pmPprHsLit pretty prints literals and is used when pretty printing pattern
-- match warnings. All are printed the same (i.e., without hashes if they are
-- primitive and not wrapped in constructors if they are boxed). This happens
-- mainly for too reasons:
--  * We do not want to expose their internal representation
--  * The warnings become too messy
pmPprHsLit :: HsLit (GhcPass x) -> SDoc
pmPprHsLit (HsChar _ c)       = pprHsChar c
pmPprHsLit (HsCharPrim _ c)   = pprHsChar c
pmPprHsLit (HsString st s)    = pprWithSourceText st (pprHsString s)
pmPprHsLit (HsStringPrim _ s) = pprHsBytes s
pmPprHsLit (HsInt _ i)        = integer (il_value i)
pmPprHsLit (HsIntPrim _ i)    = integer i
pmPprHsLit (HsWordPrim _ w)   = integer w
pmPprHsLit (HsInt8Prim _ i)   = integer i
pmPprHsLit (HsInt16Prim _ i)  = integer i
pmPprHsLit (HsInt32Prim _ i)  = integer i
pmPprHsLit (HsInt64Prim _ i)  = integer i
pmPprHsLit (HsWord8Prim _ w)  = integer w
pmPprHsLit (HsWord16Prim _ w) = integer w
pmPprHsLit (HsWord32Prim _ w) = integer w
pmPprHsLit (HsWord64Prim _ w) = integer w
pmPprHsLit (HsInteger _ i _)  = integer i
pmPprHsLit (HsRat _ f _)      = ppr f
pmPprHsLit (HsFloatPrim _ f)  = ppr f
pmPprHsLit (HsDoublePrim _ d) = ppr d

negateOverLitVal :: OverLitVal -> OverLitVal
negateOverLitVal (HsIntegral i) = HsIntegral (negateIntegralLit i)
negateOverLitVal (HsFractional f) = HsFractional (negateFractionalLit f)
negateOverLitVal _ = panic "negateOverLitVal: argument is not a number"

instance (Ord (XXOverLit p)) => Ord (HsOverLit p) where
  compare (OverLit _ val1)  (OverLit _ val2) = val1 `compare` val2
  compare (XOverLit  val1)  (XOverLit  val2) = val1 `compare` val2
  compare _ _ = panic "Ord HsOverLit"

-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module GHC.HsToCore.Match.Literal)
instance (Eq (XXOverLit p)) => Eq (HsOverLit p) where
  (OverLit _ val1) == (OverLit _ val2) = val1 == val2
  (XOverLit  val1) == (XOverLit  val2) = val1 == val2
  _ == _ = panic "Eq HsOverLit"
