{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module HsExtension where

-- This module captures the type families to precisely identify the extension
-- points for HsSyn

import GhcPrelude

import GHC.Exts (Constraint)
import Data.Data hiding ( Fixity )
import PlaceHolder
import BasicTypes
import ConLike
import NameSet
import Name
import RdrName
import Var
import Type       ( Type )
import Outputable
import SrcLoc (Located)
import Coercion
import TcEvidence

{-
Note [Trees that grow]
~~~~~~~~~~~~~~~~~~~~~~

See https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow

The hsSyn AST is reused across multiple compiler passes. We also have the
Template Haskell AST, and the haskell-src-exts one (outside of GHC)

Supporting multiple passes means the AST has various warts on it to cope with
the specifics for the phases, such as the 'ValBindsOut', 'ConPatOut',
'SigPatOut' etc.

The growable AST will allow each of these variants to be captured explicitly,
such that they only exist in the given compiler pass AST, as selected by the
type parameter to the AST.

In addition it will allow tool writers to define their own extensions to capture
additional information for the tool, in a natural way.

A further goal is to provide a means to harmonise the Template Haskell and
haskell-src-exts ASTs as well.

-}

-- | Used as a data type index for the hsSyn AST
data GhcPass (c :: Pass)
deriving instance Eq (GhcPass c)
deriving instance Typeable c => Data (GhcPass c)

data Pass = Parsed | Renamed | Typechecked
         deriving (Data)

-- Type synonyms as a shorthand for tagging
type GhcPs   = GhcPass 'Parsed      -- Old 'RdrName' type param
type GhcRn   = GhcPass 'Renamed     -- Old 'Name' type param
type GhcTc   = GhcPass 'Typechecked -- Old 'Id' type para,
type GhcTcId = GhcTc                -- Old 'TcId' type param


-- | Types that are not defined until after type checking
type family PostTc x ty -- Note [Pass sensitive types] in PlaceHolder
type instance PostTc GhcPs ty = PlaceHolder
type instance PostTc GhcRn ty = PlaceHolder
type instance PostTc GhcTc ty = ty

-- | Types that are not defined until after renaming
type family PostRn x ty  -- Note [Pass sensitive types] in PlaceHolder
type instance PostRn GhcPs ty = PlaceHolder
type instance PostRn GhcRn ty = ty
type instance PostRn GhcTc ty = ty

-- | Maps the "normal" id type for a given pass
type family IdP p
type instance IdP GhcPs = RdrName
type instance IdP GhcRn = Name
type instance IdP GhcTc = Id


-- We define a type family for each extension point. This is based on prepending
-- 'X' to the constructor name, for ease of reference.
type family XHsChar x
type family XHsCharPrim x
type family XHsString x
type family XHsStringPrim x
type family XHsInt x
type family XHsIntPrim x
type family XHsWordPrim x
type family XHsInt64Prim x
type family XHsWord64Prim x
type family XHsInteger x
type family XHsRat x
type family XHsFloatPrim x
type family XHsDoublePrim x

-- | Helper to apply a constraint to all extension points. It has one
-- entry per extension point type family.
type ForallX (c :: * -> Constraint) (x :: *) =
  ( c (XHsChar x)
  , c (XHsCharPrim x)
  , c (XHsString x)
  , c (XHsStringPrim x)
  , c (XHsInt x)
  , c (XHsIntPrim x)
  , c (XHsWordPrim x)
  , c (XHsInt64Prim x)
  , c (XHsWord64Prim x)
  , c (XHsInteger x)
  , c (XHsRat x)
  , c (XHsFloatPrim x)
  , c (XHsDoublePrim x)
  )


-- Provide the specific extension types for the parser phase.
type instance XHsChar       GhcPs = SourceText
type instance XHsCharPrim   GhcPs = SourceText
type instance XHsString     GhcPs = SourceText
type instance XHsStringPrim GhcPs = SourceText
type instance XHsInt        GhcPs = ()
type instance XHsIntPrim    GhcPs = SourceText
type instance XHsWordPrim   GhcPs = SourceText
type instance XHsInt64Prim  GhcPs = SourceText
type instance XHsWord64Prim GhcPs = SourceText
type instance XHsInteger    GhcPs = SourceText
type instance XHsRat        GhcPs = ()
type instance XHsFloatPrim  GhcPs = ()
type instance XHsDoublePrim GhcPs = ()

-- Provide the specific extension types for the renamer phase.
type instance XHsChar       GhcRn = SourceText
type instance XHsCharPrim   GhcRn = SourceText
type instance XHsString     GhcRn = SourceText
type instance XHsStringPrim GhcRn = SourceText
type instance XHsInt        GhcRn = ()
type instance XHsIntPrim    GhcRn = SourceText
type instance XHsWordPrim   GhcRn = SourceText
type instance XHsInt64Prim  GhcRn = SourceText
type instance XHsWord64Prim GhcRn = SourceText
type instance XHsInteger    GhcRn = SourceText
type instance XHsRat        GhcRn = ()
type instance XHsFloatPrim  GhcRn = ()
type instance XHsDoublePrim GhcRn = ()

-- Provide the specific extension types for the typechecker phase.
type instance XHsChar       GhcTc = SourceText
type instance XHsCharPrim   GhcTc = SourceText
type instance XHsString     GhcTc = SourceText
type instance XHsStringPrim GhcTc = SourceText
type instance XHsInt        GhcTc = ()
type instance XHsIntPrim    GhcTc = SourceText
type instance XHsWordPrim   GhcTc = SourceText
type instance XHsInt64Prim  GhcTc = SourceText
type instance XHsWord64Prim GhcTc = SourceText
type instance XHsInteger    GhcTc = SourceText
type instance XHsRat        GhcTc = ()
type instance XHsFloatPrim  GhcTc = ()
type instance XHsDoublePrim GhcTc = ()


-- ---------------------------------------------------------------------

-- | The 'SourceText' fields have been moved into the extension fields, thus
-- placing a requirement in the extension field to contain a 'SourceText' so
-- that the pretty printing and round tripping of source can continue to
-- operate.
--
-- The 'HasSourceText' class captures this requirement for the relevant fields.
class HasSourceText a where
  -- Provide setters to mimic existing constructors
  noSourceText  :: a
  sourceText    :: String -> a

  setSourceText :: SourceText -> a
  getSourceText :: a -> SourceText

-- | Provide a summary constraint that lists all the extension points requiring
-- the 'HasSourceText' class, so that it can be changed in one place as the
-- named extensions change throughout the AST.
type SourceTextX x =
  ( HasSourceText (XHsChar x)
  , HasSourceText (XHsCharPrim x)
  , HasSourceText (XHsString x)
  , HasSourceText (XHsStringPrim x)
  , HasSourceText (XHsIntPrim x)
  , HasSourceText (XHsWordPrim x)
  , HasSourceText (XHsInt64Prim x)
  , HasSourceText (XHsWord64Prim x)
  , HasSourceText (XHsInteger x)
  )


-- |  'SourceText' trivially implements 'HasSourceText'
instance HasSourceText SourceText where
  noSourceText    = NoSourceText
  sourceText s    = SourceText s

  setSourceText s = s
  getSourceText a = a


-- ----------------------------------------------------------------------
-- | Defaults for each annotation, used to simplify creation in arbitrary
-- contexts
class HasDefault a where
  def :: a

instance HasDefault () where
  def = ()

instance HasDefault SourceText where
  def = NoSourceText

-- | Provide a single constraint that captures the requirement for a default
-- across all the extension points.
type HasDefaultX x = ForallX HasDefault x

-- ----------------------------------------------------------------------
-- | Conversion of annotations from one type index to another. This is required
-- where the AST is converted from one pass to another, and the extension values
-- need to be brought along if possible. So for example a 'SourceText' is
-- converted via 'id', but needs a type signature to keep the type checker
-- happy.
class Convertable a b  | a -> b where
  convert :: a -> b

instance Convertable a a where
  convert = id

-- | A constraint capturing all the extension points that can be converted via
-- @instance Convertable a a@
type ConvertIdX a b =
  (XHsDoublePrim a ~ XHsDoublePrim b,
   XHsFloatPrim a ~ XHsFloatPrim b,
   XHsRat a ~ XHsRat b,
   XHsInteger a ~ XHsInteger b,
   XHsWord64Prim a ~ XHsWord64Prim b,
   XHsInt64Prim a ~ XHsInt64Prim b,
   XHsWordPrim a ~ XHsWordPrim b,
   XHsIntPrim a ~ XHsIntPrim b,
   XHsInt a ~ XHsInt b,
   XHsStringPrim a ~ XHsStringPrim b,
   XHsString a ~ XHsString b,
   XHsCharPrim a ~ XHsCharPrim b,
   XHsChar a ~ XHsChar b)


-- ----------------------------------------------------------------------

--
type DataId p =
  ( Data p
  , ForallX Data p
  , Data (NameOrRdrName (IdP p))

  , Data (IdP p)
  , Data (PostRn p (IdP p))
  , Data (PostRn p (Located Name))
  , Data (PostRn p Bool)
  , Data (PostRn p Fixity)
  , Data (PostRn p NameSet)
  , Data (PostRn p [Name])

  , Data (PostTc p (IdP p))
  , Data (PostTc p Coercion)
  , Data (PostTc p ConLike)
  , Data (PostTc p HsWrapper)
  , Data (PostTc p Type)
  , Data (PostTc p [ConLike])
  , Data (PostTc p [Type])
  )


-- |Constraint type to bundle up the requirement for 'OutputableBndr' on both
-- the @id@ and the 'NameOrRdrName' type for it
type OutputableBndrId id =
  ( OutputableBndr (NameOrRdrName (IdP id))
  , OutputableBndr (IdP id)
  )
