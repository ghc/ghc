{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module PlaceHolder where

import Type       ( Type )
import Outputable
import Name
import NameSet
import RdrName
import Var
import Coercion
import ConLike (ConLike)
import FieldLabel
import SrcLoc (Located)
import TcEvidence ( HsWrapper )

import Data.Data hiding ( Fixity )
import BasicTypes       (Fixity)


{-
%************************************************************************
%*                                                                      *
\subsection{Annotating the syntax}
%*                                                                      *
%************************************************************************
-}

-- NB: These are intentionally open, allowing API consumers (like Haddock)
-- to declare new instances

-- | used as place holder in PostTc and PostRn values
data PlaceHolder = PlaceHolder
  deriving (Data)

-- | Types that are not defined until after type checking
type family PostTc id ty  -- Note [Pass sensitive types]
type instance PostTc Id      ty = ty
type instance PostTc Name    ty = PlaceHolder
type instance PostTc RdrName ty = PlaceHolder

-- | Types that are not defined until after renaming
type family PostRn id ty  -- Note [Pass sensitive types]
type instance PostRn Id      ty = ty
type instance PostRn Name    ty = ty
type instance PostRn RdrName ty = PlaceHolder

placeHolderKind :: PlaceHolder
placeHolderKind = PlaceHolder

placeHolderFixity :: PlaceHolder
placeHolderFixity = PlaceHolder

placeHolderType :: PlaceHolder
placeHolderType = PlaceHolder

placeHolderTypeTc :: Type
placeHolderTypeTc = panic "Evaluated the place holder for a PostTcType"

placeHolderNames :: PlaceHolder
placeHolderNames = PlaceHolder

placeHolderNamesTc :: NameSet
placeHolderNamesTc = emptyNameSet

placeHolderHsWrapper :: PlaceHolder
placeHolderHsWrapper = PlaceHolder

{-

Note [Pass sensitive types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since the same AST types are re-used through parsing,renaming and type
checking there are naturally some places in the AST that do not have
any meaningful value prior to the pass they are assigned a value.

Historically these have been filled in with place holder values of the form

  panic "error message"

This has meant the AST is difficult to traverse using standard generic
programming techniques. The problem is addressed by introducing
pass-specific data types, implemented as a pair of open type families,
one for PostTc and one for PostRn. These are then explicitly populated
with a PlaceHolder value when they do not yet have meaning.

In terms of actual usage, we have the following

  PostTc id Kind
  PostTc id Type

  PostRn id Fixity
  PostRn id NameSet

TcId and Var are synonyms for Id

Unfortunately the type checker termination checking conditions fail for the
DataId constraint type based on this, so even though it is safe the
UndecidableInstances pragma is required where this is used.
-}

type DataId id =
  ( DataIdPost id
  , DataIdPost (NameOrRdrName id)
  )

type DataIdPost id =
  ( Data id
  , Data (PostRn id NameSet)
  , Data (PostRn id Fixity)
  , Data (PostRn id Bool)
  , Data (PostRn id Name)
  , Data (PostRn id (Located Name))
  , Data (PostRn id [Name])

  , Data (PostRn id id)
  , Data (PostTc id Type)
  , Data (PostTc id Coercion)
  , Data (PostTc id id)
  , Data (PostTc id [Type])
  , Data (PostTc id ConLike)
  , Data (PostTc id [ConLike])
  , Data (PostTc id HsWrapper)
  , Data (PostTc id [FieldLabel])
  )


-- |Follow the @id@, but never beyond Name. This is used in a 'HsMatchContext',
-- for printing messages related to a 'Match'
type family NameOrRdrName id where
  NameOrRdrName Id      = Name
  NameOrRdrName Name    = Name
  NameOrRdrName RdrName = RdrName

-- |Constraint type to bundle up the requirement for 'OutputableBndr' on both
-- the @id@ and the 'NameOrRdrName' type for it
type OutputableBndrId id =
  ( OutputableBndr id
  , OutputableBndr (NameOrRdrName id)
  )

-- |Constraint type to bundle up the requirement for 'HasOccName' on both
-- the @id@ and the 'NameOrRdrName' type for it
type HasOccNameId id =
  ( HasOccName id
  , HasOccName (NameOrRdrName id)
  )
