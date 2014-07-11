{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module PlaceHolder where

import Type       ( Type )
import Outputable
import Name
import NameSet
import RdrName
import Var
import Coercion

import Data.Data hiding ( Fixity )
import BasicTypes       (Fixity)


{-
%************************************************************************
%*                                                                      *
\subsection{Annotating the syntax}
%*                                                                      *
%************************************************************************
-}

-- | used as place holder in PostTc and PostRn values
data PlaceHolder = PlaceHolder
  deriving (Data,Typeable)

-- | Types that are not defined until after type checking
type family PostTc it ty :: * -- Note [Pass sensitive types]
type instance PostTc Id      ty = ty
type instance PostTc Name    ty = PlaceHolder
type instance PostTc RdrName ty = PlaceHolder

-- | Types that are not defined until after renaming
type family PostRn id ty :: * -- Note [Pass sensitive types]
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

Since the required bootstrap compiler at this stage does not have
closed type families, an open type family had to be used, which
unfortunately forces the requirement for UndecidableInstances.

In terms of actual usage, we have the following

  PostTc id Kind
  PostTc id Type

  PostRn id Fixity
  PostRn id NameSet

TcId and Var are synonyms for Id
-}

type DataId id =
  ( Data id
  , Data (PostRn id NameSet)
  , Data (PostRn id Fixity)
  , Data (PostRn id Bool)
  , Data (PostRn id Name)
  , Data (PostRn id [Name])

  , Data (PostTc id Type)
  , Data (PostTc id Coercion)
  )
