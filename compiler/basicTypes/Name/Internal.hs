{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Name.Internal where

import {-# SOURCE #-} TyCoRep( TyThing )

import GhcPrelude

import OccName
import Module
import SrcLoc
import Unique
import Util
import Maybes
import Binary
import FastString
import Outputable

import Control.DeepSeq
import Data.Data

{-
************************************************************************
*                                                                      *
\subsection[Name-datatype]{The @Name@ datatype, and name construction}
*                                                                      *
************************************************************************
-}

-- | A unique, unambiguous name for something, containing information about where
-- that thing originated.
data Name = Name {
                n_sort :: NameSort,     -- What sort of name it is
                n_occ  :: !OccName,     -- Its occurrence name
                n_uniq :: {-# UNPACK #-} !Unique,
                n_loc  :: !SrcSpan      -- Definition site
            }

-- NOTE: we make the n_loc field strict to eliminate some potential
-- (and real!) space leaks, due to the fact that we don't look at
-- the SrcLoc in a Name all that often.

-- See Note [About the NameSorts]
data NameSort
  = External Module

  | WiredIn Module TyThing BuiltInSyntax
        -- A variant of External, for wired-in things

  | Internal            -- A user-defined Id or TyVar
                        -- defined in the module being compiled

  | System              -- A system-defined Id or TyVar.  Typically the
                        -- OccName is very uninformative (like 's')

instance NFData Name where
  rnf Name{..} = rnf n_sort

instance NFData NameSort where
  rnf (External m) = rnf m
  rnf (WiredIn m t b) = rnf m `seq` t `seq` b `seq` ()
    -- XXX this is a *lie*, we're not going to rnf the TyThing, but
    -- since the TyThings for WiredIn Names are all static they can't
    -- be hiding space leaks or errors.
  rnf Internal = ()
  rnf System = ()

-- | BuiltInSyntax is for things like @(:)@, @[]@ and tuples,
-- which have special syntactic forms.  They aren't in scope
-- as such.
data BuiltInSyntax = BuiltInSyntax | UserSyntax

{-
Note [About the NameSorts]

1.  Initially, top-level Ids (including locally-defined ones) get External names,
    and all other local Ids get Internal names

2.  In any invocation of GHC, an External Name for "M.x" has one and only one
    unique.  This unique association is ensured via the Name Cache;
    see Note [The Name Cache] in IfaceEnv.

3.  Things with a External name are given C static labels, so they finally
    appear in the .o file's symbol table.  They appear in the symbol table
    in the form M.n.  If originally-local things have this property they
    must be made @External@ first.

4.  In the tidy-core phase, a External that is not visible to an importer
    is changed to Internal, and a Internal that is visible is changed to External

5.  A System Name differs in the following ways:
        a) has unique attached when printing dumps
        b) unifier eliminates sys tyvars in favour of user provs where possible

    Before anything gets printed in interface files or output code, it's
    fed through a 'tidy' processor, which zaps the OccNames to have
    unique names; and converts all sys-locals to user locals
    If any desugarer sys-locals have survived that far, they get changed to
    "ds1", "ds2", etc.

Built-in syntax => It's a syntactic form, not "in scope" (e.g. [])

Wired-in thing  => The thing (Id, TyCon) is fully known to the compiler,
                   not read from an interface file.
                   E.g. Bool, True, Int, Float, and many others

All built-in syntax is for wired-in things.
-}

instance HasOccName Name where
  occName = nameOccName

nameUnique              :: Name -> Unique
nameOccName             :: Name -> OccName
nameSrcLoc              :: Name -> SrcLoc
nameSrcSpan             :: Name -> SrcSpan

nameUnique  name = n_uniq name
nameOccName name = n_occ  name
nameSrcLoc  name = srcSpanStart (n_loc name)
nameSrcSpan name = n_loc  name

type instance SrcSpanLess Name = Name
instance HasSrcSpan Name where
  composeSrcSpan   (L sp  n) = n {n_loc = sp}
  decomposeSrcSpan n         = L (n_loc n) n
