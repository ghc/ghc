-- (c) The University of Glasgow 2006
-- (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
--
-- The @Class@ datatype


module GHC.Core.Class (
        Class,
        ClassOpItem,
        ClassATItem(..), TyFamEqnValidityInfo(..),
        ClassMinimalDef,
        DefMethInfo, pprDefMethInfo,

        FunDep, pprFundeps, pprFunDep,

        mkClass, mkAbstractClass, classTyVars, classArity,
        classKey, className, classATs, classATItems, classTyCon, classMethods,
        classOpItems, classBigSig, classExtraBigSig, classTvsFds, classSCTheta,
        classHasSCs, classAllSelIds, classSCSelId, classSCSelIds, classMinimalDef,
        classHasFds, isAbstractClass,
    ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Core.TyCon    ( TyCon )
import {-# SOURCE #-} GHC.Core.TyCo.Rep ( Type, PredType )
import {-# SOURCE #-} GHC.Core.TyCo.Ppr ( pprType )
import GHC.Hs.Extension (GhcRn)
import GHC.Types.Var
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Types.Unique
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Types.SrcLoc
import GHC.Types.Var.Set
import GHC.Utils.Outputable
import Language.Haskell.Syntax.BooleanFormula ( BooleanFormula, mkTrue )

import qualified Data.Data as Data

{-
************************************************************************
*                                                                      *
\subsection[Class-basic]{@Class@: basic definition}
*                                                                      *
************************************************************************

A @Class@ corresponds to a Greek kappa in the static semantics:
-}

data Class
  = Class {
        classTyCon :: TyCon,    -- The data type constructor for
                                -- dictionaries of this class
                                -- See Note [ATyCon for classes] in GHC.Core.TyCo.Rep

        className :: Name,              -- Just the cached name of the TyCon
        classKey  :: Unique,            -- Cached unique of TyCon

        classTyVars  :: [TyVar],        -- The class kind and type variables;
                                        -- identical to those of the TyCon
           -- If you want visibility info, look at the classTyCon
           -- This field is redundant because it's duplicated in the
           -- classTyCon, but classTyVars is used quite often, so maybe
           -- it's a bit faster to cache it here

        classFunDeps :: [FunDep TyVar],  -- The functional dependencies

        classBody :: ClassBody -- Superclasses, ATs, methods

     }

--  | e.g.
--
-- >  class C a b c | a b -> c, a c -> b where...
--
--  Here fun-deps are [([a,b],[c]), ([a,c],[b])]
type FunDep a = ([a],[a])

type ClassOpItem = (Id, DefMethInfo)
        -- Selector function; contains unfolding
        -- Default-method info

type DefMethInfo = Maybe (Name, DefMethSpec Type)
   -- Nothing                    No default method
   -- Just ($dm, VanillaDM)      A polymorphic default method, name $dm
   -- Just ($gm, GenericDM ty)   A generic default method, name $gm, type ty
   --                              The generic dm type is *not* quantified
   --                              over the class variables; ie has the
   --                              class variables free

data ClassATItem
  = ATI TyCon         -- See Note [Associated type tyvar names]
        (Maybe (Type, TyFamEqnValidityInfo))
          -- ^ Default associated type (if any) from this template.
          --
          -- As per Note [Associated type defaults], the Type has been renamed
          -- to use the class tyvars, while the 'TyFamEqnValidityInfo' uses
          -- the original user-written type variables.

-- | Information about a type family equation, used for validity checking
-- of closed type family equations and associated type family default equations.
--
-- This type exists to delay validity-checking after typechecking type declaration
-- groups, to avoid cyclic evaluation inside the typechecking knot.
--
-- See @Note [Type-checking default assoc decls]@ in "GHC.Tc.TyCl".
data TyFamEqnValidityInfo
  -- | Used for equations which don't need any validity checking,
  -- for example equations imported from another module.
  = NoVI

  -- | Information necessary for validity checking of a type family equation.
  | VI
    { vi_loc :: SrcSpan
    , vi_qtvs :: [TcTyVar]
      -- ^ LHS quantified type variables
    , vi_non_user_tvs :: TyVarSet
      -- ^ non-user-written type variables (for error message reporting)
      --
      -- Example: with -XPolyKinds, typechecking @type instance forall a. F = ()@
      -- introduces the kind variable @k@ for the kind of @a@. See #23734.
    , vi_pats :: [Type]
      -- ^ LHS patterns
    , vi_rhs :: Type
      -- ^ RHS of the equation
      --
      -- NB: for associated type family default declarations, this is the RHS
      -- *before* applying the substitution from
      -- Note [Type-checking default assoc decls] in GHC.Tc.TyCl.
    }

type ClassMinimalDef = BooleanFormula GhcRn -- Required methods

data ClassBody
  = AbstractClass
  | ConcreteClass {
        -- Superclasses: eg: (F a ~ b, F b ~ G a, Eq a, Show b)
        -- We need value-level selectors for both the dictionary
        -- superclasses and the equality superclasses
        cls_sc_theta :: [PredType],     -- Immediate superclasses,
        cls_sc_sel_ids :: [Id],          -- Selector functions to extract the
                                        --   superclasses from a
                                        --   dictionary of this class
        -- Associated types
        cls_ats :: [ClassATItem],  -- Associated type families

        -- Class operations (methods, not superclasses)
        cls_ops :: [ClassOpItem],  -- Ordered by tag

        -- Minimal complete definition
        cls_min_def :: ClassMinimalDef
    }
    -- TODO: maybe super classes should be allowed in abstract class definitions

classMinimalDef :: Class -> ClassMinimalDef
classMinimalDef Class{ classBody = ConcreteClass{ cls_min_def = d } } = d
classMinimalDef _ = mkTrue -- TODO: make sure this is the right direction

{-
Note [Associated type defaults]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following is an example of associated type defaults:
   class C a where
     data D a r

     type F x a b :: *
     type F p q r = (p,q)->r    -- Default

Note that

 * The TyCons for the associated types *share type variables* with the
   class, so that we can tell which argument positions should be
   instantiated in an instance decl.  (The first for 'D', the second
   for 'F'.)

 * We can have default definitions only for *type* families,
   not data families

 * In the default decl, the "patterns" should all be type variables,
   but (in the source language) they don't need to be the same as in
   the 'type' decl signature or the class.  It's more like a
   free-standing 'type instance' declaration.

 * HOWEVER, in the internal ClassATItem we rename the RHS to match the
   tyConTyVars of the family TyCon.  So in the example above we'd get
   a ClassATItem of

        ATI F (Just ((x,a) -> b, validity_info)

   That is, the type stored in the first component of the pair has been
   renamed to use the class type variables. On the other hand, the
   TyFamEqnValidityInfo, used for validity checking of the type family equation
   (considered as a free-standing equation) uses the original types, e.g.
   involving the type variables 'p', 'q', 'r'.

The @mkClass@ function fills in the indirect superclasses.

The SrcSpan is for the entire original declaration.
-}

mkClass :: Name -> [TyVar]
        -> [FunDep TyVar]
        -> [PredType] -> [Id]
        -> [ClassATItem]
        -> [ClassOpItem]
        -> ClassMinimalDef
        -> TyCon
        -> Class

mkClass cls_name tyvars fds super_classes superdict_sels at_stuff
        op_stuff mindef tycon
  = Class { classKey     = nameUnique cls_name,
            className    = cls_name,
                -- NB:  tyConName tycon = cls_name,
                -- But it takes a module loop to assert it here
            classTyVars  = tyvars,
            classFunDeps = fds,
            classBody = ConcreteClass {
                    cls_sc_theta = super_classes,
                    cls_sc_sel_ids = superdict_sels,
                    cls_ats  = at_stuff,
                    cls_ops  = op_stuff,
                    cls_min_def = mindef
                },
            classTyCon   = tycon }

mkAbstractClass :: Name -> [TyVar]
        -> [FunDep TyVar]
        -> TyCon
        -> Class

mkAbstractClass cls_name tyvars fds tycon
  = Class { classKey     = nameUnique cls_name,
            className    = cls_name,
                -- NB:  tyConName tycon = cls_name,
                -- But it takes a module loop to assert it here
            classTyVars  = tyvars,
            classFunDeps = fds,
            classBody = AbstractClass,
            classTyCon   = tycon }

{-
Note [Associated type tyvar names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The TyCon of an associated type should use the same variable names as its
parent class. Thus
    class C a b where
      type F b x a :: *
We make F use the same Name for 'a' as C does, and similarly 'b'.

The reason for this is when checking instances it's easier to match
them up, to ensure they match.  Eg
    instance C Int [d] where
      type F [d] x Int = ....
we should make sure that the first and third args match the instance
header.

Having the same variables for class and tycon is also used in checkValidRoles
(in GHC.Tc.TyCl) when checking a class's roles.


************************************************************************
*                                                                      *
\subsection[Class-selectors]{@Class@: simple selectors}
*                                                                      *
************************************************************************

The rest of these functions are just simple selectors.
-}

classArity :: Class -> Arity
classArity clas = length (classTyVars clas)
        -- Could memoise this

classAllSelIds :: Class -> [Id]
-- Both superclass-dictionary and method selectors
classAllSelIds c@(Class { classBody = ConcreteClass { cls_sc_sel_ids = sc_sels }})
  = sc_sels ++ classMethods c
classAllSelIds c = assert (null (classMethods c) ) []

classSCSelIds :: Class -> [Id]
-- Both superclass-dictionary and method selectors
classSCSelIds (Class { classBody = ConcreteClass { cls_sc_sel_ids = sc_sels }})
  = sc_sels
classSCSelIds c = assert (null (classMethods c) ) []

classSCSelId :: Class -> Int -> Id
-- Get the n'th superclass selector Id
-- where n is 0-indexed, and counts
--    *all* superclasses including equalities
classSCSelId (Class { classBody = ConcreteClass { cls_sc_sel_ids = sc_sels } }) n
  = assert (n >= 0 && lengthExceeds sc_sels n )
    sc_sels !! n
classSCSelId c n = pprPanic "classSCSelId" (ppr c <+> ppr n)

classMethods :: Class -> [Id]
classMethods (Class { classBody = ConcreteClass { cls_ops = op_stuff } })
  = [op_sel | (op_sel, _) <- op_stuff]
classMethods _ = []

classOpItems :: Class -> [ClassOpItem]
classOpItems (Class { classBody = ConcreteClass { cls_ops = op_stuff }})
  = op_stuff
classOpItems _ = []

classATs :: Class -> [TyCon]
classATs (Class { classBody = ConcreteClass { cls_ats = at_stuff } })
  = [tc | ATI tc _ <- at_stuff]
classATs _ = []

classATItems :: Class -> [ClassATItem]
classATItems (Class { classBody = ConcreteClass { cls_ats = at_stuff }})
  = at_stuff
classATItems _ = []

classSCTheta :: Class -> [PredType]
classSCTheta (Class { classBody = ConcreteClass { cls_sc_theta = theta_stuff }})
  = theta_stuff
classSCTheta _ = []

classHasSCs :: Class -> Bool
classHasSCs cls = not (null (classSCTheta cls))

classTvsFds :: Class -> ([TyVar], [FunDep TyVar])
classTvsFds c = (classTyVars c, classFunDeps c)

classHasFds :: Class -> Bool
classHasFds (Class { classFunDeps = fds }) = not (null fds)

classBigSig :: Class -> ([TyVar], [PredType], [Id], [ClassOpItem])
classBigSig (Class {classTyVars = tyvars,
                    classBody = AbstractClass})
  = (tyvars, [], [], [])
classBigSig (Class {classTyVars = tyvars,
                    classBody = ConcreteClass {
                        cls_sc_theta = sc_theta,
                        cls_sc_sel_ids = sc_sels,
                        cls_ops  = op_stuff
                    }})
  = (tyvars, sc_theta, sc_sels, op_stuff)

classExtraBigSig :: Class -> ([TyVar], [FunDep TyVar], [PredType], [Id], [ClassATItem], [ClassOpItem])
classExtraBigSig (Class {classTyVars = tyvars, classFunDeps = fundeps,
                         classBody = AbstractClass})
  = (tyvars, fundeps, [], [], [], [])
classExtraBigSig (Class {classTyVars = tyvars, classFunDeps = fundeps,
                         classBody = ConcreteClass {
                             cls_sc_theta = sc_theta, cls_sc_sel_ids = sc_sels,
                             cls_ats = ats, cls_ops = op_stuff
                         }})
  = (tyvars, fundeps, sc_theta, sc_sels, ats, op_stuff)

isAbstractClass :: Class -> Bool
isAbstractClass Class{ classBody = AbstractClass } = True
isAbstractClass _ = False

{-
************************************************************************
*                                                                      *
\subsection[Class-instances]{Instance declarations for @Class@}
*                                                                      *
************************************************************************

We compare @Classes@ by their keys (which include @Uniques@).
-}

instance Eq Class where
    c1 == c2 = classKey c1 == classKey c2
    c1 /= c2 = classKey c1 /= classKey c2

instance Uniquable Class where
    getUnique c = classKey c

instance NamedThing Class where
    getName clas = className clas

instance Outputable Class where
    ppr c = ppr (getName c)

pprDefMethInfo :: DefMethInfo -> SDoc
pprDefMethInfo Nothing                  = empty   -- No default method
pprDefMethInfo (Just (n, VanillaDM))    = text "Default method" <+> ppr n
pprDefMethInfo (Just (n, GenericDM ty)) = text "Generic default method"
                                          <+> ppr n <+> dcolon <+> pprType ty

pprFundeps :: Outputable a => [FunDep a] -> SDoc
pprFundeps []  = empty
pprFundeps fds = hsep (vbar : punctuate comma (map pprFunDep fds))

pprFunDep :: Outputable a => FunDep a -> SDoc
pprFunDep (us, vs) = hsep [interppSP us, arrow, interppSP vs]

instance Data.Data Class where
    -- don't traverse?
    toConstr _   = abstractConstr "Class"
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = mkNoRepType "Class"
