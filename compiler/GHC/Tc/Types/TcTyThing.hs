module GHC.Tc.Types.TcTyThing where

import GHC.Prelude

import GHC.Tc.Errors.Types.PromotionErr (PromotionErr, peCategory)

import GHC.Core.Type
import GHC.Core.TyCon  ( TyCon, tyConKind )

import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Var
import GHC.Types.TyThing

import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Misc

---------------------------
-- TcTyThing
---------------------------

-- | A typecheckable thing available in a local context.  Could be
-- 'AGlobal' 'TyThing', but also lexically scoped variables, etc.
-- See "GHC.Tc.Utils.Env" for how to retrieve a 'TyThing' given a 'Name'.
data TcTyThing
  = AGlobal TyThing             -- Used only in the return type of a lookup

  | ATcId           -- Ids defined in this module; may not be fully zonked
      { tct_id   :: Id
      , tct_info :: IdBindingInfo   -- See Note [Meaning of IdBindingInfo]
      }

  | ATyVar  Name TcTyVar   -- See Note [Type variables in the type environment]

  | ATcTyCon TyCon   -- Used temporarily, during kind checking, for the
                     -- tycons and classes in this recursive group
                     -- The TyCon is always a TcTyCon.  Its kind
                     -- can be a mono-kind or a poly-kind; in TcTyClsDcls see
                     -- Note [Type checking recursive type and class declarations]

  | APromotionErr PromotionErr

-- | Matches on either a global 'TyCon' or a 'TcTyCon'.
tcTyThingTyCon_maybe :: TcTyThing -> Maybe TyCon
tcTyThingTyCon_maybe (AGlobal (ATyCon tc)) = Just tc
tcTyThingTyCon_maybe (ATcTyCon tc_tc)      = Just tc_tc
tcTyThingTyCon_maybe _                     = Nothing

instance Outputable TcTyThing where     -- Debugging only
   ppr (AGlobal g)      = ppr g
   ppr elt@(ATcId {})   = text "Identifier" <>
                          brackets (ppr (tct_id elt) <> dcolon
                                 <> ppr (varType (tct_id elt)) <> comma
                                 <+> ppr (tct_info elt))
   ppr (ATyVar n tv)    = text "Type variable" <+> quotes (ppr n) <+> equals <+> ppr tv
                            <+> dcolon <+> ppr (varType tv)
   ppr (ATcTyCon tc)    = text "ATcTyCon" <+> ppr tc <+> dcolon <+> ppr (tyConKind tc)
   ppr (APromotionErr err) = text "APromotionErr" <+> ppr err

-- | IdBindingInfo describes how an Id is bound.
--
-- It is used for the following purposes:
-- a) for static forms in 'GHC.Tc.Gen.Expr.checkClosedInStaticForm' and
-- b) to figure out when a nested binding can be generalised,
--    in 'GHC.Tc.Gen.Bind.decideGeneralisationPlan'.
--
data IdBindingInfo -- See Note [Meaning of IdBindingInfo]
    = NotLetBound
    | ClosedLet
    | NonClosedLet
         RhsNames        -- Used for (static e) checks only
         ClosedTypeId    -- Used for generalisation checks
                         -- and for (static e) checks

-- | IsGroupClosed describes a group of mutually-recursive bindings
data IsGroupClosed
  = IsGroupClosed
      (NameEnv RhsNames)  -- Free var info for the RHS of each binding in the group
                          -- Used only for (static e) checks

      ClosedTypeId        -- True <=> all the free vars of the group are
                          --          imported or ClosedLet or
                          --          NonClosedLet with ClosedTypeId=True.
                          --          In particular, no tyvars, no NotLetBound

type RhsNames = NameSet   -- Names of variables, mentioned on the RHS of
                          -- a definition, that are not Global or ClosedLet

type ClosedTypeId = Bool
  -- See Note [Meaning of IdBindingInfo]

{- Note [Meaning of IdBindingInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NotLetBound means that
  the Id is not let-bound (e.g. it is bound in a
  lambda-abstraction or in a case pattern)

ClosedLet means that
   - The Id is let-bound,
   - Any free term variables are also Global or ClosedLet
   - Its type has no free variables (NB: a top-level binding subject
     to the MR might have free vars in its type)
   These ClosedLets can definitely be floated to top level; and we
   may need to do so for static forms.

   Property:   ClosedLet
             is equivalent to
               NonClosedLet emptyNameSet True

(NonClosedLet (fvs::RhsNames) (cl::ClosedTypeId)) means that
   - The Id is let-bound

   - The fvs::RhsNames contains the free names of the RHS,
     excluding Global and ClosedLet ones.

   - For the ClosedTypeId field see Note [Bindings with closed types: ClosedTypeId]

For (static e) to be valid, we need for every 'x' free in 'e',
that x's binding is floatable to the top level.  Specifically:
   * x's RhsNames must be empty
   * x's type has no free variables
See Note [Grand plan for static forms] in "GHC.Iface.Tidy.StaticPtrTable".
This test is made in GHC.Tc.Gen.Expr.checkClosedInStaticForm.
Actually knowing x's RhsNames (rather than just its emptiness
or otherwise) is just so we can produce better error messages

Note [Bindings with closed types: ClosedTypeId]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  f x = let g ys = map not ys
        in ...

Can we generalise 'g' under the OutsideIn algorithm?  Yes,
because all g's free variables are top-level; that is they themselves
have no free type variables, and it is the type variables in the
environment that makes things tricky for OutsideIn generalisation.

Here's the invariant:
   If an Id has ClosedTypeId=True (in its IdBindingInfo), then
   the Id's type is /definitely/ closed (has no free type variables).
   Specifically,
       a) The Id's actual type is closed (has no free tyvars)
       b) Either the Id has a (closed) user-supplied type signature
          or all its free variables are Global/ClosedLet
             or NonClosedLet with ClosedTypeId=True.
          In particular, none are NotLetBound.

Why is (b) needed?   Consider
    \x. (x :: Int, let y = x+1 in ...)
Initially x::alpha.  If we happen to typecheck the 'let' before the
(x::Int), y's type will have a free tyvar; but if the other way round
it won't.  So we treat any let-bound variable with a free
non-let-bound variable as not ClosedTypeId, regardless of what the
free vars of its type actually are.

But if it has a signature, all is well:
   \x. ...(let { y::Int; y = x+1 } in
           let { v = y+2 } in ...)...
Here the signature on 'v' makes 'y' a ClosedTypeId, so we can
generalise 'v'.

Note that:

  * A top-level binding may not have ClosedTypeId=True, if it suffers
    from the MR

  * A nested binding may be closed (eg 'g' in the example we started
    with). Indeed, that's the point; whether a function is defined at
    top level or nested is orthogonal to the question of whether or
    not it is closed.

  * A binding may be non-closed because it mentions a lexically scoped
    *type variable*  Eg
        f :: forall a. blah
        f x = let g y = ...(y::a)...

Under OutsideIn we are free to generalise an Id all of whose free
variables have ClosedTypeId=True (or imported).  This is an extension
compared to the JFP paper on OutsideIn, which used "top-level" as a
proxy for "closed".  (It's not a good proxy anyway -- the MR can make
a top-level binding with a free type variable.)

Note [Type variables in the type environment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type environment has a binding for each lexically-scoped
type variable that is in scope.  For example

  f :: forall a. a -> a
  f x = (x :: a)

  g1 :: [a] -> a
  g1 (ys :: [b]) = head ys :: b

  g2 :: [Int] -> Int
  g2 (ys :: [c]) = head ys :: c

* The forall'd variable 'a' in the signature scopes over f's RHS.

* The pattern-bound type variable 'b' in 'g1' scopes over g1's
  RHS; note that it is bound to a skolem 'a' which is not itself
  lexically in scope.

* The pattern-bound type variable 'c' in 'g2' is bound to
  Int; that is, pattern-bound type variables can stand for
  arbitrary types. (see
    GHC proposal #128 "Allow ScopedTypeVariables to refer to types"
    https://github.com/ghc-proposals/ghc-proposals/pull/128,
  and the paper
    "Type variables in patterns", Haskell Symposium 2018.


This is implemented by the constructor
   ATyVar Name TcTyVar
in the type environment.

* The Name is the name of the original, lexically scoped type
  variable

* The TcTyVar is sometimes a skolem (like in 'f'), and sometimes
  a unification variable (like in 'g1', 'g2').  We never zonk the
  type environment so in the latter case it always stays as a
  unification variable, although that variable may be later
  unified with a type (such as Int in 'g2').
-}

instance Outputable IdBindingInfo where
  ppr NotLetBound = text "NotLetBound"
  ppr ClosedLet = text "TopLevelLet"
  ppr (NonClosedLet fvs closed_type) =
    text "TopLevelLet" <+> ppr fvs <+> ppr closed_type

--------------
pprTcTyThingCategory :: TcTyThing -> SDoc
pprTcTyThingCategory = text . capitalise . tcTyThingCategory

tcTyThingCategory :: TcTyThing -> String
tcTyThingCategory (AGlobal thing)    = tyThingCategory thing
tcTyThingCategory (ATyVar {})        = "type variable"
tcTyThingCategory (ATcId {})         = "local identifier"
tcTyThingCategory (ATcTyCon {})      = "local tycon"
tcTyThingCategory (APromotionErr pe) = peCategory pe
