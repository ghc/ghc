{-# LANGUAGE LambdaCase #-}

module GHC.Builtin.Types.Literals
  ( typeNatTyCons
  , typeNatCoAxiomRules
  , BuiltInSynFamily(..)

    -- If you define a new built-in type family, make sure to export its TyCon
    -- from here as well.
    -- See Note [Adding built-in type families]
  , typeNatAddTyCon
  , typeNatMulTyCon
  , typeNatExpTyCon
  , typeNatLeqTyCon
  , typeNatSubTyCon
  , typeNatDivTyCon
  , typeNatModTyCon
  , typeNatLogTyCon
  , typeNatCmpTyCon
  , typeSymbolCmpTyCon
  , typeSymbolAppendTyCon
  , typeCharCmpTyCon
  , typeLeqCharTyCon
  , typeConsSymbolTyCon
  , typeUnconsSymbolTyCon
  , typeToUpperTyCon
  , typeToLowerTyCon
  , typeToTitleTyCon
  , typeCharToNatTyCon
  , typeNatToCharTyCon
  , typeIsControlTyCon
  , typeIsSpaceTyCon
  , typeIsLowerTyCon
  , typeIsUpperTyCon
  , typeIsAlphaTyCon
  , typeIsAlphaNumTyCon
  , typeIsPrintTyCon
  , typeIsDigitTyCon
  , typeIsOctDigitTyCon
  , typeIsHexDigitTyCon
  , typeIsLetterTyCon
  , typeGeneralCharCategoryTyCon
  ) where

import GHC.Prelude

import GHC.Core.Type
import GHC.Data.Pair
import GHC.Tc.Utils.TcType ( TcType, tcEqType )
import GHC.Core.TyCon    ( TyCon, FamTyConFlav(..), mkFamilyTyCon
                         , Injectivity(..) )
import GHC.Core.Coercion ( Role(..) )
import GHC.Tc.Types.Constraint ( Xi )
import GHC.Core.Coercion.Axiom ( CoAxiomRule(..), BuiltInSynFamily(..), TypeEqn )
import GHC.Types.Name          ( Name, BuiltInSyntax(..) )
import GHC.Unicode             ( GeneralCategory (..), generalCategory )
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim  ( mkTemplateAnonTyConBinders )
import GHC.Builtin.Names
                  ( gHC_TYPELITS
                  , gHC_TYPENATS
                  , typeNatAddTyFamNameKey
                  , typeNatMulTyFamNameKey
                  , typeNatExpTyFamNameKey
                  , typeNatLeqTyFamNameKey
                  , typeNatSubTyFamNameKey
                  , typeNatDivTyFamNameKey
                  , typeNatModTyFamNameKey
                  , typeNatLogTyFamNameKey
                  , typeNatCmpTyFamNameKey
                  , typeSymbolCmpTyFamNameKey
                  , typeSymbolAppendFamNameKey
                  , typeCharCmpTyFamNameKey
                  , typeLeqCharTyFamNameKey
                  , typeConsSymbolTyFamNameKey
                  , typeUnconsSymbolTyFamNameKey
                  , typeToUpperTyFamNameKey
                  , typeToLowerTyFamNameKey
                  , typeToTitleTyFamNameKey
                  , typeNatToCharTyFamNameKey
                  , typeCharToNatTyFamNameKey
                  , typeIsControlTyFamNameKey
                  , typeIsSpaceTyFamNameKey
                  , typeIsLowerTyFamNameKey
                  , typeIsUpperTyFamNameKey
                  , typeIsAlphaTyFamNameKey
                  , typeIsAlphaNumTyFamNameKey
                  , typeIsPrintTyFamNameKey
                  , typeIsDigitTyFamNameKey
                  , typeIsOctDigitTyFamNameKey
                  , typeIsHexDigitTyFamNameKey
                  , typeIsLetterTyFamNameKey
                  , typeGeneralCharCategoryTyFamKey
                  )
import GHC.Data.FastString
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Maybe ( isJust )
import Control.Monad (guard, join )
import Data.List  ( isPrefixOf, isSuffixOf )
import GHC.Types.Basic (Boxity(..))

{-
Note [Type-level literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~
There are currently two forms of type-level literals: natural numbers, symbols, and
chars (even though this module is named GHC.Builtin.Types.Literals, it covers both).

Type-level literals are supported by CoAxiomRules (conditional axioms), which
power the built-in type families (see Note [Adding built-in type families]).
Currently, all built-in type families are for the express purpose of supporting
type-level literals.

See also the Wiki page:

    https://gitlab.haskell.org/ghc/ghc/wikis/type-nats

Note [Adding built-in type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are a few steps to adding a built-in type family:

* Adding a unique for the type family TyCon

  These go in GHC.Builtin.Names. It will likely be of the form
  @myTyFamNameKey = mkPreludeTyConUnique xyz@, where @xyz@ is a number that
  has not been chosen before in GHC.Builtin.Names. There are several examples already
  in GHC.Builtin.Names—see, for instance, typeNatAddTyFamNameKey.

* Adding the type family TyCon itself

  This goes in GHC.Builtin.Types.Literals. There are plenty of examples of how to define
  these—see, for instance, typeNatAddTyCon.

  Once your TyCon has been defined, be sure to:

  - Export it from GHC.Builtin.Types.Literals. (Not doing so caused #14632.)
  - Include it in the typeNatTyCons list, defined in GHC.Builtin.Types.Literals.

* Exposing associated type family axioms

  When defining the type family TyCon, you will need to define an axiom for
  the type family in general (see, for instance, axAddDef), and perhaps other
  auxiliary axioms for special cases of the type family (see, for instance,
  axAdd0L and axAdd0R).

  After you have defined all of these axioms, be sure to include them in the
  typeNatCoAxiomRules list, defined in GHC.Builtin.Types.Literals.
  (Not doing so caused #14934.)

* Define the type family somewhere

  Finally, you will need to define the type family somewhere, likely in @base@.
  Currently, all of the built-in type families are defined in GHC.TypeLits or
  GHC.TypeNats, so those are likely candidates.

  Since the behavior of your built-in type family is specified in GHC.Builtin.Types.Literals,
  you should give an open type family definition with no instances, like so:

    type family MyTypeFam (m :: Nat) (n :: Nat) :: Nat

  Changing the argument and result kinds as appropriate.

* Update the relevant test cases

  The GHC test suite will likely need to be updated after you add your built-in
  type family. For instance:

  - The T9181 test prints the :browse contents of GHC.TypeLits, so if you added
    a test there, the expected output of T9181 will need to change.
  - The TcTypeNatSimple and TcTypeSymbolSimple tests have compile-time unit
    tests, as well as TcTypeNatSimpleRun and TcTypeSymbolSimpleRun, which have
    runtime unit tests. Consider adding further unit tests to those if your
    built-in type family deals with Nats or Symbols, respectively.
-}

{-------------------------------------------------------------------------------
Built-in type constructors for functions on type-level nats
-}

-- The list of built-in type family TyCons that GHC uses.
-- If you define a built-in type family, make sure to add it to this list.
-- See Note [Adding built-in type families]
typeNatTyCons :: [TyCon]
typeNatTyCons =
  [ typeNatAddTyCon
  , typeNatMulTyCon
  , typeNatExpTyCon
  , typeNatLeqTyCon
  , typeNatSubTyCon
  , typeNatDivTyCon
  , typeNatModTyCon
  , typeNatLogTyCon
  , typeNatCmpTyCon
  , typeSymbolCmpTyCon
  , typeSymbolAppendTyCon
  , typeCharCmpTyCon
  , typeLeqCharTyCon
  , typeConsSymbolTyCon
  , typeUnconsSymbolTyCon
  , typeToUpperTyCon
  , typeToLowerTyCon
  , typeToTitleTyCon
  , typeCharToNatTyCon
  , typeNatToCharTyCon
  , typeIsControlTyCon
  , typeIsSpaceTyCon
  , typeIsLowerTyCon
  , typeIsUpperTyCon
  , typeIsAlphaTyCon
  , typeIsAlphaNumTyCon
  , typeIsPrintTyCon
  , typeIsDigitTyCon
  , typeIsOctDigitTyCon
  , typeIsHexDigitTyCon
  , typeIsLetterTyCon
  , typeGeneralCharCategoryTyCon
  ]

typeNatAddTyCon :: TyCon
typeNatAddTyCon = mkTypeNatFunTyCon2 name
  BuiltInSynFamily
    { sfMatchFam      = matchFamAdd
    , sfInteractTop   = interactTopAdd
    , sfInteractInert = interactInertAdd
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPENATS (fsLit "+")
            typeNatAddTyFamNameKey typeNatAddTyCon

typeNatSubTyCon :: TyCon
typeNatSubTyCon = mkTypeNatFunTyCon2 name
  BuiltInSynFamily
    { sfMatchFam      = matchFamSub
    , sfInteractTop   = interactTopSub
    , sfInteractInert = interactInertSub
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPENATS (fsLit "-")
            typeNatSubTyFamNameKey typeNatSubTyCon

typeNatMulTyCon :: TyCon
typeNatMulTyCon = mkTypeNatFunTyCon2 name
  BuiltInSynFamily
    { sfMatchFam      = matchFamMul
    , sfInteractTop   = interactTopMul
    , sfInteractInert = interactInertMul
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPENATS (fsLit "*")
            typeNatMulTyFamNameKey typeNatMulTyCon

typeNatDivTyCon :: TyCon
typeNatDivTyCon = mkTypeNatFunTyCon2 name
  BuiltInSynFamily
    { sfMatchFam      = matchFamDiv
    , sfInteractTop   = interactTopDiv
    , sfInteractInert = interactInertDiv
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPENATS (fsLit "Div")
            typeNatDivTyFamNameKey typeNatDivTyCon

typeNatModTyCon :: TyCon
typeNatModTyCon = mkTypeNatFunTyCon2 name
  BuiltInSynFamily
    { sfMatchFam      = matchFamMod
    , sfInteractTop   = interactTopMod
    , sfInteractInert = interactInertMod
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPENATS (fsLit "Mod")
            typeNatModTyFamNameKey typeNatModTyCon

typeNatExpTyCon :: TyCon
typeNatExpTyCon = mkTypeNatFunTyCon2 name
  BuiltInSynFamily
    { sfMatchFam      = matchFamExp
    , sfInteractTop   = interactTopExp
    , sfInteractInert = interactInertExp
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPENATS (fsLit "^")
                typeNatExpTyFamNameKey typeNatExpTyCon

typeNatLogTyCon :: TyCon
typeNatLogTyCon = mkTypeNatFunTyCon1 name
  BuiltInSynFamily
    { sfMatchFam      = matchFamLog
    , sfInteractTop   = interactTopLog
    , sfInteractInert = interactInertLog
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPENATS (fsLit "Log2")
            typeNatLogTyFamNameKey typeNatLogTyCon

typeNatLeqTyCon :: TyCon
typeNatLeqTyCon =
  mkFamilyTyCon name
    (mkTemplateAnonTyConBinders [ typeNatKind, typeNatKind ])
    boolTy
    Nothing
    (BuiltInSynFamTyCon ops)
    Nothing
    NotInjective

  where
  name = mkWiredInTyConName UserSyntax gHC_TYPENATS (fsLit "<=?")
                typeNatLeqTyFamNameKey typeNatLeqTyCon
  ops = BuiltInSynFamily
    { sfMatchFam      = matchFamLeq
    , sfInteractTop   = interactTopLeq
    , sfInteractInert = interactInertLeq
    }

typeNatCmpTyCon :: TyCon
typeNatCmpTyCon =
  mkFamilyTyCon name
    (mkTemplateAnonTyConBinders [ typeNatKind, typeNatKind ])
    orderingKind
    Nothing
    (BuiltInSynFamTyCon ops)
    Nothing
    NotInjective

  where
  name = mkWiredInTyConName UserSyntax gHC_TYPENATS (fsLit "CmpNat")
                typeNatCmpTyFamNameKey typeNatCmpTyCon
  ops = BuiltInSynFamily
    { sfMatchFam      = matchFamCmpNat
    , sfInteractTop   = interactTopCmpNat
    , sfInteractInert = \_ _ _ _ -> []
    }

typeSymbolCmpTyCon :: TyCon
typeSymbolCmpTyCon =
  mkFamilyTyCon name
    (mkTemplateAnonTyConBinders [ typeSymbolKind, typeSymbolKind ])
    orderingKind
    Nothing
    (BuiltInSynFamTyCon ops)
    Nothing
    NotInjective

  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "CmpSymbol")
                typeSymbolCmpTyFamNameKey typeSymbolCmpTyCon
  ops = BuiltInSynFamily
    { sfMatchFam      = matchFamCmpSymbol
    , sfInteractTop   = interactTopCmpSymbol
    , sfInteractInert = \_ _ _ _ -> []
    }

typeSymbolAppendTyCon :: TyCon
typeSymbolAppendTyCon = mkTypeSymbolFunTyCon2 name
  BuiltInSynFamily
    { sfMatchFam      = matchFamAppendSymbol
    , sfInteractTop   = interactTopAppendSymbol
    , sfInteractInert = interactInertAppendSymbol
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "AppendSymbol")
                typeSymbolAppendFamNameKey typeSymbolAppendTyCon

typeConsSymbolTyCon :: TyCon
typeConsSymbolTyCon =
  mkFamilyTyCon name
    (mkTemplateAnonTyConBinders [ charTy, typeSymbolKind ])
    typeSymbolKind
    Nothing
    (BuiltInSynFamTyCon ops)
    Nothing
    NotInjective
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "ConsSymbol")
                  typeConsSymbolTyFamNameKey typeConsSymbolTyCon
  ops = BuiltInSynFamily
      { sfMatchFam      = matchFamConsSymbol
      , sfInteractTop   = interactTopConsSymbol
      , sfInteractInert = interactInertConsSymbol
      }

typeUnconsSymbolTyCon :: TyCon
typeUnconsSymbolTyCon =
  mkFamilyTyCon name
    (mkTemplateAnonTyConBinders [ typeSymbolKind ])
    (maybeKind charSymbolPairKind)
    Nothing
    (BuiltInSynFamTyCon ops)
    Nothing
    NotInjective
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "UnconsSymbol")
                  typeUnconsSymbolTyFamNameKey typeUnconsSymbolTyCon
  ops = BuiltInSynFamily
      { sfMatchFam      = matchFamUnconsSymbol
      , sfInteractTop   = interactTopUnconsSymbol
      , sfInteractInert = interactInertUnconsSymbol
      }

typeToUpperTyCon :: TyCon
typeToUpperTyCon =
  mkFamilyTyCon name
    (mkTemplateAnonTyConBinders [ charTy ])
    charTy
    Nothing
    (BuiltInSynFamTyCon ops)
    Nothing
    NotInjective
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "ToUpper")
                  typeToUpperTyFamNameKey typeToUpperTyCon
  ops = BuiltInSynFamily
      { sfMatchFam      = matchFamToUpper
      , sfInteractTop   = \_ _ -> []
      , sfInteractInert = \_ _ _ _ -> []
      }

typeToLowerTyCon :: TyCon
typeToLowerTyCon =
  mkFamilyTyCon name
    (mkTemplateAnonTyConBinders [ charTy ])
    charTy
    Nothing
    (BuiltInSynFamTyCon ops)
    Nothing
    NotInjective
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "ToLower")
                  typeToLowerTyFamNameKey typeToLowerTyCon
  ops = BuiltInSynFamily
      { sfMatchFam      = matchFamToLower
      , sfInteractTop   = \_ _ -> []
      , sfInteractInert = \_ _ _ _ -> []
      }

typeToTitleTyCon :: TyCon
typeToTitleTyCon =
  mkFamilyTyCon name
    (mkTemplateAnonTyConBinders [ charTy ])
    charTy
    Nothing
    (BuiltInSynFamTyCon ops)
    Nothing
    NotInjective
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "ToTitle")
                  typeToTitleTyFamNameKey typeToTitleTyCon
  ops = BuiltInSynFamily
      { sfMatchFam      = matchFamToTitle
      , sfInteractTop   = \_ _ -> []
      , sfInteractInert = \_ _ _ _ -> []
      }

typeNatToCharTyCon :: TyCon
typeNatToCharTyCon =
  mkFamilyTyCon name
    (mkTemplateAnonTyConBinders [ typeNatKind ])
    charTy
    Nothing
    (BuiltInSynFamTyCon ops)
    Nothing
    NotInjective
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "NatToChar")
                  typeNatToCharTyFamNameKey typeNatToCharTyCon
  ops = BuiltInSynFamily
      { sfMatchFam      = matchFamNatToChar
      , sfInteractTop   = \_ _ -> []
      , sfInteractInert = \_ _ _ _ -> []
      }

typeCharToNatTyCon :: TyCon
typeCharToNatTyCon =
  mkFamilyTyCon name
    (mkTemplateAnonTyConBinders [ charTy ])
    typeNatKind
    Nothing
    (BuiltInSynFamTyCon ops)
    Nothing
    NotInjective
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "CharToNat")
                  typeCharToNatTyFamNameKey typeCharToNatTyCon
  ops = BuiltInSynFamily
      { sfMatchFam      = matchFamCharToNat
      , sfInteractTop   = \_ _ -> []
      , sfInteractInert = \_ _ _ _ -> []
      }

-- Make a unary built-in constructor of kind: Nat -> Nat
mkTypeNatFunTyCon1 :: Name -> BuiltInSynFamily -> TyCon
mkTypeNatFunTyCon1 op tcb =
  mkFamilyTyCon op
    (mkTemplateAnonTyConBinders [ typeNatKind ])
    typeNatKind
    Nothing
    (BuiltInSynFamTyCon tcb)
    Nothing
    NotInjective

-- Make a binary built-in constructor of kind: Nat -> Nat -> Nat
mkTypeNatFunTyCon2 :: Name -> BuiltInSynFamily -> TyCon
mkTypeNatFunTyCon2 op tcb =
  mkFamilyTyCon op
    (mkTemplateAnonTyConBinders [ typeNatKind, typeNatKind ])
    typeNatKind
    Nothing
    (BuiltInSynFamTyCon tcb)
    Nothing
    NotInjective

-- Make a binary built-in constructor of kind: Symbol -> Symbol -> Symbol
mkTypeSymbolFunTyCon2 :: Name -> BuiltInSynFamily -> TyCon
mkTypeSymbolFunTyCon2 op tcb =
  mkFamilyTyCon op
    (mkTemplateAnonTyConBinders [ typeSymbolKind, typeSymbolKind ])
    typeSymbolKind
    Nothing
    (BuiltInSynFamTyCon tcb)
    Nothing
    NotInjective

-- Make a unary built-in constructor of kind: Char -> Bool
mkTypeCharPredicateTyCon :: Name -> BuiltInSynFamily -> TyCon
mkTypeCharPredicateTyCon op tcb =
  mkFamilyTyCon op
    (mkTemplateAnonTyConBinders [ charTy ])
    boolTy
    Nothing
    (BuiltInSynFamTyCon tcb)
    Nothing
    NotInjective

    -- Make a unary built-in constructor of kind: Char -> GeneralCategory
mkTypeCharCategoryTyCon :: Name -> BuiltInSynFamily -> TyCon
mkTypeCharCategoryTyCon op tcb =
  mkFamilyTyCon op
    (mkTemplateAnonTyConBinders [ charTy ])
    generalCategoryTy
    Nothing
    (BuiltInSynFamTyCon tcb)
    Nothing
    NotInjective


{-------------------------------------------------------------------------------
Built-in rules axioms
-------------------------------------------------------------------------------}

-- If you add additional rules, please remember to add them to
-- `typeNatCoAxiomRules` also.
-- See Note [Adding built-in type families]
axAddDef
  , axMulDef
  , axExpDef
  , axLeqDef
  , axCmpNatDef
  , axCmpSymbolDef
  , axAppendSymbolDef
  , axConsSymbolDef
  , axConsSymbol0
  , axUnconsSymbol0
  , axUnconsSymbolDef
  , axToUpperDef
  , axToLowerDef
  , axToTitleDef
  , axNatToCharDef
  , axCharToNatDef
  , axAdd0L
  , axAdd0R
  , axMul0L
  , axMul0R
  , axMul1L
  , axMul1R
  , axExp1L
  , axExp0R
  , axExp1R
  , axLeqRefl
  , axCmpNatRefl
  , axCmpSymbolRefl
  , axLeq0L
  , axSubDef
  , axSub0R
  , axAppendSymbol0R
  , axAppendSymbol0L
  , axDivDef
  , axDiv1
  , axModDef
  , axMod1
  , axLogDef
  :: CoAxiomRule

axAddDef = mkBinAxiom "AddDef" typeNatAddTyCon $
              \x y -> Just $ num (x + y)

axMulDef = mkBinAxiom "MulDef" typeNatMulTyCon $
              \x y -> Just $ num (x * y)

axExpDef = mkBinAxiom "ExpDef" typeNatExpTyCon $
              \x y -> Just $ num (x ^ y)

axLeqDef = mkBinAxiom "LeqDef" typeNatLeqTyCon $
              \x y -> Just $ bool (x <= y)

axCmpNatDef   = mkBinAxiom "CmpNatDef" typeNatCmpTyCon
              $ \x y -> Just $ ordering (compare x y)

axCmpSymbolDef =
  CoAxiomRule
    { coaxrName      = fsLit "CmpSymbolDef"
    , coaxrAsmpRoles = [Nominal, Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2, Pair t1 t2] <- return cs
           s2' <- isStrLitTy s2
           t2' <- isStrLitTy t2
           return (mkTyConApp typeSymbolCmpTyCon [s1,t1] ===
                   ordering (compare s2' t2')) }

axAppendSymbolDef = CoAxiomRule
    { coaxrName      = fsLit "AppendSymbolDef"
    , coaxrAsmpRoles = [Nominal, Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2, Pair t1 t2] <- return cs
           s2' <- isStrLitTy s2
           t2' <- isStrLitTy t2
           let z = mkStrLitTy (appendFS s2' t2')
           return (mkTyConApp typeSymbolAppendTyCon [s1, t1] === z)
    }

axConsSymbolDef = CoAxiomRule
    { coaxrName      = fsLit "ConsSymbolDef"
    , coaxrAsmpRoles = [Nominal, Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2, Pair t1 t2] <- return cs
           s2' <- isCharLitTy s2
           t2' <- isStrLitTy t2
           let z = mkStrLitTy (consFS s2' t2')
           return (mkTyConApp typeConsSymbolTyCon [s1, t1] === z)
    }

axUnconsSymbolDef =
  CoAxiomRule
    { coaxrName      = fsLit "UnconsSymbolDef"
    , coaxrAsmpRoles = [Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2] <- return cs
           s2' <- isStrLitTy s2
           return (mkTyConApp typeUnconsSymbolTyCon [s1] ===
                     (maybeType charSymbolPairKind $
                        Just $ charSymbolPair (mkCharLitTy $ headFS s2') (mkStrLitTy $ tailFS s2')))
    }

axToUpperDef =
  CoAxiomRule
    { coaxrName      = fsLit "ToUpperDef"
    , coaxrAsmpRoles = [Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2] <- return cs
           s2' <- isCharLitTy s2
           return (mkTyConApp typeToUpperTyCon [s1] === mkCharLitTy (Char.toUpper s2'))
    }

axToLowerDef =
  CoAxiomRule
    { coaxrName      = fsLit "ToLowerDef"
    , coaxrAsmpRoles = [Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2] <- return cs
           s2' <- isCharLitTy s2
           return (mkTyConApp typeToLowerTyCon [s1] === mkCharLitTy (Char.toLower s2'))
    }

axToTitleDef =
  CoAxiomRule
    { coaxrName      = fsLit "ToTitleDef"
    , coaxrAsmpRoles = [Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2] <- return cs
           s2' <- isCharLitTy s2
           return (mkTyConApp typeToTitleTyCon [s1] === mkCharLitTy (Char.toTitle s2'))
    }

axNatToCharDef =
  CoAxiomRule
    { coaxrName      = fsLit "NatToCharDef"
    , coaxrAsmpRoles = [Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2] <- return cs
           s2' <- isNumLitTy s2
           guard (s2' < 1114112)
           return (mkTyConApp typeNatToCharTyCon [s1] === mkCharLitTy (Char.chr $ fromIntegral s2'))
    }

axCharToNatDef =
  CoAxiomRule
    { coaxrName      = fsLit "CharToNat"
    , coaxrAsmpRoles = [Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2] <- return cs
           s2' <- isCharLitTy s2
           return (mkTyConApp typeCharToNatTyCon [s1] === mkNumLitTy (fromIntegral $ Char.ord s2'))
    }

axSubDef = mkBinAxiom "SubDef" typeNatSubTyCon $
              \x y -> fmap num (minus x y)

axDivDef = mkBinAxiom "DivDef" typeNatDivTyCon $
              \x y -> do guard (y /= 0)
                         return (num (div x y))

axModDef = mkBinAxiom "ModDef" typeNatModTyCon $
              \x y -> do guard (y /= 0)
                         return (num (mod x y))

axLogDef = mkUnAxiom "LogDef" typeNatLogTyCon $
              \x -> do (a,_) <- genLog x 2
                       return (num a)

axAdd0L     = mkAxiom1 "Add0L"    $ \(Pair s t) -> (num 0 .+. s) === t
axAdd0R     = mkAxiom1 "Add0R"    $ \(Pair s t) -> (s .+. num 0) === t
axSub0R     = mkAxiom1 "Sub0R"    $ \(Pair s t) -> (s .-. num 0) === t
axMul0L     = mkAxiom1 "Mul0L"    $ \(Pair s _) -> (num 0 .*. s) === num 0
axMul0R     = mkAxiom1 "Mul0R"    $ \(Pair s _) -> (s .*. num 0) === num 0
axMul1L     = mkAxiom1 "Mul1L"    $ \(Pair s t) -> (num 1 .*. s) === t
axMul1R     = mkAxiom1 "Mul1R"    $ \(Pair s t) -> (s .*. num 1) === t
axDiv1      = mkAxiom1 "Div1"     $ \(Pair s t) -> (tDiv s (num 1) === t)
axMod1      = mkAxiom1 "Mod1"     $ \(Pair s _) -> (tMod s (num 1) === num 0)
                                    -- XXX: Shouldn't we check that _ is 0?
axExp1L     = mkAxiom1 "Exp1L"    $ \(Pair s _) -> (num 1 .^. s) === num 1
axExp0R     = mkAxiom1 "Exp0R"    $ \(Pair s _) -> (s .^. num 0) === num 1
axExp1R     = mkAxiom1 "Exp1R"    $ \(Pair s t) -> (s .^. num 1) === t
axLeqRefl   = mkAxiom1 "LeqRefl"  $ \(Pair s _) -> (s <== s) === bool True
axCmpNatRefl    = mkAxiom1 "CmpNatRefl"
                $ \(Pair s _) -> (cmpNat s s) === ordering EQ
axCmpSymbolRefl = mkAxiom1 "CmpSymbolRefl"
                $ \(Pair s _) -> (cmpSymbol s s) === ordering EQ
axLeq0L     = mkAxiom1 "Leq0L"    $ \(Pair s _) -> (num 0 <== s) === bool True
axAppendSymbol0R  = mkAxiom1 "Concat0R"
            $ \(Pair s t) -> (mkStrLitTy nilFS `appendSymbol` s) === t
axAppendSymbol0L  = mkAxiom1 "Concat0L"
            $ \(Pair s t) -> (s `appendSymbol` mkStrLitTy nilFS) === t
axConsSymbol0 = mkAxiom1 "ConsSymbolNull"
            $ \(Pair s t) -> (s `consSymbol` mkStrLitTy nilFS) === t
axUnconsSymbol0 = mkAxiom1 "UnconsSymbolNull"
            $ \(Pair s _) -> (unconsSymbol s) === maybeType charSymbolPairKind Nothing

-- The list of built-in type family axioms that GHC uses.
-- If you define new axioms, make sure to include them in this list.
-- See Note [Adding built-in type families]
typeNatCoAxiomRules :: Map.Map FastString CoAxiomRule
typeNatCoAxiomRules = Map.fromList $ map (\x -> (coaxrName x, x))
  [ axAddDef
  , axMulDef
  , axExpDef
  , axLeqDef
  , axCmpNatDef
  , axCmpSymbolDef
  , axCmpCharDef
  , axLeqCharDef
  , axAppendSymbolDef
  , axConsSymbolDef
  , axConsSymbol0
  , axUnconsSymbol0
  , axUnconsSymbolDef
  , axToUpperDef
  , axToLowerDef
  , axToTitleDef
  , axNatToCharDef
  , axCharToNatDef
  , axAdd0L
  , axAdd0R
  , axMul0L
  , axMul0R
  , axMul1L
  , axMul1R
  , axExp1L
  , axExp0R
  , axExp1R
  , axLeqRefl
  , axCmpNatRefl
  , axCmpSymbolRefl
  , axCmpCharRefl
  , axLeqCharRefl
  , axLeq0L
  , axSubDef
  , axSub0R
  , axAppendSymbol0R
  , axAppendSymbol0L
  , axDivDef
  , axDiv1
  , axModDef
  , axMod1
  , axLogDef
  , axIsControlDef
  , axIsSpaceDef
  , axIsLowerDef
  , axIsUpperDef
  , axIsAlphaDef
  , axIsAlphaNumDef
  , axIsOctDigitDef
  , axIsHexDigitDef
  , axIsLetterDef
  , axIsPrintDef
  , axIsDigitDef
  , axGeneralCharCategoryDef
  ]



{-------------------------------------------------------------------------------
Various utilities for making axioms and types
-------------------------------------------------------------------------------}

(.+.) :: Type -> Type -> Type
s .+. t = mkTyConApp typeNatAddTyCon [s,t]

(.-.) :: Type -> Type -> Type
s .-. t = mkTyConApp typeNatSubTyCon [s,t]

(.*.) :: Type -> Type -> Type
s .*. t = mkTyConApp typeNatMulTyCon [s,t]

tDiv :: Type -> Type -> Type
tDiv s t = mkTyConApp typeNatDivTyCon [s,t]

tMod :: Type -> Type -> Type
tMod s t = mkTyConApp typeNatModTyCon [s,t]

(.^.) :: Type -> Type -> Type
s .^. t = mkTyConApp typeNatExpTyCon [s,t]

(<==) :: Type -> Type -> Type
s <== t = mkTyConApp typeNatLeqTyCon [s,t]

cmpNat :: Type -> Type -> Type
cmpNat s t = mkTyConApp typeNatCmpTyCon [s,t]

cmpSymbol :: Type -> Type -> Type
cmpSymbol s t = mkTyConApp typeSymbolCmpTyCon [s,t]

appendSymbol :: Type -> Type -> Type
appendSymbol s t = mkTyConApp typeSymbolAppendTyCon [s,t]

consSymbol :: Type -> Type -> Type
consSymbol s t = mkTyConApp typeConsSymbolTyCon [s,t]

unconsSymbol :: Type -> Type
unconsSymbol s = mkTyConApp typeUnconsSymbolTyCon [s]

(===) :: Type -> Type -> Pair Type
x === y = Pair x y

num :: Integer -> Type
num = mkNumLitTy

bool :: Bool -> Type
bool b = if b then mkTyConApp promotedTrueDataCon []
              else mkTyConApp promotedFalseDataCon []

maybeType :: Kind -> Maybe Type -> Type
maybeType k (Just x) = mkTyConApp promotedJustDataCon [k,x]
maybeType k (Nothing) = mkTyConApp promotedNothingDataCon [k]

maybeKind :: Type -> Kind
maybeKind t = mkTyConApp maybeTyCon [t]

pair :: Kind -> Kind -> Type -> Type -> Type
pair u x y z = mkTyConApp (promotedTupleDataCon Boxed 2) [u,x,y,z]

charSymbolPair :: Type -> Type -> Type
charSymbolPair = pair charTy typeSymbolKind

charSymbolPairKind :: Kind
charSymbolPairKind = mkTyConApp pairTyCon [charTy, typeSymbolKind]

isBoolLitTy :: Type -> Maybe Bool
isBoolLitTy tc =
  do (tc,[]) <- splitTyConApp_maybe tc
     case () of
       _ | tc == promotedFalseDataCon -> return False
         | tc == promotedTrueDataCon  -> return True
         | otherwise                   -> Nothing

orderingKind :: Kind
orderingKind = mkTyConApp orderingTyCon []

ordering :: Ordering -> Type
ordering o =
  case o of
    LT -> mkTyConApp promotedLTDataCon []
    EQ -> mkTyConApp promotedEQDataCon []
    GT -> mkTyConApp promotedGTDataCon []

isOrderingLitTy :: Type -> Maybe Ordering
isOrderingLitTy tc =
  do (tc1,[]) <- splitTyConApp_maybe tc
     case () of
       _ | tc1 == promotedLTDataCon -> return LT
         | tc1 == promotedEQDataCon -> return EQ
         | tc1 == promotedGTDataCon -> return GT
         | otherwise                -> Nothing

generalCat :: GeneralCategory -> Type
generalCat UppercaseLetter      = mkTyConApp promotedUppercaseLetterDataCon []
generalCat LowercaseLetter      = mkTyConApp promotedLowercaseLetterDataCon []
generalCat TitlecaseLetter      = mkTyConApp promotedTitlecaseLetterDataCon []
generalCat ModifierLetter       = mkTyConApp promotedModifierLetterDataCon []
generalCat OtherLetter          = mkTyConApp promotedOtherLetterDataCon []
generalCat NonSpacingMark       = mkTyConApp promotedNonSpacingMarkDataCon []
generalCat SpacingCombiningMark = mkTyConApp promotedSpacingCombiningMarkDataCon []
generalCat EnclosingMark        = mkTyConApp promotedEnclosingMarkDataCon []
generalCat DecimalNumber        = mkTyConApp promotedDecimalNumberDataCon []
generalCat LetterNumber         = mkTyConApp promotedLetterNumberDataCon []
generalCat OtherNumber          = mkTyConApp promotedOtherNumberDataCon []
generalCat ConnectorPunctuation = mkTyConApp promotedConnectorPunctuationDataCon []
generalCat DashPunctuation      = mkTyConApp promotedDashPunctuationDataCon []
generalCat OpenPunctuation      = mkTyConApp promotedOpenPunctuationDataCon []
generalCat ClosePunctuation     = mkTyConApp promotedClosePunctuationDataCon []
generalCat InitialQuote         = mkTyConApp promotedInitialQuoteDataCon []
generalCat FinalQuote           = mkTyConApp promotedFinalQuoteDataCon []
generalCat OtherPunctuation     = mkTyConApp promotedOtherPunctuationDataCon []
generalCat MathSymbol           = mkTyConApp promotedMathSymbolDataCon []
generalCat CurrencySymbol       = mkTyConApp promotedCurrencySymbolDataCon []
generalCat ModifierSymbol       = mkTyConApp promotedModifierSymbolDataCon []
generalCat OtherSymbol          = mkTyConApp promotedOtherSymbolDataCon []
generalCat Space                = mkTyConApp promotedSpaceDataCon []
generalCat LineSeparator        = mkTyConApp promotedLineSeparatorDataCon []
generalCat ParagraphSeparator   = mkTyConApp promotedParagraphSeparatorDataCon []
generalCat Control              = mkTyConApp promotedControlDataCon []
generalCat Format               = mkTyConApp promotedFormatDataCon []
generalCat Surrogate            = mkTyConApp promotedSurrogateDataCon []
generalCat PrivateUse           = mkTyConApp promotedPrivateUseDataCon []
generalCat NotAssigned          = mkTyConApp promotedNotAssignedDataCon []

isMaybeType :: Type -> Maybe (Maybe Type)
isMaybeType tc | Just (tc1,[t]) <- splitTyConApp_maybe tc, tc1 == promotedJustDataCon = return $ Just t
               | Just (tc1,[]) <- splitTyConApp_maybe tc, tc1 == promotedNothingDataCon = return $ Nothing
               | otherwise = Nothing

isTupleType :: Type -> Maybe (Type, Type)
isTupleType tc | Just (tc1, [x,y]) <- splitTyConApp_maybe tc, tc1 == (promotedTupleDataCon Boxed 2) = Just (x, y)
               | otherwise = Nothing

known :: (Integer -> Bool) -> TcType -> Bool
known p x = case isNumLitTy x of
              Just a  -> p a
              Nothing -> False

mkUnAxiom :: String -> TyCon -> (Integer -> Maybe Type) -> CoAxiomRule
mkUnAxiom str tc f =
  CoAxiomRule
    { coaxrName      = fsLit str
    , coaxrAsmpRoles = [Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2] <- return cs
           s2' <- isNumLitTy s2
           z   <- f s2'
           return (mkTyConApp tc [s1] === z)
    }

-- For the definitional axioms
mkBinAxiom :: String -> TyCon ->
              (Integer -> Integer -> Maybe Type) -> CoAxiomRule
mkBinAxiom str tc f =
  CoAxiomRule
    { coaxrName      = fsLit str
    , coaxrAsmpRoles = [Nominal, Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2, Pair t1 t2] <- return cs
           s2' <- isNumLitTy s2
           t2' <- isNumLitTy t2
           z   <- f s2' t2'
           return (mkTyConApp tc [s1,t1] === z)
    }

mkAxiom1 :: String -> (TypeEqn -> TypeEqn) -> CoAxiomRule
mkAxiom1 str f =
  CoAxiomRule
    { coaxrName      = fsLit str
    , coaxrAsmpRoles = [Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \case [eqn] -> Just (f eqn)
                             _     -> Nothing
    }


{-------------------------------------------------------------------------------
Evaluation
-------------------------------------------------------------------------------}

matchFamAdd :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamAdd [s,t]
  | Just 0 <- mbX = Just (axAdd0L, [t], t)
  | Just 0 <- mbY = Just (axAdd0R, [s], s)
  | Just x <- mbX, Just y <- mbY =
    Just (axAddDef, [s,t], num (x + y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamAdd _ = Nothing

matchFamSub :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamSub [s,t]
  | Just 0 <- mbY = Just (axSub0R, [s], s)
  | Just x <- mbX, Just y <- mbY, Just z <- minus x y =
    Just (axSubDef, [s,t], num z)
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamSub _ = Nothing

matchFamMul :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamMul [s,t]
  | Just 0 <- mbX = Just (axMul0L, [t], num 0)
  | Just 0 <- mbY = Just (axMul0R, [s], num 0)
  | Just 1 <- mbX = Just (axMul1L, [t], t)
  | Just 1 <- mbY = Just (axMul1R, [s], s)
  | Just x <- mbX, Just y <- mbY =
    Just (axMulDef, [s,t], num (x * y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamMul _ = Nothing

matchFamDiv :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamDiv [s,t]
  | Just 1 <- mbY = Just (axDiv1, [s], s)
  | Just x <- mbX, Just y <- mbY, y /= 0 = Just (axDivDef, [s,t], num (div x y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamDiv _ = Nothing

matchFamMod :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamMod [s,t]
  | Just 1 <- mbY = Just (axMod1, [s], num 0)
  | Just x <- mbX, Just y <- mbY, y /= 0 = Just (axModDef, [s,t], num (mod x y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamMod _ = Nothing

matchFamExp :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamExp [s,t]
  | Just 0 <- mbY = Just (axExp0R, [s], num 1)
  | Just 1 <- mbX = Just (axExp1L, [t], num 1)
  | Just 1 <- mbY = Just (axExp1R, [s], s)
  | Just x <- mbX, Just y <- mbY =
    Just (axExpDef, [s,t], num (x ^ y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamExp _ = Nothing

matchFamLog :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamLog [s]
  | Just x <- mbX, Just (n,_) <- genLog x 2 = Just (axLogDef, [s], num n)
  where mbX = isNumLitTy s
matchFamLog _ = Nothing

matchFamLeq :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamLeq [s,t]
  | Just 0 <- mbX = Just (axLeq0L, [t], bool True)
  | Just x <- mbX, Just y <- mbY =
    Just (axLeqDef, [s,t], bool (x <= y))
  | tcEqType s t  = Just (axLeqRefl, [s], bool True)
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamLeq _ = Nothing

matchFamCmpNat :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamCmpNat [s,t]
  | Just x <- mbX, Just y <- mbY =
    Just (axCmpNatDef, [s,t], ordering (compare x y))
  | tcEqType s t = Just (axCmpNatRefl, [s], ordering EQ)
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamCmpNat _ = Nothing

matchFamCmpSymbol :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamCmpSymbol [s,t]
  | Just x <- mbX, Just y <- mbY =
    Just (axCmpSymbolDef, [s,t], ordering (compare x y))
  | tcEqType s t = Just (axCmpSymbolRefl, [s], ordering EQ)
  where mbX = isStrLitTy s
        mbY = isStrLitTy t
matchFamCmpSymbol _ = Nothing

matchFamAppendSymbol :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamAppendSymbol [s,t]
  | Just x <- mbX, nullFS x = Just (axAppendSymbol0R, [t], t)
  | Just y <- mbY, nullFS y = Just (axAppendSymbol0L, [s], s)
  | Just x <- mbX, Just y <- mbY =
    Just (axAppendSymbolDef, [s,t], mkStrLitTy (appendFS x y))
  where
  mbX = isStrLitTy s
  mbY = isStrLitTy t
matchFamAppendSymbol _ = Nothing

matchFamConsSymbol :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamConsSymbol [s,t]
  | Just y <- mbY, nullFS y, Just x <- mbX = Just (axConsSymbol0, [t], mkStrLitTy (fsLit [x]))
  | Just x <- mbX, Just y <- mbY =
    Just (axConsSymbolDef, [s,t], mkStrLitTy (consFS x y))
  where
  mbX = isCharLitTy s
  mbY = isStrLitTy t
matchFamConsSymbol _ = Nothing

matchFamUnconsSymbol :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamUnconsSymbol [s]
  | Just x <- mbX, nullFS x = Just (axUnconsSymbol0, [s], maybeType charSymbolPairKind Nothing)
  | Just x <- mbX =
    Just (axUnconsSymbolDef, [s], maybeType charSymbolPairKind $ Just $ charSymbolPair (mkCharLitTy $ headFS x) (mkStrLitTy $ tailFS x))
  where
  mbX = isStrLitTy s
matchFamUnconsSymbol _ = Nothing

matchFamToUpper :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamToUpper [c]
  | Just c' <- isCharLitTy c = Just (axToUpperDef, [c], mkCharLitTy (Char.toUpper c'))
  | otherwise = Nothing
matchFamToUpper _ = Nothing

matchFamToLower :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamToLower [c]
  | Just c' <- isCharLitTy c = Just (axToLowerDef, [c], mkCharLitTy (Char.toLower c'))
  | otherwise = Nothing
matchFamToLower _ = Nothing

matchFamToTitle :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamToTitle [c]
  | Just c' <- isCharLitTy c = Just (axToTitleDef, [c], mkCharLitTy (Char.toTitle c'))
  | otherwise = Nothing
matchFamToTitle _ = Nothing

matchFamCharToNat :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamCharToNat [c]
  | Just c' <- isCharLitTy c = Just (axCharToNatDef, [c], mkNumLitTy (fromIntegral $ Char.ord c'))
  | otherwise = Nothing
matchFamCharToNat _ = Nothing

matchFamNatToChar :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamNatToChar [c]
  | Just c' <- isNumLitTy c, c' < 1114112 = Just (axNatToCharDef, [c], mkCharLitTy (Char.chr $ fromIntegral c'))
  | otherwise = Nothing
matchFamNatToChar _ = Nothing

{-------------------------------------------------------------------------------
Interact with axioms
-------------------------------------------------------------------------------}

interactTopAdd :: [Xi] -> Xi -> [Pair Type]
interactTopAdd [s,t] r
  | Just 0 <- mbZ = [ s === num 0, t === num 0 ]                          -- (s + t ~ 0) => (s ~ 0, t ~ 0)
  | Just x <- mbX, Just z <- mbZ, Just y <- minus z x = [t === num y]     -- (5 + t ~ 8) => (t ~ 3)
  | Just y <- mbY, Just z <- mbZ, Just x <- minus z y = [s === num x]     -- (s + 5 ~ 8) => (s ~ 3)
  where
  mbX = isNumLitTy s
  mbY = isNumLitTy t
  mbZ = isNumLitTy r
interactTopAdd _ _ = []

{-
Note [Weakened interaction rule for subtraction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A simpler interaction here might be:

  `s - t ~ r` --> `t + r ~ s`

This would enable us to reuse all the code for addition.
Unfortunately, this works a little too well at the moment.
Consider the following example:

    0 - 5 ~ r --> 5 + r ~ 0 --> (5 = 0, r = 0)

This (correctly) spots that the constraint cannot be solved.

However, this may be a problem if the constraint did not
need to be solved in the first place!  Consider the following example:

f :: Proxy (If (5 <=? 0) (0 - 5) (5 - 0)) -> Proxy 5
f = id

Currently, GHC is strict while evaluating functions, so this does not
work, because even though the `If` should evaluate to `5 - 0`, we
also evaluate the "then" branch which generates the constraint `0 - 5 ~ r`,
which fails.

So, for the time being, we only add an improvement when the RHS is a constant,
which happens to work OK for the moment, although clearly we need to do
something more general.
-}
interactTopSub :: [Xi] -> Xi -> [Pair Type]
interactTopSub [s,t] r
  | Just z <- mbZ = [ s === (num z .+. t) ]         -- (s - t ~ 5) => (5 + t ~ s)
  where
  mbZ = isNumLitTy r
interactTopSub _ _ = []





interactTopMul :: [Xi] -> Xi -> [Pair Type]
interactTopMul [s,t] r
  | Just 1 <- mbZ = [ s === num 1, t === num 1 ]                        -- (s * t ~ 1)  => (s ~ 1, t ~ 1)
  | Just x <- mbX, Just z <- mbZ, Just y <- divide z x = [t === num y]  -- (3 * t ~ 15) => (t ~ 5)
  | Just y <- mbY, Just z <- mbZ, Just x <- divide z y = [s === num x]  -- (s * 3 ~ 15) => (s ~ 5)
  where
  mbX = isNumLitTy s
  mbY = isNumLitTy t
  mbZ = isNumLitTy r
interactTopMul _ _ = []

interactTopDiv :: [Xi] -> Xi -> [Pair Type]
interactTopDiv _ _ = []   -- I can't think of anything...

interactTopMod :: [Xi] -> Xi -> [Pair Type]
interactTopMod _ _ = []   -- I can't think of anything...

interactTopExp :: [Xi] -> Xi -> [Pair Type]
interactTopExp [s,t] r
  | Just 0 <- mbZ = [ s === num 0 ]                                       -- (s ^ t ~ 0) => (s ~ 0)
  | Just x <- mbX, Just z <- mbZ, Just y <- logExact  z x = [t === num y] -- (2 ^ t ~ 8) => (t ~ 3)
  | Just y <- mbY, Just z <- mbZ, Just x <- rootExact z y = [s === num x] -- (s ^ 2 ~ 9) => (s ~ 3)
  where
  mbX = isNumLitTy s
  mbY = isNumLitTy t
  mbZ = isNumLitTy r
interactTopExp _ _ = []

interactTopLog :: [Xi] -> Xi -> [Pair Type]
interactTopLog _ _ = []   -- I can't think of anything...


interactTopLeq :: [Xi] -> Xi -> [Pair Type]
interactTopLeq [s,t] r
  | Just 0 <- mbY, Just True <- mbZ = [ s === num 0 ]                     -- (s <= 0) => (s ~ 0)
  where
  mbY = isNumLitTy t
  mbZ = isBoolLitTy r
interactTopLeq _ _ = []

interactTopCmpNat :: [Xi] -> Xi -> [Pair Type]
interactTopCmpNat [s,t] r
  | Just EQ <- isOrderingLitTy r = [ s === t ]
interactTopCmpNat _ _ = []

interactTopCmpSymbol :: [Xi] -> Xi -> [Pair Type]
interactTopCmpSymbol [s,t] r
  | Just EQ <- isOrderingLitTy r = [ s === t ]
interactTopCmpSymbol _ _ = []

interactTopAppendSymbol :: [Xi] -> Xi -> [Pair Type]
interactTopAppendSymbol [s,t] r
  -- (AppendSymbol a b ~ "") => (a ~ "", b ~ "")
  | Just z <- mbZ, nullFS z =
    [s === mkStrLitTy nilFS, t === mkStrLitTy nilFS ]

  -- (AppendSymbol "foo" b ~ "foobar") => (b ~ "bar")
  | Just x <- fmap unpackFS mbX, Just z <- fmap unpackFS mbZ, x `isPrefixOf` z =
    [ t === mkStrLitTy (mkFastString $ drop (length x) z) ]

  -- (AppendSymbol f "bar" ~ "foobar") => (f ~ "foo")
  | Just y <- fmap unpackFS mbY, Just z <- fmap unpackFS mbZ, y `isSuffixOf` z =
    [ t === mkStrLitTy (mkFastString $ take (length z - length y) z) ]

  where
  mbX = isStrLitTy s
  mbY = isStrLitTy t
  mbZ = isStrLitTy r

interactTopAppendSymbol _ _ = []

interactTopConsSymbol :: [Xi] -> Xi -> [Pair Type]
interactTopConsSymbol [s,t] r
  -- (ConsSymbol f b ~ "а") => (f ~ 'a', b ~ "")
  | Just [z] <- fmap unpackFS mbZ =
    [s === mkCharLitTy z, t === mkStrLitTy nilFS ]

  -- (ConsSymbol 'f' b ~ "foobar") => (b ~ "oobar")
  | Just x <- mbX, Just z <- fmap unpackFS mbZ, [x] `isPrefixOf` z =
    [ t === mkStrLitTy (mkFastString $ tail z) ]

  -- (ConsSymbol a "oobar" ~ "foobar") => (a ~ 'f')
  | Just y <- fmap unpackFS mbY, Just z <- fmap unpackFS mbZ, y `isSuffixOf` z =
    [ t === (mkCharLitTy $ head z) ]

  where
  mbX = isCharLitTy s
  mbY = isStrLitTy t
  mbZ = isStrLitTy r

interactTopConsSymbol _ _ = []

interactTopUnconsSymbol :: [Xi] -> Xi -> [Pair Type]
interactTopUnconsSymbol [s] r
  -- (UnconsSymbol b ~ Nothing) => (b ~ "")
  | Just Nothing <- mbZ =
    [s === mkStrLitTy nilFS ]
  -- (UnconsSymbol b ~ Just ('f',"oobar")) => (b ~ "foobar")
  | Just (c,str) <- mbZTuple, Just chr <- isCharLitTy c, Just str1 <- isStrLitTy str =
    [s === (mkStrLitTy $ consFS chr str1)]

  where
  mbZ = isMaybeType r
  mbZTuple = isTupleType =<< join mbZ

interactTopUnconsSymbol _ _ = []


{-------------------------------------------------------------------------------
Interaction with inerts
-------------------------------------------------------------------------------}

interactInertAdd :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertAdd [x1,y1] z1 [x2,y2] z2
  | sameZ && tcEqType x1 x2         = [ y1 === y2 ]
  | sameZ && tcEqType y1 y2         = [ x1 === x2 ]
  where sameZ = tcEqType z1 z2
interactInertAdd _ _ _ _ = []

interactInertSub :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertSub [x1,y1] z1 [x2,y2] z2
  | sameZ && tcEqType x1 x2         = [ y1 === y2 ]
  | sameZ && tcEqType y1 y2         = [ x1 === x2 ]
  where sameZ = tcEqType z1 z2
interactInertSub _ _ _ _ = []

interactInertMul :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertMul [x1,y1] z1 [x2,y2] z2
  | sameZ && known (/= 0) x1 && tcEqType x1 x2 = [ y1 === y2 ]
  | sameZ && known (/= 0) y1 && tcEqType y1 y2 = [ x1 === x2 ]
  where sameZ   = tcEqType z1 z2

interactInertMul _ _ _ _ = []

interactInertDiv :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertDiv _ _ _ _ = []

interactInertMod :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertMod _ _ _ _ = []

interactInertExp :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertExp [x1,y1] z1 [x2,y2] z2
  | sameZ && known (> 1) x1 && tcEqType x1 x2 = [ y1 === y2 ]
  | sameZ && known (> 0) y1 && tcEqType y1 y2 = [ x1 === x2 ]
  where sameZ = tcEqType z1 z2

interactInertExp _ _ _ _ = []

interactInertLog :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertLog _ _ _ _ = []


interactInertLeq :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertLeq [x1,y1] z1 [x2,y2] z2
  | bothTrue && tcEqType x1 y2 && tcEqType y1 x2 = [ x1 === y1 ]
  | bothTrue && tcEqType y1 x2                 = [ (x1 <== y2) === bool True ]
  | bothTrue && tcEqType y2 x1                 = [ (x2 <== y1) === bool True ]
  where bothTrue = isJust $ do True <- isBoolLitTy z1
                               True <- isBoolLitTy z2
                               return ()

interactInertLeq _ _ _ _ = []


interactInertAppendSymbol :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertAppendSymbol [x1,y1] z1 [x2,y2] z2
  | sameZ && tcEqType x1 x2         = [ y1 === y2 ]
  | sameZ && tcEqType y1 y2         = [ x1 === x2 ]
  where sameZ = tcEqType z1 z2
interactInertAppendSymbol _ _ _ _ = []


interactInertConsSymbol :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertConsSymbol [x1,y1] z1 [x2,y2] z2
  | sameZ && tcEqType x1 x2         = [ y1 === y2 ]
  | sameZ && tcEqType y1 y2         = [ x1 === x2 ]
  where sameZ = tcEqType z1 z2
interactInertConsSymbol _ _ _ _ = []

interactInertUnconsSymbol :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertUnconsSymbol [x1] z1 [x2] z2
  | sameZ = [ x1 === x2 ]
  where sameZ = tcEqType z1 z2
interactInertUnconsSymbol _ _ _ _ = []


{- -----------------------------------------------------------------------------
These inverse functions are used for simplifying propositions using
concrete natural numbers.
----------------------------------------------------------------------------- -}

-- | Subtract two natural numbers.
minus :: Integer -> Integer -> Maybe Integer
minus x y = if x >= y then Just (x - y) else Nothing

-- | Compute the exact logarithm of a natural number.
-- The logarithm base is the second argument.
logExact :: Integer -> Integer -> Maybe Integer
logExact x y = do (z,True) <- genLog x y
                  return z


-- | Divide two natural numbers.
divide :: Integer -> Integer -> Maybe Integer
divide _ 0  = Nothing
divide x y  = case divMod x y of
                (a,0) -> Just a
                _     -> Nothing

-- | Compute the exact root of a natural number.
-- The second argument specifies which root we are computing.
rootExact :: Integer -> Integer -> Maybe Integer
rootExact x y = do (z,True) <- genRoot x y
                   return z



{- | Compute the n-th root of a natural number, rounded down to
the closest natural number.  The boolean indicates if the result
is exact (i.e., True means no rounding was done, False means rounded down).
The second argument specifies which root we are computing. -}
genRoot :: Integer -> Integer -> Maybe (Integer, Bool)
genRoot _  0    = Nothing
genRoot x0 1    = Just (x0, True)
genRoot x0 root = Just (search 0 (x0+1))
  where
  search from to = let x = from + div (to - from) 2
                       a = x ^ root
                   in case compare a x0 of
                        EQ              -> (x, True)
                        LT | x /= from  -> search x to
                           | otherwise  -> (from, False)
                        GT | x /= to    -> search from x
                           | otherwise  -> (from, False)

{- | Compute the logarithm of a number in the given base, rounded down to the
closest integer.  The boolean indicates if we the result is exact
(i.e., True means no rounding happened, False means we rounded down).
The logarithm base is the second argument. -}
genLog :: Integer -> Integer -> Maybe (Integer, Bool)
genLog x 0    = if x == 1 then Just (0, True) else Nothing
genLog _ 1    = Nothing
genLog 0 _    = Nothing
genLog x base = Just (exactLoop 0 x)
  where
  exactLoop s i
    | i == 1     = (s,True)
    | i < base   = (s,False)
    | otherwise  =
        let s1 = s + 1
        in s1 `seq` case divMod i base of
                      (j,r)
                        | r == 0    -> exactLoop s1 j
                        | otherwise -> (underLoop s1 j, False)

  underLoop s i
    | i < base  = s
    | otherwise = let s1 = s + 1 in s1 `seq` underLoop s1 (div i base)

-----------------------------------------------------------------------------

typeCharCmpTyCon :: TyCon
typeCharCmpTyCon =
  mkFamilyTyCon name
    (mkTemplateAnonTyConBinders [ charTy, charTy ])
    orderingKind
    Nothing
    (BuiltInSynFamTyCon ops)
    Nothing
    NotInjective
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "CmpChar")
                  typeCharCmpTyFamNameKey typeCharCmpTyCon
  ops = BuiltInSynFamily
      { sfMatchFam      = matchFamCmpChar
      , sfInteractTop   = interactTopCmpChar
      , sfInteractInert = \_ _ _ _ -> []
      }

interactTopCmpChar :: [Xi] -> Xi -> [Pair Type]
interactTopCmpChar [s,t] r
  | Just EQ <- isOrderingLitTy r = [ s === t ]
interactTopCmpChar _ _ = []

cmpChar :: Type -> Type -> Type
cmpChar s t = mkTyConApp typeCharCmpTyCon [s,t]

axCmpCharDef, axCmpCharRefl :: CoAxiomRule
axCmpCharDef =
  CoAxiomRule
    { coaxrName      = fsLit "CmpCharDef"
    , coaxrAsmpRoles = [Nominal, Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2, Pair t1 t2] <- return cs
           s2' <- isCharLitTy s2
           t2' <- isCharLitTy t2
           return (mkTyConApp typeCharCmpTyCon [s1,t1] ===
                   ordering (compare s2' t2')) }
axCmpCharRefl = mkAxiom1 "CmpCharRefl"
  $ \(Pair s _) -> (cmpChar s s) === ordering EQ

matchFamCmpChar :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamCmpChar [s,t]
  | Just x <- mbX, Just y <- mbY =
    Just (axCmpCharDef, [s,t], ordering (compare x y))
  | tcEqType s t = Just (axCmpCharRefl, [s], ordering EQ)
  where mbX = isCharLitTy s
        mbY = isCharLitTy t
matchFamCmpChar _ = Nothing

-----------------------------------------------------------------------------

typeLeqCharTyCon :: TyCon
typeLeqCharTyCon =
  mkFamilyTyCon name
    (mkTemplateAnonTyConBinders [ charTy, charTy ])
    boolTy
    Nothing
    (BuiltInSynFamTyCon ops)
    Nothing
    NotInjective
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "LeqChar")
                  typeLeqCharTyFamNameKey typeLeqCharTyCon
  ops = BuiltInSynFamily
      { sfMatchFam      = matchFamLeqChar
      , sfInteractTop   = interactTopLeqChar
      , sfInteractInert = interactInertLeqChar
      }

interactTopLeqChar :: [Xi] -> Xi -> [Pair Type]
interactTopLeqChar [s,t] r
  | Just True <- isBoolLitTy r = [ s === t ]
interactTopLeqChar _ _ = []

leqChar :: Type -> Type -> Type
leqChar s t = mkTyConApp typeLeqCharTyCon [s,t]

axLeqCharDef, axLeqCharRefl :: CoAxiomRule
axLeqCharDef =
  CoAxiomRule
    { coaxrName      = fsLit "LeqCharDef"
    , coaxrAsmpRoles = [Nominal, Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2, Pair t1 t2] <- return cs
           s2' <- isCharLitTy s2
           t2' <- isCharLitTy t2
           return (mkTyConApp typeLeqCharTyCon [s1,t1] ===
                   bool (s2' <= t2')) }
axLeqCharRefl = mkAxiom1 "LeqCharRefl"
  $ \(Pair s _) -> (leqChar s s) === bool True

matchFamLeqChar :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamLeqChar [s,t]
  | Just x <- mbX, Just y <- mbY =
    Just (axLeqCharDef, [s,t], bool (x <= y))
  | tcEqType s t = Just (axLeqCharRefl, [s], bool True)
  where mbX = isCharLitTy s
        mbY = isCharLitTy t
matchFamLeqChar _ = Nothing

interactInertLeqChar :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertLeqChar [x1,y1] z1 [x2,y2] z2
  | bothTrue && tcEqType x1 y2 && tcEqType y1 x2 = [ x1 === y1 ]
  | bothTrue && tcEqType y1 x2                 = [ (x1 `leqChar` y2) === bool True ]
  | bothTrue && tcEqType y2 x1                 = [ (x2 `leqChar` y1) === bool True ]
  where bothTrue = isJust $ do True <- isBoolLitTy z1
                               True <- isBoolLitTy z2
                               return ()
interactInertLeqChar _ _ _ _ = []

-----------------------------------------------------------------------------

-- | Type-level char predicates

-- | TyCons

typeIsControlTyCon :: TyCon
typeIsControlTyCon = mkTypeCharPredicateTyCon name
  BuiltInSynFamily
    { sfMatchFam      = matchFamIsControl
    , sfInteractTop   = \_ _ -> []
    , sfInteractInert = \_ _ _ _ -> []
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "IsControl")
                  typeIsControlTyFamNameKey typeIsControlTyCon


typeIsSpaceTyCon :: TyCon
typeIsSpaceTyCon = mkTypeCharPredicateTyCon name
  BuiltInSynFamily
    { sfMatchFam      = matchFamIsSpace
    , sfInteractTop   = \_ _ -> []
    , sfInteractInert = \_ _ _ _ -> []
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "IsSpace")
                  typeIsSpaceTyFamNameKey typeIsSpaceTyCon

typeIsLowerTyCon :: TyCon
typeIsLowerTyCon = mkTypeCharPredicateTyCon name
  BuiltInSynFamily
    { sfMatchFam      = matchFamIsLower
    , sfInteractTop   = \_ _ -> []
    , sfInteractInert = \_ _ _ _ -> []
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "IsLower")
                  typeIsLowerTyFamNameKey typeIsLowerTyCon

typeIsUpperTyCon :: TyCon
typeIsUpperTyCon = mkTypeCharPredicateTyCon name
  BuiltInSynFamily
    { sfMatchFam      = matchFamIsUpper
    , sfInteractTop   = \_ _ -> []
    , sfInteractInert = \_ _ _ _ -> []
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "IsUpper")
                  typeIsUpperTyFamNameKey typeIsUpperTyCon

typeIsAlphaTyCon :: TyCon
typeIsAlphaTyCon = mkTypeCharPredicateTyCon name
  BuiltInSynFamily
    { sfMatchFam      = matchFamIsAlpha
    , sfInteractTop   = \_ _ -> []
    , sfInteractInert = \_ _ _ _ -> []
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "IsAlpha")
                  typeIsAlphaTyFamNameKey typeIsAlphaTyCon

typeIsAlphaNumTyCon :: TyCon
typeIsAlphaNumTyCon = mkTypeCharPredicateTyCon name
  BuiltInSynFamily
    { sfMatchFam      = matchFamIsAlphaNum
    , sfInteractTop   = \_ _ -> []
    , sfInteractInert = \_ _ _ _ -> []
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "IsAlphaNum")
                  typeIsAlphaNumTyFamNameKey typeIsAlphaNumTyCon

typeIsPrintTyCon :: TyCon
typeIsPrintTyCon = mkTypeCharPredicateTyCon name
  BuiltInSynFamily
    { sfMatchFam      = matchFamIsPrint
    , sfInteractTop   = \_ _ -> []
    , sfInteractInert = \_ _ _ _ -> []
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "IsPrint")
                  typeIsPrintTyFamNameKey typeIsPrintTyCon

typeIsDigitTyCon :: TyCon
typeIsDigitTyCon = mkTypeCharPredicateTyCon name
  BuiltInSynFamily
    { sfMatchFam      = matchFamIsDigit
    , sfInteractTop   = \_ _ -> []
    , sfInteractInert = \_ _ _ _ -> []
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "IsDigit")
                  typeIsDigitTyFamNameKey typeIsDigitTyCon

typeIsOctDigitTyCon :: TyCon
typeIsOctDigitTyCon = mkTypeCharPredicateTyCon name
  BuiltInSynFamily
    { sfMatchFam      = matchFamIsOctDigit
    , sfInteractTop   = \_ _ -> []
    , sfInteractInert = \_ _ _ _ -> []
    }
  where
    name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "IsOctDigit")
                    typeIsOctDigitTyFamNameKey typeIsOctDigitTyCon

typeIsHexDigitTyCon :: TyCon
typeIsHexDigitTyCon = mkTypeCharPredicateTyCon name
  BuiltInSynFamily
    { sfMatchFam      = matchFamIsHexDigit
    , sfInteractTop   = \_ _ -> []
    , sfInteractInert = \_ _ _ _ -> []
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "IsHexDigit")
                  typeIsHexDigitTyFamNameKey typeIsHexDigitTyCon

typeIsLetterTyCon :: TyCon
typeIsLetterTyCon = mkTypeCharPredicateTyCon name
  BuiltInSynFamily
    { sfMatchFam      = matchFamIsLetter
    , sfInteractTop   = \_ _ -> []
    , sfInteractInert = \_ _ _ _ -> []
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "IsLetter")
                  typeIsLetterTyFamNameKey typeIsLetterTyCon



-- | Reduction rules

-- | This is a helper that allows one generate the definitional axioms for unary
-- | predicates on characters

mkAxiomCharPredicate :: String -> TyCon -> (Char -> Maybe Type) -> CoAxiomRule
mkAxiomCharPredicate str tc f =
  CoAxiomRule
    { coaxrName      = fsLit str
    , coaxrAsmpRoles = [Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2] <- return cs
           s2' <- isCharLitTy s2
           z   <- f s2'
           return (mkTyConApp tc [s1] === z)
    }

axIsControlDef
  , axIsSpaceDef
  , axIsLowerDef
  , axIsUpperDef
  , axIsAlphaDef
  , axIsAlphaNumDef
  , axIsOctDigitDef
  , axIsHexDigitDef
  , axIsLetterDef
  , axIsPrintDef
  , axIsDigitDef
  :: CoAxiomRule
axIsControlDef = mkAxiomCharPredicate "IsControlDef" typeIsControlTyCon $
                   \x -> return $ bool (Char.isControl x)

axIsSpaceDef = mkAxiomCharPredicate "IsSpaceDef" typeIsSpaceTyCon $
                 \x -> return $ bool (Char.isSpace x)

axIsLowerDef = mkAxiomCharPredicate "IsLowerDef" typeIsLowerTyCon $
                 \x -> return $ bool (Char.isLower x)

axIsUpperDef = mkAxiomCharPredicate "IsUpperDef" typeIsUpperTyCon $
                 \x -> return $ bool (Char.isUpper x)

axIsAlphaDef = mkAxiomCharPredicate "IsAlphaDef" typeIsAlphaTyCon $
                 \x -> return $ bool (Char.isAlpha x)

axIsAlphaNumDef = mkAxiomCharPredicate "IsAlphaNumDef" typeIsAlphaNumTyCon $
                    \x -> return $ bool (Char.isAlphaNum x)

axIsOctDigitDef = mkAxiomCharPredicate "IsOctDigitDef" typeIsOctDigitTyCon $
                    \x -> return $ bool (Char.isOctDigit x)

axIsHexDigitDef = mkAxiomCharPredicate "IsHexDigitDef" typeIsHexDigitTyCon $
                    \x -> return $ bool (Char.isHexDigit x)

axIsLetterDef = mkAxiomCharPredicate "IsLetterDef" typeIsLetterTyCon $
                  \x -> return $ bool (Char.isLetter x)

axIsPrintDef = mkAxiomCharPredicate "IsPrintDef" typeIsPrintTyCon $
                 \x -> return $ bool (Char.isPrint x)

axIsDigitDef = mkAxiomCharPredicate "IsDigitDef" typeIsDigitTyCon $
                 \x -> return $ bool (Char.isDigit x)


matchFamIsControl :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamIsControl [c]
  | Just c' <- isCharLitTy c = Just (axIsControlDef, [c], bool (Char.isControl c'))
  | otherwise = Nothing
matchFamIsControl _ = Nothing

matchFamIsSpace :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamIsSpace [c]
  | Just c' <- isCharLitTy c = Just (axIsSpaceDef, [c], bool (Char.isSpace c'))
  | otherwise = Nothing
matchFamIsSpace _ = Nothing

matchFamIsLower :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamIsLower [c]
  | Just c' <- isCharLitTy c = Just (axIsLowerDef, [c], bool (Char.isLower c'))
  | otherwise = Nothing
matchFamIsLower _ = Nothing

matchFamIsUpper :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamIsUpper [c]
  | Just c' <- isCharLitTy c = Just (axIsUpperDef, [c], bool (Char.isUpper c'))
  | otherwise = Nothing
matchFamIsUpper _ = Nothing

matchFamIsAlpha :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamIsAlpha [c]
  | Just c' <- isCharLitTy c = Just (axIsAlphaDef, [c], bool (Char.isAlpha c'))
  | otherwise = Nothing
matchFamIsAlpha _ = Nothing

matchFamIsAlphaNum :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamIsAlphaNum [c]
  | Just c' <- isCharLitTy c = Just (axIsAlphaNumDef, [c], bool (Char.isAlphaNum c'))
  | otherwise = Nothing
matchFamIsAlphaNum _ = Nothing

matchFamIsPrint :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamIsPrint [c]
  | Just c' <- isCharLitTy c = Just (axIsPrintDef, [c], bool (Char.isPrint c'))
  | otherwise = Nothing
matchFamIsPrint _ = Nothing

matchFamIsDigit :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamIsDigit [c]
  | Just c' <- isCharLitTy c = Just (axIsDigitDef, [c], bool (Char.isDigit c'))
  | otherwise = Nothing
matchFamIsDigit _ = Nothing

matchFamIsOctDigit :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamIsOctDigit [c]
  | Just c' <- isCharLitTy c = Just (axIsOctDigitDef, [c], bool (Char.isOctDigit c'))
  | otherwise = Nothing
matchFamIsOctDigit _ = Nothing

matchFamIsHexDigit :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamIsHexDigit [c]
  | Just c' <- isCharLitTy c = Just (axIsHexDigitDef, [c], bool (Char.isHexDigit c'))
  | otherwise = Nothing
matchFamIsHexDigit _ = Nothing

matchFamIsLetter :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamIsLetter [c]
  | Just c' <- isCharLitTy c = Just (axIsLetterDef, [c], bool (Char.isLetter c'))
  | otherwise = Nothing
matchFamIsLetter _ = Nothing

-- | The functions required for the GeneralCharCategory built-in type family

typeGeneralCharCategoryTyCon :: TyCon
typeGeneralCharCategoryTyCon = mkTypeCharCategoryTyCon name
  BuiltInSynFamily
    { sfMatchFam      = matchFamGeneralCharCategory
    , sfInteractTop   = \_ _ -> []
    , sfInteractInert = \_ _ _ _ -> []
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "GeneralCharCategory")
                  typeGeneralCharCategoryTyFamKey typeGeneralCharCategoryTyCon

mkAxiomCharGeneral :: String -> TyCon -> (Char -> Maybe Type) -> CoAxiomRule
mkAxiomCharGeneral str tc f =
  CoAxiomRule
    { coaxrName      = fsLit str
    , coaxrAsmpRoles = [Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2] <- return cs
           s2' <- isCharLitTy s2
           z   <- f s2'
           return (mkTyConApp tc [s1] === z)
    }

axGeneralCharCategoryDef :: CoAxiomRule
axGeneralCharCategoryDef = mkAxiomCharGeneral "GeneralCharCategory" typeGeneralCharCategoryTyCon $
                             \x -> return $ generalCat (generalCategory x)

matchFamGeneralCharCategory :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamGeneralCharCategory [c]
  | Just c' <- isCharLitTy c = Just (axGeneralCharCategoryDef, [c], generalCat (generalCategory c'))
  | otherwise = Nothing
matchFamGeneralCharCategory _ = Nothing
