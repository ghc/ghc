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
  , typeConsSymbolTyCon
  , typeUnconsSymbolTyCon
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
import GHC.Types.Unique.FM
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
                  , typeConsSymbolTyFamNameKey
                  , typeUnconsSymbolTyFamNameKey
                  )
import GHC.Data.FastString
import Data.Maybe ( isJust )
import Control.Monad ( guard, join )
import Data.List  ( isPrefixOf, isSuffixOf )

{-
Note [Type-level literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~
There are currently three forms of type-level literals: natural numbers, symbols, and
chars.

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
  , typeConsSymbolTyCon
  , typeUnconsSymbolTyCon
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
    (mkTemplateAnonTyConBinders [ naturalTy, naturalTy ])
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
    (mkTemplateAnonTyConBinders [ naturalTy, naturalTy ])
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
    (Injective [True, True])
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

-- Make a unary built-in constructor of kind: Nat -> Nat
mkTypeNatFunTyCon1 :: Name -> BuiltInSynFamily -> TyCon
mkTypeNatFunTyCon1 op tcb =
  mkFamilyTyCon op
    (mkTemplateAnonTyConBinders [ naturalTy ])
    naturalTy
    Nothing
    (BuiltInSynFamTyCon tcb)
    Nothing
    NotInjective

-- Make a binary built-in constructor of kind: Nat -> Nat -> Nat
mkTypeNatFunTyCon2 :: Name -> BuiltInSynFamily -> TyCon
mkTypeNatFunTyCon2 op tcb =
  mkFamilyTyCon op
    (mkTemplateAnonTyConBinders [ naturalTy, naturalTy ])
    naturalTy
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
  , axUnconsSymbolDef
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
                   ordering (lexicalCompareFS s2' t2')) }

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
           case unconsFS s2' of
             Nothing         -> return (mkTyConApp typeUnconsSymbolTyCon [s1] ===
                                  (maybeType charSymbolPairKind Nothing))
             Just (char, fs) -> return (mkTyConApp typeUnconsSymbolTyCon [s1] ===
                                  (maybeType charSymbolPairKind $
                                  Just $
                                    charSymbolPair
                                      (mkCharLitTy char)
                                      (mkStrLitTy fs)))
    }

axSubDef = mkBinAxiom "SubDef" typeNatSubTyCon $
              \x y -> fmap num (minus x y)

axDivDef = mkBinAxiom "DivDef" typeNatDivTyCon $
              \x y -> do guard (y /= 0)
                         return (num (div x y))

axModDef = mkBinAxiom "ModDef" typeNatModTyCon $
              \x y -> do guard (y /= 0)
                         return (num (mod x y))

axLogDef = mkUnAxiom "LogDef" typeNatLogTyCon isNumLitTy $
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

-- The list of built-in type family axioms that GHC uses.
-- If you define new axioms, make sure to include them in this list.
-- See Note [Adding built-in type families]
typeNatCoAxiomRules :: UniqFM FastString CoAxiomRule
typeNatCoAxiomRules = listToUFM $ map (\x -> (coaxrName x, x))
  [ axAddDef
  , axMulDef
  , axExpDef
  , axLeqDef
  , axCmpNatDef
  , axCmpSymbolDef
  , axCmpCharDef
  , axAppendSymbolDef
  , axConsSymbolDef
  , axUnconsSymbolDef
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

(===) :: Type -> Type -> Pair Type
x === y = Pair x y

num :: Integer -> Type
num = mkNumLitTy

bool :: Bool -> Type
bool b = if b then mkTyConApp promotedTrueDataCon []
              else mkTyConApp promotedFalseDataCon []

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

known :: (Integer -> Bool) -> TcType -> Bool
known p x = case isNumLitTy x of
              Just a  -> p a
              Nothing -> False

mkUnAxiom :: String -> TyCon -> (Type -> Maybe a) -> (a -> Maybe Type) -> CoAxiomRule
mkUnAxiom str tc toMb f =
  CoAxiomRule
    { coaxrName      = fsLit str
    , coaxrAsmpRoles = [Nominal]
    , coaxrRole      = Nominal
    , coaxrProves    = \cs ->
        do [Pair s1 s2] <- return cs
           s2' <- toMb s2
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
    Just (axCmpSymbolDef, [s,t], ordering (lexicalCompareFS x y))
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
  | Just x <- mbX, Just y <- mbY =
    Just (axConsSymbolDef, [s,t], mkStrLitTy (consFS x y))
  where
  mbX = isCharLitTy s
  mbY = isStrLitTy t
matchFamConsSymbol _ = Nothing

matchFamUnconsSymbol :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamUnconsSymbol [s]
  | Just x <- mbX =
    case nullFS x of
      True  -> Just (axUnconsSymbolDef, [s], maybeType charSymbolPairKind Nothing)
      False -> Just (axUnconsSymbolDef, [s], maybeType charSymbolPairKind $ Just $ charSymbolPair (mkCharLitTy $ headFS x) (mkStrLitTy $ tailFS x))
  where
  mbX = isStrLitTy s
matchFamUnconsSymbol _ = Nothing

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
  -- ConsSymbol a b ~ "blah" => (a ~ 'b', b ~ "lah")
  | Just x <- mbX
  , Just y <- fmap unpackFS mbY
  , Just z <- fmap unpackFS mbZ
  , y `isSuffixOf` z
  , [x] `isPrefixOf` z =
    [ r === (mkCharLitTy $ head z), r === mkStrLitTy (mkFastString $ tail z) ]

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
interactInertConsSymbol [x1, y1] z1 [x2, y2] z2
  | sameZ         = [ x1 === x2, y1 === y2 ]
  where sameZ = tcEqType z1 z2
interactInertConsSymbol _ _ _ _ = []

interactInertUnconsSymbol :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertUnconsSymbol [x1, y1] z1 [x2, y2] z2
  | sameZ = [ x1 === x2, y1 === y2 ]
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
