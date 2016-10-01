module Options.Language where

import Types

languageOptions :: [Flag]
languageOptions =
  [ flag { flagName = "-fconstraint-solver-iterations=⟨n⟩"
         , flagDescription =
           "*default: 4.* Set the iteration limit for the type-constraint "++
           "solver. Typically one iteration suffices; so please "++
           "yell if you find you need to set it higher than the default. "++
           "Zero means infinity."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-freduction-depth=⟨n⟩"
         , flagDescription =
           "*default: 200.* Set the :ref:`limit for type simplification "++
           "<undecidable-instances>`. Zero means infinity."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fcontext-stack=⟨n⟩"
         , flagDescription =
           "Deprecated. Use ``-freduction-depth=⟨n⟩`` instead."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fglasgow-exts"
         , flagDescription =
           "Deprecated. Enable most language extensions; "++
           "see :ref:`options-language` for exactly which ones."
         , flagType = DynamicFlag
         , flagReverse = "-fno-glasgow-exts"
         }
  , flag { flagName = "-firrefutable-tuples"
         , flagDescription = "Make tuple pattern matching irrefutable"
         , flagType = DynamicFlag
         , flagReverse = "-fno-irrefutable-tuples"
         }
  , flag { flagName = "-fpackage-trust"
         , flagDescription =
           "Enable :ref:`Safe Haskell <safe-haskell>` trusted package "++
           "requirement for trustworthy modules."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ftype-function-depth=⟨n⟩"
         , flagDescription = "Deprecated. Use ``-freduction-depth=⟨n⟩`` instead."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-XAllowAmbiguousTypes"
         , flagDescription =
           "Allow the user to write :ref:`ambiguous types <ambiguity>`, and "++
           "the type inference engine to infer them."
         , flagType = DynamicFlag
         , flagReverse = "-XNoAllowAmbiguousTypes"
         , flagSince = "7.8.1"
         }
  , flag { flagName = "-XArrows"
         , flagDescription =
           "Enable :ref:`arrow notation <arrow-notation>` extension"
         , flagType = DynamicFlag
         , flagReverse = "-XNoArrows"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XApplicativeDo"
         , flagDescription =
           "Enable :ref:`Applicative do-notation desugaring <applicative-do>`"
         , flagType = DynamicFlag
         , flagReverse = "-XNoApplicativeDo"
         , flagSince = "8.0.1"
         }
  , flag { flagName = "-XAutoDeriveTypeable"
         , flagDescription =
           "As of GHC 7.10, this option is not needed, and should not be "++
           "used. Previously this would automatically :ref:`derive Typeable "++
           "instances for every datatype and type class declaration "++
           "<deriving-typeable>`. Implies :ghc-flag:`-XDeriveDataTypeable`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoAutoDeriveTypeable"
         , flagSince = "7.8.1"
         }
  , flag { flagName = "-XBangPatterns"
         , flagDescription = "Enable :ref:`bang patterns <bang-patterns>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoBangPatterns"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XBinaryLiterals"
         , flagDescription =
           "Enable support for :ref:`binary literals <binary-literals>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoBinaryLiterals"
         , flagSince = "7.10.1"
         }
  , flag { flagName = "-XCApiFFI"
         , flagDescription =
           "Enable :ref:`the CAPI calling convention <ffi-capi>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoCAPIFFI"
         , flagSince = "7.10.1"
         }
  , flag { flagName = "-XConstrainedClassMethods"
         , flagDescription =
           "Enable :ref:`constrained class methods <class-method-types>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoConstrainedClassMethods"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XConstraintKinds"
         , flagDescription =
           "Enable a :ref:`kind of constraints <constraint-kind>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoConstraintKinds"
         , flagSince = "7.4.1"
         }
  , flag { flagName = "-XCPP"
         , flagDescription =
           "Enable the :ref:`C preprocessor <c-pre-processor>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoCPP"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XDataKinds"
         , flagDescription = "Enable :ref:`datatype promotion <promotion>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoDataKinds"
         , flagSince = "7.4.1"
         }
  , flag { flagName = "-XDefaultSignatures"
         , flagDescription =
           "Enable :ref:`default signatures <class-default-signatures>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoDefaultSignatures"
         , flagSince = "7.2.1"
         }
  , flag { flagName = "-XDeriveAnyClass"
         , flagDescription =
           "Enable :ref:`deriving for any class <derive-any-class>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoDeriveAnyClass"
         , flagSince = "7.10.1"
         }
  , flag { flagName = "-XDeriveDataTypeable"
         , flagDescription =
           "Enable ``deriving`` for the :ref:`Data class "++
           "<deriving-typeable>`. Implied by :ghc-flag:`-XAutoDeriveTypeable`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoDeriveDataTypeable"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XDeriveFunctor"
         , flagDescription =
           "Enable :ref:`deriving for the Functor class <deriving-extra>`. "++
           "Implied by :ghc-flag:`-XDeriveTraversable`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoDeriveFunctor"
         , flagSince = "7.10.1"
         }
  , flag { flagName = "-XDeriveFoldable"
         , flagDescription =
           "Enable :ref:`deriving for the Foldable class <deriving-extra>`. "++
           "Implied by :ghc-flag:`-XDeriveTraversable`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoDeriveFoldable"
         , flagSince = "7.10.1"
         }
  , flag { flagName = "-XDeriveGeneric"
         , flagDescription =
           "Enable :ref:`deriving for the Generic class <deriving-typeable>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoDeriveGeneric"
         , flagSince = "7.2.1"
         }
  , flag { flagName = "-XDeriveGeneric"
         , flagDescription =
           "Enable :ref:`deriving for the Generic class <deriving-typeable>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoDeriveGeneric"
         , flagSince = "7.2.1"
         }
  , flag { flagName = "-XDeriveLift"
         , flagDescription =
           "Enable :ref:`deriving for the Lift class <deriving-lift>`"
         , flagType = DynamicFlag
         , flagReverse = "-XNoDeriveLift"
         , flagSince = "7.2.1"
         }
  , flag { flagName = "-XDeriveTraversable"
         , flagDescription =
           "Enable :ref:`deriving for the Traversable class <deriving-extra>`. "++
           "Implies :ghc-flag:`-XDeriveFunctor` and :ghc-flag:`-XDeriveFoldable`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoDeriveTraversable"
         , flagSince = "7.10.1"
         }
  , flag { flagName = "-XDerivingStrategies"
         , flagDescription =
           "Enables :ref:`deriving strategies <deriving-strategies>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoDerivingStrategies"
         , flagSince = "8.2.1"
         }
  , flag { flagName = "-XDisambiguateRecordFields"
         , flagDescription =
           "Enable :ref:`record field disambiguation <disambiguate-fields>`. "++
           "Implied by :ghc-flag:`-XRecordWildCards`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoDisambiguateRecordFields"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XEmptyCase"
         , flagDescription =
           "Allow :ref:`empty case alternatives <empty-case>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoEmptyCase"
         , flagSince = "7.8.1"
         }
  , flag { flagName = "-XEmptyDataDecls"
         , flagDescription = "Enable empty data declarations."
         , flagType = DynamicFlag
         , flagReverse = "-XNoEmptyDataDecls"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XExistentialQuantification"
         , flagDescription =
           "Enable :ref:`existential quantification <existential-quantification>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoExistentialQuantification"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XExplicitForAll"
         , flagDescription =
           "Enable :ref:`explicit universal quantification <explicit-foralls>`."++
           " Implied by :ghc-flag:`-XScopedTypeVariables`, :ghc-flag:`-XLiberalTypeSynonyms`,"++
           " :ghc-flag:`-XRankNTypes` and :ghc-flag:`-XExistentialQuantification`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoExplicitForAll"
         , flagSince = "6.12.1"
         }
  , flag { flagName = "-XExplicitNamespaces"
         , flagDescription =
           "Enable using the keyword ``type`` to specify the namespace of "++
           "entries in imports and exports (:ref:`explicit-namespaces`). "++
           "Implied by :ghc-flag:`-XTypeOperators` and :ghc-flag:`-XTypeFamilies`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoExplicitNamespaces"
         , flagSince = "7.6.1"
         }
  , flag { flagName = "-XExtendedDefaultRules"
         , flagDescription =
           "Use GHCi's :ref:`extended default rules <extended-default-rules>` "++
           "in a normal module."
         , flagType = DynamicFlag
         , flagReverse = "-XNoExtendedDefaultRules"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XFlexibleContexts"
         , flagDescription =
           "Enable :ref:`flexible contexts <flexible-contexts>`. Implied by "++
           ":ghc-flag:`-XImplicitParams`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoFlexibleContexts"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XFlexibleInstances"
         , flagDescription =
           "Enable :ref:`flexible instances <instance-rules>`. "++
           "Implies :ghc-flag:`-XTypeSynonymInstances`. "++
           "Implied by :ghc-flag:`-XImplicitParams`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoFlexibleInstances"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XForeignFunctionInterface"
         , flagDescription =
           "Enable :ref:`foreign function interface <ffi>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoForeignFunctionInterface"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XFunctionalDependencies"
         , flagDescription =
           "Enable :ref:`functional dependencies <functional-dependencies>`. "++
           "Implies :ghc-flag:`-XMultiParamTypeClasses`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoFunctionalDependencies"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XGADTs"
         , flagDescription =
           "Enable :ref:`generalised algebraic data types <gadt>`. "++
           "Implies :ghc-flag:`-XGADTSyntax` and :ghc-flag:`-XMonoLocalBinds`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoGADTs"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XGADTSyntax"
         , flagDescription =
           "Enable :ref:`generalised algebraic data type syntax <gadt-style>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoGADTSyntax"
         , flagSince = "7.2.1"
         }
  , flag { flagName = "-XGeneralizedNewtypeDeriving"
         , flagDescription =
           "Enable :ref:`newtype deriving <newtype-deriving>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoGeneralizedNewtypeDeriving"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XGenerics"
         , flagDescription =
           "Deprecated, does nothing. No longer enables "++
           ":ref:`generic classes <generic-classes>`. See also GHC's support "++
           "for :ref:`generic programming <generic-programming>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoGenerics"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XImplicitParams"
         , flagDescription =
           "Enable :ref:`Implicit Parameters <implicit-parameters>`. "++
           "Implies :ghc-flag:`-XFlexibleContexts` and :ghc-flag:`-XFlexibleInstances`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoImplicitParams"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XNoImplicitPrelude"
         , flagDescription =
           "Don't implicitly ``import Prelude``. "++
           "Implied by :ghc-flag:`-XRebindableSyntax`."
         , flagType = DynamicFlag
         , flagReverse = "-XImplicitPrelude"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XImpredicativeTypes"
         , flagDescription =
           "Enable :ref:`impredicative types <impredicative-polymorphism>`. "++
           "Implies :ghc-flag:`-XRankNTypes`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoImpredicativeTypes"
         , flagSince = "6.10.1"
         }
  , flag { flagName = "-XIncoherentInstances"
         , flagDescription =
           "Enable :ref:`incoherent instances <instance-overlap>`. "++
           "Implies :ghc-flag:`-XOverlappingInstances`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoIncoherentInstances"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XTypeFamilyDependencies"
         , flagDescription =
           "Enable :ref:`injective type families <injective-ty-fams>`. "++
           "Implies :ghc-flag:`-XTypeFamilies`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoTypeFamilyDependencies"
         , flagSince = "8.0.1"
         }
  , flag { flagName = "-XInstanceSigs"
         , flagDescription =
           "Enable :ref:`instance signatures <instance-sigs>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoInstanceSigs"
         , flagSince = "7.10.1"
         }
  , flag { flagName = "-XInterruptibleFFI"
         , flagDescription = "Enable interruptible FFI."
         , flagType = DynamicFlag
         , flagReverse = "-XNoInterruptibleFFI"
         , flagSince = "7.2.1"
         }
  , flag { flagName = "-XKindSignatures"
         , flagDescription =
           "Enable :ref:`kind signatures <kinding>`. "++
           "Implied by :ghc-flag:`-XTypeFamilies` and :ghc-flag:`-XPolyKinds`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoKindSignatures"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XLambdaCase"
         , flagDescription =
           "Enable :ref:`lambda-case expressions <lambda-case>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoLambdaCase"
         , flagSince = "7.6.1"
         }
  , flag { flagName = "-XLiberalTypeSynonyms"
         , flagDescription =
           "Enable :ref:`liberalised type synonyms <type-synonyms>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoLiberalTypeSynonyms"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XMagicHash"
         , flagDescription =
           "Allow ``#`` as a :ref:`postfix modifier on identifiers <magic-hash>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoMagicHash"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XMonadComprehensions"
         , flagDescription =
           "Enable :ref:`monad comprehensions <monad-comprehensions>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoMonadComprehensions"
         , flagSince = "7.2.1"
         }
  , flag { flagName = "-XMonoLocalBinds"
         , flagDescription =
           "Enable :ref:`do not generalise local bindings <mono-local-binds>`. "++
           "Implied by :ghc-flag:`-XTypeFamilies` and :ghc-flag:`-XGADTs`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoMonoLocalBinds"
         , flagSince = "6.12.1"
         }
  , flag { flagName = "-XNoMonomorphismRestriction"
         , flagDescription =
           "Disable the :ref:`monomorphism restriction <monomorphism>`."
         , flagType = DynamicFlag
         , flagReverse = "-XMonomorphismRestriction"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XMultiParamTypeClasses"
         , flagDescription =
           "Enable :ref:`multi parameter type classes "++
           "<multi-param-type-classes>`. Implied by "++
           ":ghc-flag:`-XFunctionalDependencies`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoMultiParamTypeClasses"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XMultiWayIf"
         , flagDescription =
           "Enable :ref:`multi-way if-expressions <multi-way-if>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoMultiWayIf"
         , flagSince = "7.6.1"
         }
  , flag { flagName = "-XNamedFieldPuns"
         , flagDescription = "Enable :ref:`record puns <record-puns>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoNamedFieldPuns"
         , flagSince = "6.10.1"
         }
  , flag { flagName = "-XNamedWildCards"
         , flagDescription = "Enable :ref:`named wildcards <named-wildcards>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoNamedWildCards"
         , flagSince = "7.10.1"
         }
  , flag { flagName = "-XNegativeLiterals"
         , flagDescription =
           "Enable support for :ref:`negative literals <negative-literals>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoNegativeLiterals"
         , flagSince = "7.8.1"
         }
  , flag { flagName = "-XNPlusKPatterns"
         , flagDescription = "Enable support for ``n+k`` patterns. "++
           "Implied by :ghc-flag:`-XHaskell98`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoNPlusKPatterns"
         , flagSince = "6.12.1"
         }
  , flag { flagName = "-XNullaryTypeClasses"
         , flagDescription =
           "Deprecated, does nothing. :ref:`nullary (no parameter) type "++
           "classes <nullary-type-classes>` are now enabled using "++
           ":ghc-flag:`-XMultiParamTypeClasses`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoNullaryTypeClasses"
         , flagSince = "7.8.1"
         }
  , flag { flagName = "-XNumDecimals"
         , flagDescription =
           "Enable support for 'fractional' integer literals."
         , flagType = DynamicFlag
         , flagReverse = "-XNoNumDecimals"
         , flagSince = "7.8.1"
         }
  , flag { flagName = "-XOverlappingInstances"
         , flagDescription =
           "Enable :ref:`overlapping instances <instance-overlap>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoOverlappingInstances"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XOverloadedLists"
         , flagDescription =
           "Enable :ref:`overloaded lists <overloaded-lists>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoOverloadedLists"
         , flagSince = "7.8.1"
         }
  , flag { flagName = "-XOverloadedStrings"
         , flagDescription =
           "Enable :ref:`overloaded string literals <overloaded-strings>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoOverloadedStrings"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XPackageImports"
         , flagDescription =
           "Enable :ref:`package-qualified imports <package-imports>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoPackageImports"
         , flagSince = "6.10.1"
         }
  , flag { flagName = "-XParallelArrays"
         , flagDescription =
           "Enable parallel arrays. Implies :ghc-flag:`-XParallelListComp`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoParallelArrays"
         , flagSince = "7.4.1"
         }
  , flag { flagName = "-XParallelListComp"
         , flagDescription =
           "Enable :ref:`parallel list comprehensions "++
           "<parallel-list-comprehensions>`. "++
           "Implied by :ghc-flag:`-XParallelArrays`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoParallelListComp"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XPartialTypeSignatures"
         , flagDescription =
           "Enable :ref:`partial type signatures <partial-type-signatures>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoPartialTypeSignatures"
         , flagSince = "7.10.1"
         }
  , flag { flagName = "-XNoPatternGuards"
         , flagDescription = "Disable :ref:`pattern guards <pattern-guards>`. "++
           "Implied by :ghc-flag:`-XHaskell98`."
         , flagType = DynamicFlag
         , flagReverse = "-XPatternGuards"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XPatternSynonyms"
         , flagDescription =
           "Enable :ref:`pattern synonyms <pattern-synonyms>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoPatternSynonyms"
         , flagSince = "7.10.1"
         }
  , flag { flagName = "-XPolyKinds"
         , flagDescription =
           "Enable :ref:`kind polymorphism <kind-polymorphism>`. "++
           "Implies :ghc-flag:`-XKindSignatures`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoPolyKinds"
         , flagSince = "7.4.1"
         }
  , flag { flagName = "-XPolymorphicComponents"
         , flagDescription =
           "Enable :ref:`polymorphic components for data constructors "++
           "<universal-quantification>`. Synonym for :ghc-flag:`-XRankNTypes`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoPolymorphicComponents"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XPostfixOperators"
         , flagDescription =
           "Enable :ref:`postfix operators <postfix-operators>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoPostfixOperators"
         , flagSince = "7.10.1"
         }
  , flag { flagName = "-XQuasiQuotes"
         , flagDescription = "Enable :ref:`quasiquotation <th-quasiquotation>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoQuasiQuotes"
         , flagSince = "6.10.1"
         }
  , flag { flagName = "-XRank2Types"
         , flagDescription =
           "Enable :ref:`rank-2 types <universal-quantification>`. "++
           "Synonym for :ghc-flag:`-XRankNTypes`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoRank2Types"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XRankNTypes"
         , flagDescription =
           "Enable :ref:`rank-N types <universal-quantification>`. "++
           "Implied by :ghc-flag:`-XImpredicativeTypes`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoRankNTypes"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XRebindableSyntax"
         , flagDescription =
           "Employ :ref:`rebindable syntax <rebindable-syntax>`. "++
           "Implies :ghc-flag:`-XNoImplicitPrelude`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoRebindableSyntax"
         , flagSince = "7.0.1"
         }
  , flag { flagName = "-XRecordWildCards"
         , flagDescription =
           "Enable :ref:`record wildcards <record-wildcards>`. "++
           "Implies :ghc-flag:`-XDisambiguateRecordFields`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoRecordWildCards"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XRecursiveDo"
         , flagDescription =
           "Enable :ref:`recursive do (mdo) notation <recursive-do-notation>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoRecursiveDo"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XRoleAnnotations"
         , flagDescription =
           "Enable :ref:`role annotations <role-annotations>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoRoleAnnotations"
         , flagSince = "7.10.1"
         }
  , flag { flagName = "-XSafe"
         , flagDescription =
           "Enable the :ref:`Safe Haskell <safe-haskell>` Safe mode."
         , flagType = DynamicFlag
         , flagSince = "7.2.1"
         }
  , flag { flagName = "-XScopedTypeVariables"
         , flagDescription =
           "Enable :ref:`lexically-scoped type variables "++
           "<scoped-type-variables>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoScopedTypeVariables"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XStandaloneDeriving"
         , flagDescription =
           "Enable :ref:`standalone deriving <stand-alone-deriving>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoStandaloneDeriving"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XStaticPointers"
         , flagDescription =
           "Enable :ref:`static pointers <static-pointers>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoStaticPointers"
         , flagSince = "7.10.1"
         }
  , flag { flagName = "-XStrictData"
         , flagDescription =
           "Enable :ref:`default strict datatype fields <strict-data>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoStrictData"
         }
  , flag { flagName = "-XTemplateHaskell"
         , flagDescription =
           "Enable :ref:`Template Haskell <template-haskell>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoTemplateHaskell"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XTemplateHaskellQuotes"
         , flagDescription = "Enable quotation subset of "++
                             ":ref:`Template Haskell <template-haskell>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoTemplateHaskellQuotes"
         , flagSince = "8.0.1"
         }
  , flag { flagName = "-XNoTraditionalRecordSyntax"
         , flagDescription =
           "Disable support for traditional record syntax "++
           "(as supported by Haskell 98) ``C {f = x}``"
         , flagType = DynamicFlag
         , flagReverse = "-XTraditionalRecordSyntax"
         , flagSince = "7.4.1"
         }
  , flag { flagName = "-XTransformListComp"
         , flagDescription =
           "Enable :ref:`generalised list comprehensions "++
           "<generalised-list-comprehensions>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoTransformListComp"
         , flagSince = "6.10.1"
         }
  , flag { flagName = "-XTrustworthy"
         , flagDescription =
           "Enable the :ref:`Safe Haskell <safe-haskell>` Trustworthy mode."
         , flagType = DynamicFlag
         , flagSince = "7.2.1"
         }
  , flag { flagName = "-XTupleSections"
         , flagDescription = "Enable :ref:`tuple sections <tuple-sections>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoTupleSections"
         , flagSince = "7.10.1"
         }
  , flag { flagName = "-XTypeFamilies"
         , flagDescription =
           "Enable :ref:`type families <type-families>`. "++
           "Implies :ghc-flag:`-XExplicitNamespaces`, :ghc-flag:`-XKindSignatures`, "++
           "and :ghc-flag:`-XMonoLocalBinds`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoTypeFamilies"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XTypeOperators"
         , flagDescription =
           "Enable :ref:`type operators <type-operators>`. "++
           "Implies :ghc-flag:`-XExplicitNamespaces`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoTypeOperators"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XTypeSynonymInstances"
         , flagDescription =
           "Enable :ref:`type synonyms in instance heads "++
           "<flexible-instance-head>`. Implied by :ghc-flag:`-XFlexibleInstances`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoTypeSynonymInstances"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XUnboxedTuples"
         , flagDescription = "Enable :ref:`unboxed tuples <unboxed-tuples>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoUnboxedTuples"
         , flagSince = "6.8.1"
         }
  , flag { flagName ="-XUnboxedSums"
         , flagDescription = "Enable :ref: `unboxed sums <unboxed-sums>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoUnboxedSums"
         , flagSince = "8.2.1"
         }
  , flag { flagName = "-XUndecidableInstances"
         , flagDescription =
           "Enable :ref:`undecidable instances <undecidable-instances>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoUndecidableInstances"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XUnicodeSyntax"
         , flagDescription = "Enable :ref:`unicode syntax <unicode-syntax>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoUnicodeSyntax"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XUnliftedFFITypes"
         , flagDescription = "Enable unlifted FFI types."
         , flagType = DynamicFlag
         , flagReverse = "-XNoUnliftedFFITypes"
         , flagSince = "6.8.1"
         }
  , flag { flagName = "-XUnsafe"
         , flagDescription =
           "Enable :ref:`Safe Haskell <safe-haskell>` Unsafe mode."
         , flagType = DynamicFlag
         , flagSince = "7.4.1"
         }
  , flag { flagName = "-XViewPatterns"
         , flagDescription = "Enable :ref:`view patterns <view-patterns>`."
         , flagType = DynamicFlag
         , flagReverse = "-XNoViewPatterns"
         , flagSince = "6.10.1"
         }
  ]
