# Changelog for [`template-haskell` package](http://hackage.haskell.org/package/template-haskell)

## 2.14.0.0 *August 2018*

  * Introduce an `addForeignFilePath` function, as well as a corresponding
    `qAddForeignFile` class method to `Quasi`. Unlike `addForeignFile`, which
    takes the contents of the file as an argument, `addForeignFilePath` takes
    as an argument a path pointing to a foreign file. A new `addForeignSource`
    function has also been added which takes a file's contents as an argument.

    The old `addForeignFile` function is now deprecated in favor of
    `addForeignSource`, and the `qAddForeignFile` method of `Quasi` has been
    removed entirely.

  * Introduce an `addTempFile` function, as well as a corresponding
    `qAddTempFile` method to `Quasi`, which requests a temporary file of
    a given suffix.

  * Add a `ViaStrategy` constructor to `DerivStrategy`.

## 2.13.0.0 *March 2018*

  * Bundled with GHC 8.4.1

  * `Language.Haskell.TH.FamFlavour`, which was deprecated in 2.11,
    has been removed.

  * Add support for overloaded labels. Introduces `labelE :: String -> ExpQ`.

  * Add `KindQ`, `TyVarBndrQ`, and `FamilyResultSigQ` aliases to
    `Language.Haskell.TH.Lib`.

  * Add `Language.Haskell.TH.Lib.Internal` module, which exposes some
    additional functionality that is used internally in GHC's integration
    with Template Haskell. This is not a part of the public API, and as
    such, there are no API guarantees for this module from version to version.

  * `MonadIO` is now a superclass of `Quasi`, `qRunIO` has a default
    implementation `qRunIO = liftIO`

  * Add `MonadIO Q` instance

## 2.12.0.0 *July 2017*

  * Bundled with GHC 8.2.1

  * Add support for pattern synonyms. This introduces one new constructor to
    `Info` (`PatSynI`), two new constructors to `Dec` (`PatSynD` and
    `PatSynSigD`), and two new data types (`PatSynDir` and `PatSynArgs`),
    among other changes. (#8761)

  * Add support for unboxed sums. (#12478)

  * Add support for visible type applications. (#12530)

  * Add support for attaching deriving strategies to `deriving` statements
    (#10598)

  * Add support for `COMPLETE` pragmas. (#13098)

  * `unboxedTupleTypeName` and `unboxedTupleDataName` now work for unboxed
    0-tuples and 1-tuples (#12977)

  * `Language.Haskell.TH` now reexports all of `Language.Haskell.TH.Lib`.
    (#12992). This causes `Language.Haskell.TH` to export more types and
    functions that it did before:
    - `TExp`, `BangQ`, and `FieldExpQ`
    - `unboxedTupP`, `unboxedTupE` and `unboundVarE`
    - `infixLD`, `infixRD`, and `infixND`
    - `unboxedTupleT` and `wildCardT`
    - `plainTV` and `kindedTV`
    - `interruptible` and `funDep`
    - `valueAnnotation`, `typeAnnotation`, and `moduleAnnotation`

  * Add support for overloaded labels.

## 2.11.0.0  *May 2016*

  * Bundled with GHC 8.0.1

  * The compiler can now resolve infix operator fixities in types on its own.
    The `UInfixT` constructor of `Type` is analoguous to `UInfixE` for expressions
    and can contain a tree of infix type applications which will be reassociated
    according to the fixities of the operators. The `ParensT` constructor can be
    used to explicitly group expressions.

  * Add `namePackage` and `nameSpace`

  * Make `dataToQa` and `dataToExpQ` able to handle `Data` instances whose
    `toConstr` implementation relies on a function instead of a data
    constructor (#10796)

  * Add `Show` instances for `NameFlavour` and `NameSpace`

  * Remove `FamilyD` and `FamFlavour`.  Add `DataFamilyD` and `OpenTypeFamilyD`
    as the representation of data families and open type families
    respectively. (#6018)

  * Add `TypeFamilyHead` for common elements of `OpenTypeFamilyD` and
    `ClosedTypeFamilyD` (#10902)

  * The `Strict` datatype was split among different datatypes: three for
    writing the strictness information of data constructors' fields as denoted
    in Haskell source code (`SourceUnpackedness` and `SourceStrictness`, as
    well as `Bang`), and one for strictness information after a constructor is
    compiled (`DecidedStrictness`). `Strict`, `StrictType` and `VarStrictType`
    have been deprecated in favor of `Bang`, `BangType` and `VarBangType`.
    (#10697)

  * Add `reifyConStrictness` to query a data constructor's `DecidedStrictness`
    values for its fields (#10697)

  * The `ClassOpI`, `DataConI`, and `VarI` constructors no longer have a
    `Fixity` field. Instead, all `Fixity` information for a given `Name` is
    now determined through the `reifyFixity` function, which returns `Just` the
    fixity if there is an explicit fixity declaration for that `Name`, and
    `Nothing` otherwise (#10704 and #11345)

  * Add `MonadFail Q` instance for GHC 8.0 and later (#11661)

  * Add support for OVERLAP(S/PED/PING) pragmas on instances


## 2.10.0.0  *Mar 2015*

  * Bundled with GHC 7.10.1
  * Remove build-dependency on `containers` package
  * Make `Pred` a type synonym of `Type`, and deprecate `classP`/`equalP` (#7021)
  * Add support for `LINE` pragma via `prageLineD` and `LineP`
  * Replace `Int#` with `!Int` in `NameFlavour` constructors
  * Derive `Generic` for TH types (#9527)
  * Add `standaloneDerivD` (#8100)
  * Add support for generic default signatures via `defaultSigD` (#9064)
  * Add `Lift` instances for `()` and `Rational`
  * Derive new `Show` and `Data` instances for `Loc`
  * Derive `Eq` instances for `Loc`, `Info`, and `ModuleInfo`
  * Make calling conventions available in template haskell consistent
    with those from GHC (#9703)
  * Add support for `-XStaticValues` via `staticE`
  * Add `Ord` instances to TH types
  * Merge some instances from `th-orphans` (`Ppr` instances for `Lit`
    and `Loc` as well as `Lift` instances for numeric types
  * Put parens around `(ty :: kind)` when pretty-printing TH syntax
