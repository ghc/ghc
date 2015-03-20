# Changelog for [`template-haskell` package](http://hackage.haskell.org/package/template-haskell)

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
