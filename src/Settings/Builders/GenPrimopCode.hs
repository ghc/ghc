module Settings.Builders.GenPrimopCode (genPrimopCodeArgs) where

import Expression
import Predicates (builder, file)

-- Stdin/stdout are handled in a special way. See Rules/Actions.hs.
-- TODO: Do we want to keep "--usage"? It seems to be unused.
genPrimopCodeArgs :: Args
genPrimopCodeArgs = builder GenPrimopCode ? mconcat
    [ file "//PrimopWrappers.hs"                 ? arg "--make-haskell-wrappers"
    , file "//Prim.hs"                           ? arg "--make-haskell-source"
    , file "//primop-data-decl.hs-incl"          ? arg "--data-decl"
    , file "//primop-tag.hs-incl"                ? arg "--primop-tag"
    , file "//primop-list.hs-incl"               ? arg "--primop-list"
    , file "//primop-has-side-effects.hs-incl"   ? arg "--has-side-effects"
    , file "//primop-out-of-line.hs-incl"        ? arg "--out-of-line"
    , file "//primop-commutable.hs-incl"         ? arg "--commutable"
    , file "//primop-code-size.hs-incl"          ? arg "--code-size"
    , file "//primop-can-fail.hs-incl"           ? arg "--can-fail"
    , file "//primop-strictness.hs-incl"         ? arg "--strictness"
    , file "//primop-fixity.hs-incl"             ? arg "--fixity"
    , file "//primop-primop-info.hs-incl"        ? arg "--primop-primop-info"
    , file "//primop-vector-uniques.hs-incl"     ? arg "--primop-vector-uniques"
    , file "//primop-vector-tys.hs-incl"         ? arg "--primop-vector-tys"
    , file "//primop-vector-tys-exports.hs-incl" ? arg "--primop-vector-tys-exports"
    , file "//primop-vector-tycons.hs-incl"      ? arg "--primop-vector-tycons"
    , file "//primop-usage.hs-incl"              ? arg "--usage" ]
