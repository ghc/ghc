module Settings.Builders.GenPrimopCode (genPrimopCodeBuilderArgs) where

import Settings.Builders.Common

genPrimopCodeBuilderArgs :: Args
genPrimopCodeBuilderArgs = builder GenPrimopCode ? mconcat
    [ output "//PrimopWrappers.hs"                 ? arg "--make-haskell-wrappers"
    , output "//Prim.hs"                           ? arg "--make-haskell-source"
    , output "//primop-data-decl.hs-incl"          ? arg "--data-decl"
    , output "//primop-tag.hs-incl"                ? arg "--primop-tag"
    , output "//primop-list.hs-incl"               ? arg "--primop-list"
    , output "//primop-effects.hs-incl"            ? arg "--primop-effects"
    , output "//primop-out-of-line.hs-incl"        ? arg "--out-of-line"
    , output "//primop-commutable.hs-incl"         ? arg "--commutable"
    , output "//primop-code-size.hs-incl"          ? arg "--code-size"
    , output "//primop-strictness.hs-incl"         ? arg "--strictness"
    , output "//primop-is-exposed.hs-incl"         ? arg "--is-exposed"
    , output "//primop-is-work-free.hs-incl"       ? arg "--is-work-free"
    , output "//primop-is-cheap.hs-incl"           ? arg "--is-cheap"
    , output "//primop-fixity.hs-incl"             ? arg "--fixity"
    , output "//primop-primop-info.hs-incl"        ? arg "--primop-primop-info"
    , output "//primop-vector-uniques.hs-incl"     ? arg "--primop-vector-uniques"
    , output "//primop-vector-tys.hs-incl"         ? arg "--primop-vector-tys"
    , output "//primop-vector-tys-exports.hs-incl" ? arg "--primop-vector-tys-exports"
    , output "//primop-vector-tycons.hs-incl"      ? arg "--primop-vector-tycons"
    , output "//primop-docs.hs-incl"               ? arg "--wired-in-docs"
    , output "//primop-deprecations.hs-incl"       ? arg "--wired-in-deprecations"
    , output "//primop-usage.hs-incl"              ? arg "--usage" ]
