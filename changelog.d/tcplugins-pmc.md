section: compiler
issues: #26395
mrs: !14797
synopsis:
  Typechecker plugins are now run during pattern-match checking
description:
  Typechecker plugins (and defaulting plugins) are now run during pattern-match
  checking, which significantly improves pattern-match warnings for programs
  that rely on typechecking plugins in order to typecheck (e.g. dealing with
  GADTs indexed by natural numbers and using natural number arithmetic).
