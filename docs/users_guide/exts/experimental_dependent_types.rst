.. _experimental-dependent-types:

Experimental dependent types
============================

.. extension:: ExperimentalDependentTypes
    :shortdesc: Check restricted retained Boolean quantification.

    :since: 10.1
    :status: Experimental

    Enable retained Boolean quantification for an internal experiment.

This flag is local to the retained Boolean experiment.
This extension is disabled by default. It enables ``foreach`` in types and
checks a restricted subset of explicit value signatures. The experiment uses a
native dependent Core representation. Ordinary module compilation and interface
serialization remain unsupported. The regression harness executes checked local
declarations through a separate native bytecode entry.

The syntax retains a value argument whose identity can occur in the
result type. For example::

    {-# LANGUAGE ExperimentalDependentTypes #-}

    keep :: foreach (b :: Bool) -> Select b -> Select b
    keep b x = x

Here ``Select`` denotes a type family indexed by ``Bool``. The argument ``b`` is
an ordinary runtime Boolean whose identity can occur in the result type.
Applications retain that value argument. They accept Boolean constructors or
named local Boolean variables. They do not construct singleton witnesses or
evaluate an index to determine its type.

The current subset requires one annotated Boolean binder at the start of a
complete monomorphic value signature. A dependent function must have one equation
with a bare retained variable pattern. Its result must have lifted kind, and checking
the remaining arguments must require an identity result wrapper. Ordinary rank
restrictions still apply. Index-dependent constraints are limited to reflexive
Boolean equalities. Quick Look inference and optimisation pragmas for retained
functions remain restricted. Dedicated rejection checks report GHC-83605.
Ordinary payload mismatches use the existing type error. Unsupported type
positions report GHC-43582.

Dependent source constructor patterns and Boolean case refinement remain
unimplemented. In particular, source ``choose`` is not supported yet.

The extension implies no other extensions. In particular, it does not imply
:extension:`KindSignatures`. The ``:: Bool`` annotation specifies the
type of a value binder. It does not make that binder a type variable.

When the extension is disabled, ``foreach`` remains an ordinary identifier::

    foreach :: a -> a
    foreach x = x

When the extension is enabled, ``foreach`` is a reserved keyword. Programs that
use it as an identifier must leave the extension disabled. The extension does
not enable general dependent Haskell, native dependent interfaces, or the
ordinary optimizer for dependent Core.
