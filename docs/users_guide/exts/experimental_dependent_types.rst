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

Constructor equations can specialise the result type::

    choose :: foreach (b :: Bool) -> Select b
    choose True = 42
    choose False = False

The checker substitutes the constructor into later argument types and the
result type. A consumer can therefore receive the selected value::

    observe :: foreach (b :: Bool) -> Select b -> String
    observe True n = show (n :: Int)
    observe False p = if p then "True" else "False"

The current subset requires one annotated Boolean binder at the start of a
complete monomorphic value signature. A dependent function uses one equation
with a bare retained variable pattern, or one unguarded equation for each Boolean
constructor. Arguments after a constructor pattern require bare variable patterns
and unrestricted multiplicities. The result must have lifted kind, and the outer
match wrapper must be identity. Ordinary rank restrictions still apply.
Index-dependent constraints are limited to reflexive
Boolean equalities. Quick Look inference and optimisation pragmas for retained
functions remain restricted. Dedicated rejection checks report GHC-83605.
Ordinary payload mismatches use the existing type error. Unsupported type
positions report GHC-43582.

Refinement in source ``case`` expressions remains unimplemented. The constructor
rule currently applies to function equations.

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
