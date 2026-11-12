.. _experimental-dependent-types:

Experimental dependent types
============================

.. extension:: ExperimentalDependentTypes
    :shortdesc: Reserve retained dependent quantification syntax.

    :since: 10.1
    :status: Experimental

    Parse retained dependent quantification syntax for an internal experiment.

This flag is local to the retained Boolean experiment.
This extension is disabled by default. It enables the parser to recognise
``foreach`` in types. The renamer rejects each such type with
``Typechecking of retained quantification is not implemented yet.`` The extension
does not provide a working source language for dependent functions.

The proposed syntax retains a value argument whose identity can occur in the
result type. For example::

    {-# LANGUAGE ExperimentalDependentTypes #-}

    choose :: foreach (b :: Bool) -> Select b
    keep   :: foreach (b :: Bool) -> Select b -> Select b

These signatures describe the intended experiment. They are not accepted by
the renamer. No variable is bound by retained quantification in this increment.
``Select`` would denote a type family indexed by ``Bool``. The intended argument
``b`` is an ordinary runtime Boolean. It is not an erased type argument or a
singleton witness. Checking, application, and constructor refinement in source
remain unimplemented.

The extension implies no other extensions. In particular, it does not imply
:extension:`KindSignatures`. The proposed ``:: Bool`` annotation specifies the
type of a value binder. It does not make that binder a type variable.

When the extension is disabled, ``foreach`` remains an ordinary identifier::

    foreach :: a -> a
    foreach x = x

When the extension is enabled, ``foreach`` is a reserved keyword. Programs that
use it as an identifier must leave the extension disabled. The extension does
not enable general dependent Haskell, native dependent interfaces, or the
ordinary optimizer for dependent Core.
