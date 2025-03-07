Named ``default`` declarations
==============================

.. extension:: NamedDefaults
    :shortdesc:
      Enable ``default`` declarations with explicitly named class,
      extending :ref:`class_defaulting`.

The ``NamedDefaults`` extension extends the type-class defaulting mechanism
described in :ref:`class_defaulting`, allowing default types to be specified
on a per-class basis, and to be imported and exported.

Motivation
----------

Haskell 2010 `language report
<https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-790004.3.4>`__
includes rarely used ``default`` declarations. Their primary purpose is to
improve the ergonomics of numeric literals in simple programs: ::

    main = print (6 * 7)

The type of the right-hand side of this equation is ``(Num a, Show a) => IO
()``. There is nothing to tell the compiler which concrete type to instantiate
for ``a``. To resolve this ambiguity, the user can declare something like ::

    default (Integer, Int, Double)

to specify that any ambiguous type variable (such as ``a`` in the above example)
constrained to class ``Num`` should default to ``Integer``. If that type doesn't
satisfy the remaining constraints required from the context (such as ``Show a``
above), the other specified types (e.g. ``Int`` and ``Double``) are tried in
turn.

The language report applies the ``default`` declarations only to the ambiguities
involving the ``Num`` class. The reason for this limitation is that the numeric
literals are the only literals with ambiguous types in the language
report. Since then, however, the :extension:`OverloadedStrings` and
:extension:`OverloadedLists` language extensions have made more syntactic
constructs ambiguous. The former in particular is commonly used for more
convenient coding of ``Text`` literals. Another potential source of ambiguity
are the Prelude and the core libraries which are slowly evolving to be more
generalized.

Specifying the class
--------------------

The Haskell 2010 language report specifies the following syntax for the default
declaration:

|    \ *topdecl* → ``default`` (*qtycon*\ `1`:subscript: , … , *qtycon*\ `n`:subscript:) (n ≥ 0)

where each type *qtycon*\ `i`:subscript: must be an instance of class ``Num``.

With the :extension:`NamedDefaults` language extension, GHC generalizes these
declarations so the user can specify the default types for any class rather than
just ``Num``:

|    \ *topdecl* → ``default`` *qtycls*? (*qtycon*\ `1`:subscript: , … , *qtycon*\ `n`:subscript:) (n ≥ 0)

where each type *qtycon*\ `i`:subscript: must be an instance of the specified
class *qtycls*. The types may belong to any kind, but the class must have a
single parameter. This declaration for example becomes legal: ::

    default Monoid ([Int])

and in turn allows this program to compile: ::

    main = print mempty

If no class is specified, the earlier default of ``Num`` is assumed. In other
words, the Haskell '98 syntax of::

    default (Int, Float)

would mean exactly the same as::

    default Num (Int, Float)

Exporting the defaults
----------------------

A ``default`` declaration by itself applies only within the module it's in. The
:extension:`NamedDefaults` extension also extends the syntax of module exports
to permit a new form of export item: ::

    module MyModule (default Monoid)

which exports the default that's in effect in the module for the named
class. This can mean either that it's declared in the same module or that it's
imported from another module.

When exporting a ``default Num`` declaration, the class ``Num`` has to be
explicitly named like any other class.

A module with no explicit export list (as in ``module M where {...}``) exports
all defaults declared in the module. Re-export of a whole imported module (as in
``module M (module N) where{...}``) does *not* export any defaults.

While default exports must be made explicit, their imports are automatic
and implicit. To suppress or modify an imported default, a module can declare
its own; a local ``default`` declaration will override all imported defaults for
the same class.

Definition of subsumption
-------------------------

Given two ``default`` declarations for the same class
   
   |      ``default`` *C*  (*Type*\ `1`:subscript:\ `a`:superscript: , … , *Type*\ `m`:subscript:\ `a`:superscript:)
   |      ``default`` *C*  (*Type*\ `1`:subscript:\ `b`:superscript: , … , *Type*\ `n`:subscript:\ `b`:superscript:)

if *m* ≤ *n* and the first type sequence *Type*\ `1`:subscript:\
`a`:superscript: , … , *Type*\ `m`:subscript:\ `a`:superscript: is a
sub-sequence of the second sequence *Type*\ `1`:subscript:\ `b`:superscript: , …
, *Type*\ `n`:subscript:\ `b`:superscript: (*i.e.*, the former can be obtained
by removing a number of *Type*\ `i`:subscript:\ `b`:superscript: items from the
latter), we say that the second declaration *subsumes* the first one.


Rules for disambiguation of multiple declarations
-------------------------------------------------

Only a single ``default`` declaration can be in effect in any single module for
any particular class. If there is more than one ``default`` declaration in
scope, the conflict is resolved using the following rules:

1. Two declarations for two different classes are not considered to be in
   conflict; they can, however, clash at a particular use site as we'll see in
   the following section.
2. Two declarations for the same class explicitly declared in the same module
   are considered a static error.
3. A ``default`` declaration in a module takes precedence over any imported
   ``default`` declarations for the same class. However the compiler may issue
   a warning (enabled by :ghc-flag:`-Wtype-defaults`) if an imported declaration
   is not subsumed by the local declaration.
4. For any two imported ``default`` declarations for the same class where one
   subsumes the other, we ignore the subsumed declaration.
5. If a class has neither a local ``default`` declaration nor an imported
   ``default`` declaration that subsumes all other imported ``default``
   declarations for the class, the conflict between the imports is
   unresolvable. The effect is to ignore all ``default`` declarations for the
   class, so that no declaration is in effect in the module. The compiler may
   emit a warning in this case, if enabled by :ghc-flag:`-Wtype-defaults`, but
   no error would be triggered about the imports. Of course an error may be
   triggered in the body of the module if it contains an actual ambiguous type
   for the class with the conflicting imported defaults, as per the following
   subsection.

As a result, in any module each class has either one default declaration in
scope (a locally-declared one, or an imported one that subsumes all other
imported ones), or none. This single default is used to resolve ambiguity, as
described in the next subsection.

Note that a ``default`` declaration that repeats a type name more than once is
perfectly valid, and sometimes may be necessary to resolve coflicts. For
example, a module that imports two conflicting defaults

::

   default C (Int, Bool)

and
   
::

   default C (Bool, Int)

may use a local declaration

::

   default C (Int, Bool, Int)

to override the imports. Because this declaration subsumes both imported
defaults it will not trigger any compiler warning. When used to resolve
ambiguity (next section) it behaves exactly like ``default C( Int, Bool)``; that
is, the repeats can be discarded.

   
Rules for disambiguation at the use site
----------------------------------------

The disambiguation rules are a conservative extension of the existing rules in
Haskell 2010, which state that ambiguous type variable *v* is defaultable if:

    - *v* appears only in constraints of the form *C* *v*, where *C* is a class,
      and

    - at least one of these classes is a numeric class, (that is, ``Num`` or a
      subclass of ``Num``), and

    - all of these classes are defined in the Prelude or a standard library.

    Each defaultable variable is replaced by the first type in the default list
    that is an instance of all the ambiguous variable’s classes. It is a static
    error if no such type is found.

The new rules instead require only that 

- *v* appears in at least one constraint of the form *C* *v*, where *C* is a
  single-parameter class.

Informally speaking, the type selected for defaulting is the first type from the
``default`` list for class *C* that satisfies all constraints on type variable
*v*. If there are multiple *C*\ `i`:subscript: *v* constraints with competing
``default`` declarations, they have to resolve to the same type.

To make the design more explicit, the following algorithm *can* be used for
default resolution, but any other method that achieves the same effect can be
substitued:

Let *S* be the complete set of unsolved constraints, and initialize *S*\
`x`:subscript: to an empty set of constraints.  For every *v* that is free in
*S*:

1. Define *C*\ `v`:subscript: = { *C*\ `i`:subscript: v | *C*\ `i`:subscript: v
   ∈ *S* }, the subset of *S* consisting of all constraints in *S* of form (*C*\
   `i`:subscript: v), where *C*\ `i`:subscript: is a single-parameter type class.
2. Define *D*\ `v`:subscript:, by extending *C*\ `v`:subscript: with the
   superclasses of every *C*\ `i`:subscript: in *C*\ `v`:subscript:
3. Define *E*\ `v`:subscript:, by filtering *D*\ `v`:subscript: to contain only
   classes with a default declaration.
4. For each *C*\ `i`:subscript: in *E*\ `v`:subscript:, find the first type *T*
   in the default list for *C*\ `i`:subscript: for which, for every (*C*\
   `i`:subscript: v) in *C*\ `v`:subscript:, the constraint (*C*\ `i`:subscript:
   *T*) is soluble.
5. If there is precisely one type *T* in the resulting type set, resolve the
   ambiguity by adding a ``v ~ T``\ `i`:subscript: constraint to a set *S*\
   `x`:subscript:; otherwise report a static error.
