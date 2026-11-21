# R15. Closed term evaluator

A closed term contains no free variables. Serokell's
example uses the value of `length [1,2,3]` to check a three-element vector.
The card proposes executing the function as bytecode or native code during
compilation. It is a subtask of R13, function promotion. It has no outgoing
dependency on the upstream graph. [Serokell R15](https://ghc.serokell.io/dh#closed-term-evaluator)

## First source example

```haskell
v :: Vec (length [1,2,3]) Bool
v = True :. False :. True :. VNil
```

The producer defines ordinary `length :: [Natural] -> Natural` using
`foldr`. The consumer imports this function unqualified and enables
`RequiredTypeArguments`. GHC resolves `length` to that import and
checks the three-element list at kind `[Natural]`. The vector index uses
GHC's built-in `Nat`. Prelude's `length` returns `Int`. The dependent extension is off in
the producer. The consumer has access to its interface and object file.
[Producer](../examples/closed-evaluation/ClosedLengthProducer.hs)
and [consumer](../examples/closed-evaluation/Client.hs).

| Check | Observed on the fork |
| --- | --- |
| Execute the typed call | The GHC API returns `3` with each interpreter (internal and external) and each producer format (bytecode and object). The probe checks the actual interpreter and the producer dependency reported by the compiler. |
| Observe a change invisible to the producer ABI | Changing the fold seed from zero to one changes the object bytes and preserves the ABI hash at `-O0`. The same probe executable returns `4` with both interpreters in fresh sessions. The probe supplies producer source for target discovery. |
| Typecheck the known result | With `Vec 3 Bool`, the client builds, prints `[True,False,True]` and rejects a two-cell body. |
| Typecheck the computed result | The source declaration builds and prints `[True,False,True]`. GHC rejects a two-cell body at `v`. The producer is imported only by the type signature. |
| Reuse or recheck the client | With an unchanged producer, GHC skips the client. Its interface records the producer object fingerprint. Changing only the fold seed preserves the producer ABI and makes the unchanged client fail with expected `Vec 4 Bool` and actual `Vec 3 Bool`. |

Run the two scripts with absolute paths to a built compiler and its
matching library directory. The interpreter probe requires the matching
`ghc` package and both interpreters. A compiler built without an internal
interpreter can run the source check with `-fexternal-interpreter` as a
third argument.

```sh
dir=docs/dependent-haskell/examples/closed-evaluation
"$dir/check-interpreter.sh" "$PWD/_build/stage1/bin/ghc" "$PWD/_build/stage1/lib"
"$dir/check-source.sh" "$PWD/_build/stage1/bin/ghc" "$PWD/_build/stage1/lib"
```

## Compiler work

- [x] Preserve the qualified imported term at a type-application head.
      Preserve ordinary type-name precedence, import usage, and the
      existing path for checked source equations. Pass the resolved
      name and source span from `lookupTypeAppHeadRn` to `tc_app_ty`.
- [x] Check the actual imported function type and its closed literal
      argument before execution. Construct the typed application from the
      resolved identifier. Execute it once for that elaboration, decode
      the `Natural` result and emit an ordinary `mkNumLitTy` before
      producing Core. A printed value supplies no coercion between
      existing Core types. [Renamer](../../../compiler/GHC/Rename/Env.hs)
      and [type elaboration](../../../compiler/GHC/Tc/Gen/HsType.hs).
- [x] Check the execution boundary. Safe Haskell rejects the call.
      Ordinary inferred modules that execute it lose safe-inferred status.
      Open lists, SOURCE-only imports and explicit-level imports are rejected.
      A missing object and a synchronous exception report GHC-70565 at
      the application. The configured interpreter must support the compiled
      object. Both native interpreter modes respond to a process-group
      interrupt during evaluation of a diverging producer.
- [x] Record the actual linked dependencies in the consumer interface.
      Compile an unchanged client again after rebuilding only the producer.
      Inspect the recorded object fingerprint and first verify that an
      unchanged build skips the client. Its old three-cell definition
      must then fail at the binding. An ABI check alone would miss this
      change. [Splice dependency recording](../../../compiler/GHC/Tc/Gen/Splice.hs)
      and [interface usage calculation](../../../compiler/GHC/Iface/Make.hs).

The compiler checks the imported function and the literal list before
execution. It can still reject the surrounding type or declaration after
the call has run.

The separate-client check passed with the bootstrap-built compiler using
external iserv, and with the self-hosted compiler using each interpreter.
The latter loads a dynamic object in its internal interpreter on this
Linux build. The check replaces only the object named in the client's
interface, preserving the original interfaces and other object variant.
Further controls used an empty list and a literal larger than a machine
word. The producer returned `70` and `18446744073709551617` respectively.
The [Showcase](../Showcase.hs) reflects a computed index with `natVal`.

## Closed character calls

Proposal #509 uses ordinary functions from an installed library in proofs.
The type checker can run `Data.Char.toUpper 'x'` to obtain `'X'` and
`Data.Char.isDigit '7'` to obtain `'True`. A signature that claims
`isDigit '7' :~: 'False` fails at `Refl`. The client imports `Data.Char`
only for its types. [Proposal #509](https://github.com/ghc-proposals/ghc-proposals/pull/509)
and [Showcase](../Showcase.hs).

M101 selects the input and result representations independently. It checks
the resolved function's ordinary unary arrow and the input literal before
execution. A `Char` or list of `Natural` values can produce a `Natural`, a
`Char`, or a `Bool`. A user-defined `Char -> Natural` function can therefore
compute a numeric index. GHC reuses its character literals and DataKinds'
promoted Boolean constructors for the result. A call with an unsupported
result such as `Data.Char.ord :: Char -> Int` reports the actual return
type at the function name.

The client interface lists the direct `base` dependency on this Linux
build. It fingerprints the `ghc-internal` Haskell archive with the
external interpreter and the shared library with the internal interpreter.
For a home module, changing just the recorded `.o` or `.dyn_o` invalidates
the client while the installed interfaces stay byte-identical.
[Character witnesses](../../../testsuite/tests/dependent/should_run/NativeClosedChar.hs)
and [link dependency recording](../../../compiler/GHC/HsToCore/Usage.hs).

## Overloaded library functions

```haskell
total :: Prelude.sum '[2, 3, 5] :~: 10
total = Refl
```

The ordinary type of `sum` has domain `t a`. Matching it against
`[Natural]` determines both type arguments. The type checker then checks
the imported occurrence against `[Natural] -> Natural` as a term. It
selects the `Foldable []` and `Num Natural` instances. The same source
test calls a producer that doubles the total, a `Foldable` function that
returns `Bool`, and a polymorphic identity on a character. A missing or
ambiguous dictionary prevents execution even when the producer would
ignore it. An instance from the module under compilation fails the
ordinary stage check. [Source witnesses](../../../testsuite/tests/dependent/should_run/NativeClosedSum.hs).

The specialization records the imported names that supply its evidence.
This matters when the simplifier replaces a class method call with its
implementation. The instance object can disappear from the evaluated
code. The unchanged client must still recheck its proof when the instance
selects another method implementation. If the bytecode retains a
dictionary reference, the client also records the object that the
interpreter loaded.
[Instance recompilation test](../../../testsuite/tests/dependent/should_run/NativeClosedSumDependencies.sh).

## List results

```haskell
reversed :: Prelude.reverse '[2, 3, 5] :~: '[5, 3, 2]
reversed = Refl
```

The imported definition determines the order. The function's checked result
type supplies the element kind, including for an empty list. The initial
list representation supports `Natural`, `Char` and `Bool` elements.
A function that throws an exception
in a later element fails at the source call. The interpreter must finish
reading the result before GHC accepts the proof.

For a list, the interpreter uses the built-in element dictionary's
`showList` method. A module may define its own `Show [Natural]` instance
that prints every list as `[2,3,5]`. That instance still works for
ordinary printing. The compiler reads the actual reversed order.
[Source checks](../../../testsuite/tests/dependent/should_run/NativeClosedLists.hs)
and [representation references](../REFERENCES.md#reifying-closed-list-results).

## Nested closed calls

```haskell
total :: Prelude.sum (Prelude.reverse '[2, 3, 5]) :~: 10
total = Refl
```

The inner result supplies the input kind for `sum`. In another check, the
outer function reads the first element and its result must be `5`.
This distinguishes a genuinely reversed argument from the original list.
The compiler checks staging before preparing a nested argument.
The original implementation followed draft proposal #509 and
reified each argument. The current staged path composes ordinary
calls before observing their result.
[Proposal #509](https://github.com/ghc-proposals/ghc-proposals/pull/509)
and [source tests](../../../testsuite/tests/dependent/should_run/NativeClosedLists.hs).

GHC uses the declared argument and result types to check the whole chain
before entering the interpreter. For example, applying `toUpper` to a
list produces a kind error even if evaluating that list would throw an
exception. If the kinds agree but an outer instance is missing, the
instance error also takes precedence. These checks use the declared
types of the functions. The enclosing type still has its own checks.
[Kind error](../../../testsuite/tests/dependent/should_fail/NativeClosedNestedKind.hs)
and [missing instance](../../../testsuite/tests/dependent/should_fail/NativeClosedNestedMissing.hs).

The checked kind of each intermediate guides the enclosing ordinary
call. For example, `Prelude.odd (Prelude.sum '[2, 3, 5])` returns
`'False`. A computed empty list of naturals cannot supply the Boolean
list required by `and`. When a checked family supplies an operand,
GHC packs its promoted value with the matching ordinary constructor.
[Positive examples](../../../testsuite/tests/dependent/should_run/NativeClosedLists.hs)
and [wrong empty-list kind](../../../testsuite/tests/dependent/should_fail/NativeClosedComputedEmpty.hs).

The client records both objects when the interpreter links code for
both functions. With their interfaces held fixed, changing either object
can invalidate the client's type. [Recompilation test](../../../testsuite/tests/dependent/should_run/NativeClosedNestedDependencies.sh).

Same-module definitions require further work on R21. General closed
expressions and user-defined representations, including newtypes, remain
open. Inputs with unknowns belong to [R16](R16-open-term-evaluator.md).
The [implementation references](../REFERENCES.md#executing-closed-calls-and-reducing-open-calls)
give the earlier prototype and the current GHC entry points.

## Ground source inputs

M110 accepts the same six kinds at the innermost source call as it does
between nested calls. `Prelude.odd 5` returns `'True`. The declaration
of `Prelude.and` selects the Boolean element kind for `'[]`, so the call
returns `'True`. An ambiguous output such as the result of
`Prelude.fromIntegral 5` still needs more information than its operand
supplies. A mixed literal list fails at the incorrectly kinded element.
[Source proofs](../../../testsuite/tests/dependent/should_run/NativeClosedLists.hs)
and [mixed list diagnostic](../../../testsuite/tests/dependent/should_fail/NativeClosedSourceMixed.stderr).

## Unqualified names and list syntax

M111 uses the upstream source spelling. With `RequiredTypeArguments`,
an in-scope term can supply an unqualified application head in a type.
An explicit `forall.` also prevents implicit quantification of the
function name. With neither, the existing implicit type binder keeps
its meaning. A type binder introduced by `forall length.` still takes
precedence. GHC's parser distinguishes `[1,2,3]` from the list type
syntax `[x]` under `ListTuplePuns`. Use `'[x]` or `'[]` to request a
singleton or empty promoted list when that extension is active.
[Consumer](../examples/closed-evaluation/Client.hs)
and [proposal #378 on term variables in types](https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html#term-variables-in-types).

## Checked family results inside closed calls

In `Data.Char.toLower (Data.Char.toUpper 'x')`, the installed
`toUpper` supplies a checked family equation and `toLower` executes
in the interpreter. The solver reduces the inner application to
`'X'` using the producer interface. The outer function returns `'x'`.
A change in the checked equations can invalidate the consumer even
when the interpreter never loads the certified worker object.
A purely symbolic `toUpper (toUpper c)` can remain in an exported
type while `c` is unknown.
[Mixed source proofs](../../../testsuite/tests/dependent/should_run/NativeClosedLists.hs)
and [nominal family reduction](../../../compiler/GHC/Tc/Solver.hs).

## Functions with several arguments

`Prelude.gcd 18 24 :~: 6` checks with `Refl`. Both integers select
`Natural`. GHC resolves `Integral Natural` once for the whole application.
`Prelude.subtract 2 7 :~: 5` checks the order of the two inputs.
A helper with type `(Foldable f, Eq a) => f a -> f a -> Bool` can take
`'[]` and `'[ 'True]`. The second operand fixes the first at `[Bool]`.
[Source proofs](../../../testsuite/tests/dependent/should_run/NativeClosedLists.hs).

GHC checks arity, kinds, staging and instances before executing any
operand. It reports a conflicting kind at the first inconsistent operand.
The interpreter evaluates the composed ordinary applications with
Haskell's usual demand behavior. A checked family still normalizes
its operand at the type boundary. The staged call checker accepts
leading invisible parameters.
[Source of the evaluation rule](../REFERENCES.md#saturated-closed-calls-and-open-pair-projections).

## Returning a pair

`Prelude.quotRem 17 5 :~: '(3,2)` checks with `Refl`. The compiler
evaluates the ordinary function once and inspects its actual pair.
An ordinary nested call can also receive a pair directly.

When the pair is the observed result, the interpreter encodes both
fields before returning to the type checker. An overlapping
`Show (Natural, Natural)` instance in the client cannot change the result.
Observing a pair forces a bottom in its second field. An ordinary
consumer may leave that field undemanded. GHC rejects an unsupported
observed result such as `(Natural, Int)` before running its producer.
The one-shot test also changes a producer object while retaining the
installed interface bytes and the client source. The old equality fails
when the compiler checks the client again.
[Returned-pair tests](../../../testsuite/tests/dependent/should_run/NativeClosedReturnedPair.sh)
and [the result representation](../../../compiler/GHC/Tc/Gen/Splice.hs).

## Returning a constructor with alternatives

`Data.List.uncons '[2,3,5] :~: 'Just '(2, '[3,5])` checks with `Refl`.
`Data.Maybe.fromMaybe 9 (Data.Maybe.listToMaybe '[])` computes `9`.
An ordinary parser with type `String -> Either String Natural` can return
either a value or a diagnostic string to another closed call. The
[Showcase](../Showcase.hs) parses the character lists for `23` and `x`
and uses `fromRight` to select a default for the second.

To observe a result, GHC checks the field kinds of all alternatives
before running the producer. A Core case records its actual constructor
and encodes its present fields. The compiler reconstructs the value
with the corresponding DataKinds constructor. Observing
`Just undefined` fails when the encoder demands its field.
`isNothing (Just undefined)` only needs the constructor tag. A
client-defined `Show` instance cannot supply the constructor or fields.
[Source proofs](../../../testsuite/tests/dependent/should_run/NativeClosedChoices.hs)
and [failure in a present field](../../../testsuite/tests/dependent/should_fail/NativeClosedPresentBottom.hs).

The native checks pass a `Natural` greater than 2^64 through a nested
`Right (Just (...))` and read back the result. The installed `Either`
codec uses the defining module and type name. A user datatype with the
same type name uses the ordinary algebraic datatype rules described
below. Changing only the recorded producer
object from `Right (Just (3, True))` to `Right (Just (4, True))`
invalidates the old proof. In that check, the producer interfaces are
byte-for-byte identical. [Lookalike datatype diagnostic](../../../testsuite/tests/dependent/should_fail/NativeClosedOtherEither.stderr).

The response uses the installed definitions of `Show`, `IO` and
`returnIO` by their defining module and name. The renderer checks that
the scalar dictionary comes from the module that defines the canonical
`Show` class. It checks the representation of the complete response as
`IO String` before passing it to the interpreter. Producer specialization
still uses ordinary instance resolution.
[Response construction](../../../compiler/GHC/Tc/Gen/Splice.hs).

An attempted closed call to `Data.Maybe.fromJust` requires an implicit
`HasCallStack` argument. The current staged call checker reports
GHC-91416 at that use. The integration test uses `fromMaybe` with an
explicit default.

## Compound list results and constructor literals

Closed results can contain lists at any supported element kind.
The built-in cases are `Natural`, `Char`, `Bool`, and lists, boxed pairs,
`Maybe` and `Either` whose element or field types are all supported.
For example, `Data.List.tails '[1, 2, 3]` returns a list of Natural lists.
`Prelude.concat` can consume that computed result. The source test also
passes a list of lists of `Either` through `concat` and selects the left
fields with `Data.Either.lefts`.
[Compound list proofs](../../../testsuite/tests/dependent/should_run/NativeClosedCompoundLists.hs).

The target interpreter uses a recursive Core case to inspect a compound
list. Each cons records its element and continues with its tail. Nil
terminates that list's encoding. Scalar lists keep the element's
canonical `showList` representation. The compiler consumes the complete
response and reconstructs the promoted constructors. Forcing the
response forces the list spine and every present element. It also forces
the present fields of a nested pair or sum. An absent alternative has no
fields to force. An ordinary consumer can ignore an element or the
entire list before the final result is observed. An observed infinite
list cannot finish this encoding, and large results require space
for a complete response.
[Result codec](../../../compiler/GHC/Tc/Gen/Splice.hs)
and [bottom control](../../../testsuite/tests/dependent/should_fail/NativeClosedCompoundListBottom.hs).

Complete ordinary calls can also take direct promoted constructor
literals. For example, `Prelude.concat '[ '[], '[ 'x'], '[]]` returns
`'[ 'x']`, and `Data.Maybe.catMaybes '[ 'Nothing, 'Just 3]` returns
`'[3]`. GHC uses the ordinary kind checker and the producer's input
types. A later Boolean operand can fix the missing side of an earlier
`Either`. The same inference can fix a bare empty list to `[Char]`.
A fully known domain can select an empty list of compound elements.
The legacy unconstrained bare empty list still defaults to
`[Natural]`. Missing constructor kinds remain ambiguous when the inputs
supply no information to fix them.

GHC plans staged child calls before execution, checks the operand kinds
together, and checks the producer's required dictionaries before running
the children. It admits constructor literal syntax with closed fields.
Explicit kind applications and expressions inside those fields remain
outside this source grammar. The response decoder still validates the
actual constructor and its fields against the checked type.
[Ambiguous constructor](../../../testsuite/tests/dependent/should_fail/NativeClosedConstructorAmbiguous.hs)
and [missing evidence before a throwing child](../../../testsuite/tests/dependent/should_fail/NativeClosedConstructorEvidence.hs).

Both compiler stages built. The self-hosted compiler checked the compound
examples with both interpreter modes and passed the registered source
tests. A separate, bounded check returned and consumed 32, 256, and 1,024
optional elements with Core and STG lint enabled. The normal-way
dependent directory passed 198 of 234 cases. The 36 failures had the
same individual output as at M119.

## Returning an ordinary user-defined enumeration

The ordinary `Signal` module exports `data Signal = Stop | Go`. It
enables no dependent features. A client checks which constructor
`Signal.at` returns and can pass that constructor to another ordinary
function.

```haskell
evenSignal :: Signal.at 2 :~: 'Signal.Stop
evenSignal = Refl

mayProceed :: Signal.shouldStop (Signal.at 3) :~: 'False
mayProceed = Refl
```

The interpreter inspects the actual boxed constructor. The compiler
uses its ordinal in this datatype and the original DataKinds constructor
identity. A different module can declare constructors with the same
spelling and in a different order. The result type selects the correct
declaration. For example, if the consumer imports both as qualified
modules, GHC-70565 reports an operand of kind `B.Token` where `A.Token`
is required. An enumeration written with GADT declaration syntax also
works if all its constructors are ordinary nullary constructors.
[Ordinary producer](../examples/closed-evaluation/Signal.hs)
and [source controls](../../../testsuite/tests/dependent/should_run/NativeClosedEnums.hs).

To evaluate a producer result, the caller must bring every constructor
into scope with a non-SOURCE import. A qualified import or a re-export suffices. An optimized
interface can contain the representation of an abstract type. Access to
that metadata alone does not authorize inspection. GHC-70565 identifies
the type and the import requirement, without printing its private
constructors. GHC records the constructor uses for unused-import checks
and recompilation.
[Abstract result](../../../testsuite/tests/dependent/should_fail/NativeClosedEnumAbstract.hs)
and [partially imported result](../../../testsuite/tests/dependent/should_fail/NativeClosedEnumPartial.hs).

A one-shot client was reusable with unchanged inputs. Changing the
recorded producer object while preserving all interface bytes
invalidated its old equality. Removing constructor access from the
re-export interface also invalidated the client with the original
objects still installed.

## M127. Returning finite values of ordinary algebraic datatypes

An ordinary producer can return a value of a parameterized recursive
datatype. A later ordinary call can consume that value. In this example
the same source module also defines a polymorphic `leaves` function.

```haskell
tree :: T.sample 3 :~: 'T.Fork ('T.Leaf 3) ('T.Fork ('T.Leaf 4) ('T.Leaf 5))
tree = Refl

leaves :: T.leaves (T.sample 3) :~: '[3, 4, 5]
leaves = Refl
```

Before execution, GHC checks all constructors of the result type and
builds a finite graph of their ground field types. A repeated exact type
uses an existing node. A fresh instantiation of a datatype already on
the current path must be a proper subterm of the earlier type. This
admits ordinary and mutual recursion and rejects expanding recursion
such as `Grow a` containing `Grow [a]`. It can also reject a finite
cycle where a datatype permutes two different parameter types. The
diagnostic identifies the two instantiations that failed the check.
GHC admits boxed strict fields with matching source and worker field
types. M129 adds support for constructor result equalities. Unboxed and
unpacked representations and retained field binders remain open.
[Ordinary tree](../examples/closed-evaluation/FiniteTree.hs),
[source tests](../../../testsuite/tests/dependent/should_run/NativeClosedEnums.hs)
and [expanding recursion](../../../testsuite/tests/dependent/should_fail/NativeClosedExpanding.hs).

The interpreter uses the checked constructors to inspect the actual
result. It runs the producer once and encodes only the returned
constructor and its present fields. A user `Show` instance cannot
choose the constructor or its fields. Full reification forces every
present field. An infinite value can diverge.

Operand construction follows the constructors in the checked value.
For a user datatype, each selected constructor must be in scope from a
non-SOURCE import. For example, a client that imports only `Ready` can
pass `'Ready` to `score`. The unused alternatives require no codec. The
constructor wrapper enforces demand when packing strict boxed fields.
A bare imported nullary type synonym can name an operand. After
expansion, a private constructor still fails at the operand span
without printing its name.
[Private constructor alias](../../../testsuite/tests/dependent/should_fail/NativeClosedEnumOperand.hs)
and [partially imported unrelated `Either`](../../../testsuite/tests/dependent/should_fail/NativeClosedOtherEither.hs).

The self-hosted compiler passed the recursive-result and selected-operand
checks with the internal and external interpreter. The checks used Core
and STG lint. They covered tree reconstruction, mutual recursion, boxed
strict fields and failure at bottom in a selected field. They rebuilt the provider
with an additional constructor, leaving the client source, producer
signature and returned `Leaf` fixed. The unchanged client failed
because its explicit constructor import became partial.

## M129. Constructors at a known GADT index

An ordinary GADT can return different constructors at different indices.
In `Mark a flag`, `Off` returns `Mark a 'False`. Both `Count` and
`Pair` return `Mark a 'True`. GHC therefore accepts this proof in a client
that imports only `Count` and `Pair` from the datatype.

```haskell
nested :: P.readMark (P.sample 6) :~: 67
nested = Refl
```

The ordinary producer returns `Pair 6 7`. The second ordinary function
computes `10 * 6 + 7`. GHC also rejects `P.sample 6 :~: 'P.Pair 7 6`
at `Refl`. Trying to compare `P.sample 0` with `'P.Off` fails earlier,
at kind checking. The two sides have different Boolean indices.
[Ordinary producer](../examples/closed-evaluation/IndexedMark.hs),
[dependent client](../../../testsuite/tests/dependent/should_run/NativeClosedIndexed.hs)
and [wrong-index diagnostic](../../../testsuite/tests/dependent/should_fail/NativeClosedIndexedWrongKind.stderr).

The compiler checks reachability with GHC's existing constructor
matcher. For each possible constructor, it binds the worker's equality
evidence in a Core case. It reads back the constructor tag and data
fields. DataKinds supplies the promoted constructor and the ordinary
constructor wrapper supplies the equality during operand reconstruction.
An index may refer to another ground parameter. User-written parameter
order is respected when applying the promoted constructor.

Missing any reachable constructor blocks inspection before execution.
The client records the parent datatype declaration as a dependency.
Changing just its interface to make a hidden constructor reachable
invalidated the unchanged client with all original objects still in place.
M129 excludes reachable constructors with existential binders or explicit
dictionaries. A branch GHC can rule out at the
requested index needs neither a representation nor a constructor import.
[Missing constructor](../../../testsuite/tests/dependent/should_fail/NativeClosedIndexedPartial.hs)
and [dictionary control](../../../testsuite/tests/dependent/should_fail/NativeClosedIndexedContext.hs).

## M130. Recovering the predecessor of a vector length

An ordinary source module defines a length-indexed vector. Its `two`
function constructs two consecutive numbers. Its `safeHead` accepts a
vector with any nonzero length. Both functions can also be used in a
dependent client's type.

```haskell
first :: V.safeHead (V.two 3) :~: 3
first = Refl
```

GHC also checks the entire result against
`'V.VCons 3 ('V.VCons 4 'V.VNil)`. Reading both cells with an ordinary
consumer gives `34`. At a call on an empty vector, GHC-70565 reports
that the operand's kind is `Vec 'Z Natural` while the function expects
`Vec ('S n) a`. The mismatch is known from the producer's signature.
The error fixture uses a throwing producer so that premature execution
would be visible.
[Ordinary vector](../examples/closed-evaluation/IndexedVec.hs),
[successful checks](../../../testsuite/tests/dependent/should_run/NativeClosedIndexedVec.hs)
and [empty-vector diagnostic](../../../testsuite/tests/dependent/should_fail/NativeClosedIndexedVecEmpty.stderr).

The predecessor is an erased type argument of `VCons`. In the first
case branch GHC has nominal evidence that `'S ('S 'Z)` equals `'S m`.
Projecting the constructor argument relates `'S 'Z` and `m`. The
renderer uses that proof to view the tail at its known length. The
decoder uses the recovered argument in DataKinds constructor order.
The interpreter transmits just the constructor tags and data fields.

This works through saturated, nominally injective ordinary or promoted
constructors. For example, `PairOf a '(n, m)` determines the lengths of
two vector fields independently. The compiler checks repeated occurrences
of the same index for agreement. A recursive field may shorten one
non-`Type` index to a strict structural subterm while the other arguments
stay fixed. [Two indexed fields](../../../testsuite/tests/dependent/should_run/NativeClosedVectorPair.hs).

If an erased argument is absent from the result or appears only under
a noninjective family, the compiler reports the unrecoverable argument
before executing the producer. M135 below handles an index carried by
a runtime singleton field.
[Core evidence](../CORE.md#nominal-evidence-for-constructor-local-indices),
[hidden-index diagnostic](../../../testsuite/tests/dependent/should_fail/NativeClosedIndexHidden.stderr)
and [noninjective family diagnostic](../../../testsuite/tests/dependent/should_fail/NativeClosedIndexFamily.stderr).

## M132. Written existential operands

An ordinary consumer can read a written promoted package that contains
a singleton and a vector. The kind checker supplies the erased length
argument in the checked constructor application.

```haskell
cells :: E.readSomeVec ('E.SomeVec ('E.SS ('E.SS 'E.SZ))
                                 ('V.VCons 4 ('V.VCons 5 'V.VNil))) :~: '[4, 5]
cells = Refl
```

Operand construction checks the actual source type arguments, the
constructor's result index and its field kinds. It applies the ordinary
constructor wrapper. An ambiguous erased argument cannot be packed as
a concrete operand. For a computed package, the result type alone leaves
the length existential. M135 recovers it by observing the first field.
[Ordinary singleton and package](../examples/closed-evaluation/ExistentialVec.hs),
[written operand proof](../../../testsuite/tests/dependent/should_run/NativeClosedIndexedVec.hs)
and [ambiguous operand](../../../testsuite/tests/dependent/should_fail/NativeClosedSomeVecAmbiguous.hs).

## M135. Reading a runtime existential package

`someVec :: Natural -> SomeVec Natural` returns an empty vector at
zero, one cell at an odd input, and two cells at a positive even input.
The type stays the same in all three cases. GHC observes the stored
witness to recover the length chosen on that call.

```haskell
two :: E.someVec 4 :~: 'E.SomeVec ('E.SS ('E.SS 'E.SZ))
                                ('V.VCons 4 ('V.VCons 5 'V.VNil))
two = Refl
```

The original M135 package has one constructor, one erased index, and two
fields. The first field carries a witness whose constructors specify
a nullary base or a unary successor of another witness. The second
field has its own indexed datatype. Its recursive field may appear
anywhere among the ordinary data fields. Each renderer uses the actual
type variables and equalities bound by its own Core case. The decoder
recovers the index from the first field and checks the second against it.
The fare regression has two base indices and puts the recursive field
first. GHC obtains the constructors for all these examples from their
datatype declarations.
[Vector proofs](../../../testsuite/tests/dependent/should_run/NativeClosedIndexedVec.hs)
and [fare datatype](../../../testsuite/tests/dependent/should_run/NativeClosedIndexedRoute.hs).

Ordinary imports must expose the constructors used by both renderers
and the promoted index constructors. GHC checks the imports before
executing the producer. A test uses
`unsafeCoerce` to give a two-cell vector the wrong length. Its runtime
constructor sequence fails the decoder's check against a one-cell
witness. A divergent witness can still make closed checking diverge.
The [shared vector length](#recover-a-hidden-length-from-the-vector) case below
uses the vector constructors themselves.
[Import check](../../../testsuite/tests/dependent/should_fail/NativeClosedSomeVecIndexVisibility.hs),
[missing witness](../../../testsuite/tests/dependent/should_fail/NativeClosedWitnessMissing.hs)
and [inconsistent result](../../../testsuite/tests/dependent/should_fail/NativeClosedWitnessMismatch.hs).

A separate-module M135 check compiled a proof of
`E.readSomeVec (E.someVec 4) :~: '[4, 5]` twice. GHC reused the unchanged
client. Changing only the producer from `n + 1` to `n + 2`
changed its object and preserved its interface ABI hash. GHC rechecked
the original proof and reported that the returned second cell was
`6` where the client required `5`.

## Read back a recursive result with witnessed indices

An ordinary producer can return a batch of vectors whose lengths vary
from one chunk to the next. Each `Chunk` stores a singleton for its own
vector length. The remaining batch has the same type as the whole
result. In a separate module the client checks both lengths and the
three values with `Refl`.

```haskell
evenBatch :: B.build 4 :~: 'B.Chunk ('E.SS 'E.SZ) ('V.VCons 4 'V.VNil)
                            ('B.Chunk ('E.SS ('E.SS 'E.SZ))
                                ('V.VCons 5 ('V.VCons 6 'V.VNil)) 'B.End)
evenBatch = Refl
```

The compiler checks the first two fields of each witnessed constructor.
It uses the existing recursive result codec for the remaining fields.
The interpreter reads each singleton and the decoder checks its vector
at that length. A length from an earlier chunk does not constrain a
later chunk. A hidden reachable constructor or an inconsistent witness
causes GHC-70565 at the source call.

Full readback demands the remaining batch. An ordinary enclosing call
can inspect just the first width. The proof of
`B.firstWidth (B.partial 9) :~: 1` then leaves the vector and remaining
chunks undefined. The source-hidden regression uses stripped producer
interfaces. It checks the full even and odd results, the partial-width
proof, and a full read that forces an undefined tail. The Showcase
prints the complete proof, the two widths, and the partial observation.
[Ordinary batch](../examples/closed-evaluation/WitnessedBatch.hs),
[separate-module check](../../../testsuite/tests/dependent/should_run/NativeClosedWitnessedBatch.sh),
and [Idris dependent pairs](../REFERENCES.md#recursively-read-back-witnessed-constructors).

An ordinary producer can also return two vectors at the same hidden
length. The fields can have different element types.

```haskell
data MaskedRow a where
    MaskedRow :: SN n -> Vec n a -> Char -> Vec n Bool -> MaskedRow a
```

The evaluator reads the witness and checks each vector at that index.
It reads the intervening character at its own type. A complete result
with the wrong mask length fails at the source call. An outer observation
of just the width can leave the other three fields undefined.
`selectedValues` consumes the two vectors together. The shared index
excludes unequal spines. Applying the mask `[False, True]` to the values
`[4, 5]` returns `[5]`. In an ordinary module,
`MaskedRow (SS SZ) (VCons 4 VNil) 'x' VNil` fails at the last `VNil`
with GHC-83865. The mask has length zero where the constructor requires one.
[Source and projections](../examples/closed-evaluation/WitnessedBatch.hs).

### Recover a hidden length from the vector

```haskell
data KeyedRow a where
    KeyedRow :: Char -> Vec n Char -> Vec n a -> KeyedRow a
```

GHC reads the tag and both vectors in declaration order. The keys determine
the hidden length. `VNil` gives zero and each `VCons` adds one. GHC checks
the values at that length before constructing the promoted row. The empty
row and the complete two-column row both admit a `Refl` proof.
`lookupColumn 'y' (keyedRow 4)` returns `Just 5`. Finding the first
key leaves the rest of the values unevaluated.

Each indexed field must have the checked constructor cases described above. A `Proxy n`
field leaves its index unknown. GHC reports GHC-70565 at the written call
and names the field and its constructor before executing the producer.
[Ordinary row](../examples/closed-evaluation/WitnessedBatch.hs) and
[separate-module checks](../../../testsuite/tests/dependent/should_run/NativeClosedWitnessedBatch.sh).

### Read independent row and column counts

```haskell
data TableMargins a where
    TableMargins :: forall cols rows a. Vec rows Char -> Vec cols Char
                 -> Vec rows a -> Vec cols a -> TableMargins a
```

The row labels determine `rows`. The column labels determine `cols`.
The evaluator checks each vector of totals against its own labels.
The order in the `forall` can differ from the order in which the
fields reveal these lengths. A table can have zero rows and one column.
With one row and two columns, `lookupTotals 'r' 'y' (tableMargins 4)`
returns `(Just 8, Just 4)`. Reading only the column total leaves the row
labels and row totals unevaluated.

An ordinary caller that supplies the column totals in the row-total
position gets GHC-25897 at that argument. GHC reports the two rigid
lengths `cols` and `rows`.

In a separate recompilation check, GHC reused the unchanged proof. Replacing
only the producer object added a second row and preserved the module ABI.
GHC then rechecked the proof and reported the extra label at `Refl`
with GHC-83865.

### Read the stored cells of a rectangular table

```haskell
data Table a where
    Table :: Vec rows Char -> Vec cols Char -> Vec rows (Vec cols a) -> Table a
```

The labels determine both lengths before the evaluator reads the cells.
The outer vector must contain one row for each row label. Every inner
vector must contain one cell for each column label. A table with no rows
still has a column count from its labels. A table with no columns can
contain a vector of empty rows.

The separate dependent client proves the full value of `sample 4` with
`Refl`. It has the rows `[1,2,3]` and `[4,5,6]`. The client can also prove
the result of `totals (sample 4)`, whose row totals are `[6,15]` and column
totals are `[5,7,9]`. GHC reads the producer's interface and runs its
object code.

A field that uses several hidden indices can determine its own structural
index. In `ColumnTable cols cells`, the labels establish the column count.
The constructors of the outer vector establish the row count. The type of
each inner vector still uses the established column count. An empty outer
vector establishes zero rows. A nonempty vector of empty rows establishes
a positive row count and zero columns. GHC reports GHC-70565 at the
producer call if another required index is missing. It names the constructor, field number and indices before
running the producer. An inconsistent row or column count returned by the
interpreter also gives GHC-70565 at the call. In ordinary term code a
ragged row gives GHC-83865 at the incorrect constructor field.
[Ordinary table](../examples/closed-evaluation/RectangularTable.hs),
[source-hidden regression](../../../testsuite/tests/dependent/should_run/NativeClosedWitnessedBatch.sh)
and [Agda comparison](../REFERENCES.md#recursively-read-back-witnessed-constructors).

## M136. Composing ordinary calls before observation

`LazyParcel` exports an abstract datatype and ordinary functions.
Its constructor is private. The second field of `make 7` throws if
demanded. The first field can still appear in a type.

```haskell
firstField :: P.first (P.make 7) :~: 7
firstField = Refl
```

GHC checks the stage, kind and instance requirements of each ordinary
call. It then compiles their typed Core application together. Only the
observed result needs a codec. The intermediate type may stay abstract
or have a representation that the codec cannot reify. For example,
an ordinary consumer can ignore an intermediate `Either Natural Int`.
Trying to observe the parcel itself still fails because the caller
cannot inspect its constructor.
[Ordinary parcel](../examples/closed-evaluation/LazyParcel.hs),
[abstract intermediate](../../../testsuite/tests/dependent/should_run/NativeClosedLazyParcel.hs)
and [observing an abstract result](../../../testsuite/tests/dependent/should_fail/NativeClosedLazyParcelEscape.hs).

A kind mismatch still points to the operand and prevents execution.
When a demanded field throws, the interpreter reports the exception
at the call whose result is observed. For example,
`P.second (P.make 7)` reports the exception at `P.second`. An ordinary
fused computation has no separate observation of `P.make`. A checked
source family remains a boundary that can require its own observation.
[Kind mismatch](../../../testsuite/tests/dependent/should_fail/NativeClosedLazyParcelKind.hs)
and [demanded field](../../../testsuite/tests/dependent/should_fail/NativeClosedLazyParcelDemand.hs).

GHC records the ordinary heads and dictionary names before interactive
simplification. It records the objects actually linked by the
interpreter. The existing separate-module test replaces each linked
object in turn while keeping its ABI and client source fixed.
[Object replay](../../../testsuite/tests/dependent/should_run/NativeClosedNestedDependencies.sh).

Another test gives an ordinary provider an optimization rule for
`first (make n)`. Both the ordinary executable and the staged call use
the rule. It eliminates both calls, so the provider object cannot
invalidate the proof. Replacing only its interface changes the rule's
result from `7` to `8`. GHC rejects the unchanged client's proof at
`Refl`. [Optimized interface replay](../../../testsuite/tests/dependent/should_run/NativeClosedFusedRule.sh).

## M139. Using the installed Prelude length in a vector index

```haskell
vector :: Vec (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) Bool
vector = VCons True (VCons False (VCons True VNil))
```

`Vec` declares its first parameter at kind `Nat`. The list fixes the
input to `Prelude.length`, which returns an `Int`. The declared index
then fixes the result of `fromIntegral` to `Nat`. GHC checks the
`Integral Int` and `Num Natural` instances before executing the
closed application. It observes the final `3`. The signed intermediate
can stay in ordinary Core. A second example passes a negative `Int`
through `abs` before conversion. [Vector and proofs](../../../testsuite/tests/dependent/should_run/NativeClosedSum.hs).

A written result annotation can also supply the kind. A bare
`Proxy (Prelude.fromIntegral 5)` leaves it unknown. Asking for
`Bool` requires a `Num Bool` instance, even with deferred type errors.
Using `Prelude.length` directly as a `Nat` still produces a kind
mismatch at the call. M139 handled the expected kind at the outer call.
[Ambiguous result](../../../testsuite/tests/dependent/should_fail/NativeClosedSourceAmbiguous.hs),
[missing instance](../../../testsuite/tests/dependent/should_fail/NativeClosedResultDictionary.hs)
and [missing conversion](../../../testsuite/tests/dependent/should_fail/NativeClosedLengthNoCast.hs).

## M140. Checking a nested conversion

```haskell
proof :: Prelude.max (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) 2 :~: 3
proof = Refl
```

The second argument of `max` fixes its operand type to `Natural`.
The first argument uses that type to select the result of `fromIntegral`.
Swapping the two source arguments gives the same result. A declared
monomorphic input or a known enclosing result can also supply the kind.
A constructor literal such as `'Just 6` can constrain a different argument.

GHC checks the literal siblings before planning the nested ordinary calls.
It still executes their combined Core expression on demand. A test passes
a conversion of an exceptional `Int` to a function that ignores its
argument. The result is `'True`. A requested `Num Bool` instance still
fails at the conversion even when the outer function ignores the value.
At this checkpoint, a computed sibling could not determine another
computed sibling's type.
[Positive and demand checks](../../../testsuite/tests/dependent/should_run/NativeClosedNestedExpected.hs)
and [unsatisfied dictionary](../../../testsuite/tests/dependent/should_fail/NativeClosedNestedResultDictionary.hs).

## M143. Sharing the kinds of computed operands

```haskell
proof :: Prelude.max (Prelude.sum '[1, 2])
                     (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) :~: 3
proof = Refl
```

`sum` takes a list of `Natural` and returns a `Natural`. Its result fixes
the shared operand type of `max`. That fixes the result of `fromIntegral`,
even if the conversion appears first. An enclosing result kind and a
constructor literal can still constrain the same application.

The planner instantiates each ordinary application in the tree once.
The kind solver checks their shared constraints before the planner
prepares the ordinary producer dictionaries. A checked-family operand
and a nested legacy empty-list application retain their own admission
checks. Each boundary supplies its checked kind to the tree. Bare empty
lists default to `Natural` only after the applicable operand constraints
have been collected.

The compiler combines the original Core applications for the final
observer. An ignored conversion can contain a throwing operand without
forcing it. Two conversions without a target type remain ambiguous.
Incompatible computed kinds fail before execution.
[Both orders and the demand check](../../../testsuite/tests/dependent/should_run/NativeClosedNestedExpected.hs),
[ambiguous computed siblings](../../../testsuite/tests/dependent/should_fail/NativeClosedComputedSiblingsAmbiguous.hs)
and [incompatible computed siblings](../../../testsuite/tests/dependent/should_fail/NativeClosedComputedSiblingsMismatch.hs).

At M143 an interface-only projection still checked its argument separately. In
`(P.pick (W.pair (Prelude.fromIntegral (Prelude.length '[1,2,3])) 'x') :: Nat)`,
`P.pick` selects the first field. Its expected result fixes that field's
kind to `Nat`. M143 asked the ordinary pair builder to infer the whole pair first
and reported GHC-70565 at `W.pair`. M145 below uses that program as
its source check.

## M145. Sharing kinds across a checked family

    proof :: (Projection.pick
                (Worker.makePair
                  (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) 'x') :: Nat)
             :~: 3
    proof = Refl

The checked projection relates its result kind to the first kind of
its pair input. The enclosing `Nat` fixes that first kind before
`Char` is known. The second literal supplies `Char`. The shared tree
uses both constraints to specialize the ordinary pair builder and
`fromIntegral`. The projection can use its checked interface family
when the linker cannot load the ordinary projection. The kind solver
finishes specialization before the compiler prepares ordinary evidence
and the checked family application. The same rule checks still apply.

Closed arguments can also determine erased indices. The literal vector
constructors in `PackedVecLibrary.appendVec` fix both input lengths.
The solver reduces addition at those lengths to check the result
shape. Ordinary term checking instantiates the imported function at
the resulting type. If the result still needs an unresolved family
application, the evaluator cannot prepare readback. An imported
route function also uses this rule with an index of kind `Stop`.
Agda's vector append likewise obtains implicit lengths from its
arguments. [Agda implicit arguments](https://agda.readthedocs.io/en/v2.8.0/language/implicit-arguments.html),
[Agda vector append](https://agda.github.io/agda-stdlib/v2.3/Data.Vec.Base.html)
and [route check](../../../testsuite/tests/dependent/should_run/NativeClosedIndexedVec.hs).

An empty list can use a known result kind before selecting its legacy
`Natural` default. If an ordinary consumer requires two results of the
same type, a computed `Char` operand can fix the result of an earlier
`firstOf '[]`. A consumer that ignores that earlier operand leaves the
empty-list exception unevaluated. If the enclosing kind does not
determine an empty list's type, the existing default still applies.

An empty list can also take a compound element kind such as `Maybe Char`.
The checker uses the enclosing result constraint before it applies the
legacy default.

The tests compile the projection from ordinary source and use its
interface without its object. They check the inferred `Nat` result,
an unresolved result kind, a missing `Num Bool` instance, and the
ignored empty sibling. [Projection checks](../../../testsuite/tests/dependent/should_run/NativeClosedFamilyExpected.hs)
and [empty sibling check](../../../testsuite/tests/dependent/should_run/NativeClosedReturnedPair.hs).

The same proof checks after the projection's object becomes available.
The tests inspect the consumer interface to distinguish the object
dependency from the earlier interface-only use. For a binary checked
case, the compiler checks the fallback's kind and dictionaries before
reducing the case. It evaluates the fallback only if the equation
selects that argument. The interface-only test puts an exceptional
empty-list selection there. The `Just` branch succeeds and the
`Nothing` branch reports the exception. [Separate compilation, demand
and dependencies](../../../testsuite/tests/dependent/should_run/NativeClosedReturnedPair.sh).

## M149. Demand only the selected field of a computed pair

A closed ordinary call can use a projection of a computed boxed pair
as an operand when the projection is available through a checked
interface alone. The compiler matches the certified equation against a
promoted pair of distinct variables. When the equation returns one variable, GHC builds
a typed Core case that demands the pair constructor and returns that
field. The interpreter renders the final result. This preserves
Haskell's strictness for boxed constructor patterns and laziness for
their variable and wildcard subpatterns. [Haskell 2010 Report, section
3.17](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-610003.17).

The use still checks the projection's source owner and rewrite rules.
The ordinary pair builder still needs its executable, and its object
remains a recompilation dependency. The tests cover projections of both
fields at the same type. An exceptional unused field stays unevaluated.
Selecting the exceptional field reports the exception. At M149, a
top-level nominal application needed a fully observed operand to persist
in an interface. M153 adds an equation that preserves the copied field.

For a user datatype, a bridge can replay certified constructor
patterns using Core cases and carry unused fields as ordinary Core
binders. It must check the constructor identities, strictness,
exhaustiveness and field kinds while keeping the original interpreter
dependencies. Retaining an unevaluated operand in a nominal type also
needs a representation that survives interface serialization and delays
the rule check until equality uses the equation. [Separate compilation and demand
checks](../../../testsuite/tests/dependent/should_run/NativeClosedReturnedPair.sh).

The direct example is `pick (make 7) :~: 7`, where `make n` returns
`(n, error "unused")`. Forming `type Alias = pick (make 7)` also needs
a type for the operand. A local compiler thunk cannot travel through an
interface. The M149 design gave each suspended field a private family
owned by its producer and indexed by its arguments. M153 implements
that design for a pair with a copied input. Forming the projection in
an alias leaves its equation unused. Using the equation later checks
the applicable rewrite rules.

A separate model checks the proposed equation shape using ordinary
closed families with `UndecidableInstances`. Given an empty family
`Deferred (n :: Nat) :: Char` and `Make n = '(n, Deferred n)`, an
interface-only client proves `Pick (Make 7) :~: 7` with `Refl`. GHC
rejects the claim that the second field is `'x'`. This model supplied `Make` by hand.

## M153. Export a direct projection of a partial pair

An ordinary definition `make n = (n, e)` returns a pair before it
inspects `e`. A dependent module can export
`type Selected = pick (make 7)`. An ordinary client can prove
`Selected :~: 7` with `Refl`. Both modules can compile with only
the producer and projection interfaces. A matching rewrite rule for
either function is checked at the proof. The source alias can still
be formed when a matching rule is visible.

The checker reads the actual saturated pair constructor in solved Core.
Exactly one field must be an unchanged input binder. The other field may
throw, diverge or compute a useful value. GHC publishes the ordinary
producer's checked equation and a private empty family for that field.
The name identifies the producer and field position. Its inputs include
all the producer's type and value arguments. No equation establishes a
particular value for the suspended field.

With only the projection interface available, GHC can keep both nominal
applications when their equations select the copied field. Selecting a
computed field can still use the producer's object and normal closed
readback. Extra computed arguments also stay on the executable path if
forming the nominal operand could force an ignored argument. A top-level
case demanding an input before constructing the pair cannot receive this
equation. [Source example](../examples/open-evaluation/OpenBuildWorker.hs)
and [separate compilation, dependency and rule checks](../../../testsuite/tests/dependent/should_run/NativeClosedPartialPair.sh).

For `bump n = (n + 1, error "unused")`, neither field is an input
binder. With only the projection interface, the direct proof at `7`
still forces the other field. Supplying the projection executable lets
the same proof check at `8`. A certificate for the computed field would
also need to justify the addition at its actual specialization.

A separate control moves the projection into an ordinary module.
Its `number n = case bump n of (x, _) -> x` lets the client prove
`number 7 :~: 8`. Its other projection reports the exception in the
unobserved field when that field is selected. The proof interface
records the object for `bump`. Replacing that object alone with the
version that adds two rejects the unchanged proof at `Refl` with
actual `9` and expected `8`. The interpreter can already compute
the demanded field. The open question is how to represent the
partially observed operand of a projection in an exported type
while retaining the guards on both source functions.

Two other ordinary workers both return `(8, 'x')` at input `7`.
M154 proves equality of their selected numbers. With a matching
rewrite rule on the projection, the same `Refl` reports GHC-83605.
A proposed partial readback must preserve the successful equality
and keep the rule check at proof use. Treating an apparent error call
as proof that a field cannot return would be incorrect. An ordinary
producer rule that rewrites `errorWithoutStackTrace` at `Char` to
`'x'` actually fired in the control.

### When equality needs the whole operand

The M157 prototype computed the number in `pick (bump 7) :~: 8`
without evaluating the unused field. It gave that field a private
name for each observation. An exported alias retained the projection.
A subsequent proof checked its rewrite rules. Changing only the object
for the imported addition invalidated the old result.

A nominal parameter exposes a compatibility problem. Let two ordinary
workers return `(n + 1, 'x')` and put a rewrite rule on `pick`. This
program compiles on M155:

```haskell
same :: N (F.pick (W.firstWorker 7)) -> N (F.pick (W.secondWorker 7))
same x = x
```

Full readback gives GHC the same pair on both sides. It can check
`same` by reflexivity without reducing `pick`. The prototype gave the
two occurrences different unread fields. GHC then tried the projection
equation and rejected the program with GHC-83605. The implementation
stays open. The next design must let a projection leave the sibling
unevaluated and preserve an equality that requires the whole operand.
[Registered compatibility test](../../../testsuite/tests/dependent/should_run/NativeClosedPartialPair.sh).

The same obligation can cross an interface. One module exports
`value :: N A` and `consume :: N B -> Bool`, where the two synonyms
use the different workers. The ordinary client writes only
`consume value`. Both M155 and M158 accept this client with the matching
rule and without either worker's object. A design that delays readback
must preserve this use after the provider has finished compiling.

## M161. Observe a field decision from the loaded executable

The ordinary worker returns `(n + 1, errorWithoutStackTrace "unread")`.
A second version has a rewrite rule that changes the second field to
`'x'`. The rule fires before code generation. With the original
`.hi` and the replacement object, M160 can read the full pair. It can
also check `same x = x` against a different worker that returns the
same pair under a matching projection rule. A decision made solely
from that first interface would miss the change.

The executable experiment adds an explicit observer to the fixture.
A plugin reads the pair constructor after the ordinary Core optimizer.
It follows floated local bindings and checks the demand signature of
the function called by the second field. A saturated call whose
signature excludes returning can supply a partial observation. In the
measured `-O1` build, the original field qualified. The rewritten
character did not. At `-O0`, and with `-fignore-interface-pragmas`,
the imported error function's demand signature was unavailable to the
pass, even though its installed interface included that signature.
At `-O0 -fno-ignore-interface-pragmas` the pass could read the
signature and classify the original field. The rewritten character
still had no such signature.

The observer returns its decision and the worker's result together.
One ordinary client printed `Partial 8` with the first object and
`Whole 8 'x'` with the second. The type checker proved each observation
with `Refl`. Keeping `Worker.hi` fixed and replacing only `Worker.o`
invalidated the first proof. Run the experiment with the fork's
self-hosted compiler on the Linux dynamic build:

```sh
bash docs/dependent-haskell/examples/closed-evaluation/check-readback-entry.sh \
  "$PWD/_build/stage1/bin/ghc" "$PWD/_build/stage1/lib"
```

The fixture calls its observer explicitly. Direct `fst (bump 7)`
still needs a compiler representation that preserves the original
projection and its proof-time rewrite checks. A compiler-generated
observer must refer to the same worker implementation at the call
site, including the implementations of any helpers used to classify
a field. The probe exercises native object changes. Establishing
that correspondence for bytecode and custom compiler hooks remains open.
[Core plugin](../examples/closed-evaluation/PairReadbackProbe.hs)
and [source proofs](../examples/closed-evaluation/check-readback-entry.sh).

Changing only a helper object shows why this matters. The worker calls
an imported `NOINLINE` function for the second field. With the original
helper, the observer reported `True` and forcing the field raised an
exception. With a replacement helper, the same worker object still
reported `True`, but forcing the field returned `'x'`. Returning the
decision from the worker solves the worker-only replacement in this
experiment. Using it to omit a field also requires validating the
helper used to justify that decision.

The compatibility test also computes the two sibling characters in
different ways. At input `7` both return `'x'`. The identity function
still checks under the projection rule. At input `8` the siblings are
`'a'` and `'b'`. Although both selected numbers are `9`, GHC reports
GHC-83605 when equality would require the guarded projection.
[Input-sensitive reflexivity test](../../../testsuite/tests/dependent/should_run/NativeClosedPartialPair.sh).

## M165. Named functions as closed operands

A complete closed call can pass an imported ordinary function as an
operand. For example, `Prelude.map Prelude.not '[ 'True, 'False]`
produces `'[ 'False, 'True]`. The same planner accepts a separately
compiled worker and callback. A polymorphic callback can take its type
from the other operands. The planner requires an ordinary arrow with no
class constraints on the callback.

Lookup keeps ordinary type bindings ahead of a term fallback. The
typechecker checks the callback against the enclosing call before the
interpreter runs. The checked callback enters the same runtime Core
expression as the other operands. The compiler records its interface and
linker dependencies and reifies the final result. Function-valued results
and open higher-order aliases remain outside this path.

## Native prototype for a computed pair field

A private compiler has checked the direct source proof
`pick (bump 7) :~: 8` for `bump n = (n + 1, error "unread")`.
An ordinary client checked the exported alias without the projection's
object file. Demanding the other field raised the expected exception.

The producer emits a private entry from its final Core. The entry can
identify a nonreturning field or refer to the entry of a specific
imported function. GHC follows these references during type checking.
A positive result lets it evaluate only the selected field. A missing
entry selects full readback. An ordinary program can therefore link
against an older transitive helper that lacks the private entry.
Replacing only that helper object or a registered package archive
caused GHC to recheck the proof. When both fields returned, comparing
the complete operand preserved reflexivity under a projection rule.

The experiment used a private native build of `GHC.Internal.Err`.
The paired compiler and interpreter check either projection of a
supported scalar pair. They fall back quietly when an older object lacks
the optional symbol. If an ordinary symbol is missing, GHC reports
GHC-70565 at the source call. A per-module table records scalar relays.
In one eight-function module the object and interface measured 9,992 and
1,925 bytes, compared with 9,768 and 1,852 before the change. Giving each
function its own entry used 15,376 and 2,279 bytes in that module.
Shared compiler integration remains open. [Previous object and equality
controls](#m161-observe-a-field-decision-from-the-loaded-executable).
