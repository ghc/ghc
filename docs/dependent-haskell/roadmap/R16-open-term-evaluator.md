# R16. Open term evaluator

The upstream example has a type variable whose value the compiler cannot
know. Identity can return it unchanged. Serokell proposes normalization
by evaluation for this problem. R13 includes this card. The cards for
lambdas, lets and cases in types also depend on R16 and dependent Core
(R05). [Serokell R16](https://ghc.serokell.io/dh#open-term-evaluator)

```haskell
x :: forall a. Num a => id a
x = 42
```

With `ExperimentalDependentTypes` and `ScopedTypeVariables`, the compiler
accepts this signature. At `Int`, the value is `42`. Before M100 the
renamer reported GHC-68947 at `id`. The same program now also works with
`Prelude.id` and with an independently defined function of the same type
whose body returns its argument. [Source examples](../examples/open-evaluation/OpenSource.hs).

With `TypeApplications`, a caller can also write `Proxy @(Prelude.id a)`
for a rigid `a`. GHC checks whether the whole visible argument is complete
before it reduces the call.

## Selecting between two type variables

```haskell
first :: forall a b. a -> b -> const a b
first x _ = x
```

The compiler reads the ordinary definition of Prelude's `const`. It gives
each distinct source type variable a symbolic operand and checks which one
the function returns. The same variable in two argument positions has one
identity. An independently defined `second _ y = y` selects `b`. Changing
the pattern to `second !_ y = y` makes Core inspect the first operand.
When `a` is rigid, evaluation stops at that case and GHC-70566 names `a`
at the call. The source harness runs both projections with `Bool` and
`Int` and checks the strict version under separate compilation and
`--make`. [Producer](../examples/open-evaluation/OpenProducer.hs)
and [consumer](../examples/open-evaluation/OpenSource.hs). The
[strictness references](../REFERENCES.md#strict-and-lazy-open-projections)
give the Haskell and Agda precedents for this check.

## Selecting a type with a known character

An ordinary producer can select between two unknown types by inspecting a
known character. The two exported functions choose different branches.

```haskell
route :: Char -> t -> t -> t
route 'x' l _ = l
route _   _ r = r

routeLeft, routeRight :: t -> t -> t
routeLeft  = route 'x'
routeRight = route '!'
```

In the client, the two operands of `OpenProducer.routeLeft a b` are
distinct rigid type variables. The type checker reads the worker's Core,
matches the character, and returns `a`. With `routeRight`, it returns
`b`. Calling the two projections on `False` and `42 :: Int` returns
`False` and `42`, respectively. If the selected path demands a rigid
operand, GHC reports that operand at the call.
The observer checks the wired-in character data constructor in the
worker's Core. GHC desugars the character alternatives into a case on
its `Char#` field. In GHCi these provider modules retain their unfoldings
in bytecode mode. The observer reads through a debugger breakpoint when
checking the type.
[Worker](../examples/open-evaluation/OpenAlias.hs),
[client](../examples/open-evaluation/OpenSource.hs)
and [separate-compilation test](../examples/open-evaluation/check-source.sh).

## Inferring a character from a pattern

```haskell
upper :: Proxy _c -> Proxy (Data.Char.toTitle _c)
upper (_ :: Proxy 'a') = Proxy @'A'
```

The pattern determines `_c`. GHC runs the imported function on `'a'`
and checks the body against the resulting `Proxy 'A'`. If the body
claims `'Z'`, the error at `Data.Char.toTitle` reports both characters.
If the pattern leaves `_c` unknown, GHC reports the blocked input there.
An ordinary client sees `upper :: Proxy 'a' -> Proxy 'A'` in the
interface. [Source tests](../../../testsuite/tests/dependent/should_run/NativeOpenRetry.hs)
and [interface test](../../../testsuite/tests/dependent/should_run/NativeOpenRetryInterface.sh).

A partial signature with no leading constraints owns the pending calls
on its named wildcards. After solving the binding group's ordinary
constraints, GHC retries the calls whose operands are literals. A result
can make a second call ready. The group must finish every call before
generalization. A binding plan that skips this inference step rejects a
pending call at its source. [Solver and design note](../../../compiler/GHC/Tc/Solver.hs)
and [Agda's constraint ownership](../REFERENCES.md#retrying-a-check-during-inference).

## Reducing a rigid variable

Use the actual typed unfolding for Prelude's `id`. A source skolem
`a :: Type` stands for a type chosen by the caller. Give the evaluator
a fresh neutral variable of the same kind and keep the correspondence
to that skolem. After evaluation, only that variable can be read back
as `a`. A same-spelling variable with a different identity must fail the
check. This experiment has no runtime representation for types.

An unfolding is the typed Core body that a producer makes available in its
interface. It can refer to other bindings. On this build GHC shares the
implementations of `id` and `breakpoint`. The evaluator follows the alias
and records both uses. With a home-module alias, changing only its worker
makes the unchanged client recheck the call. A missing unfolding is an
error at the call. The client can compile at `-O0`. The fixture's ordinary
producer uses `-fno-omit-interface-pragmas` to supply its body.
[Separate compilation and compilation with `--make`](../examples/open-evaluation/check-source.sh).

The GHC API test checks eleven observations. They include following that
alias, distinguishing same-spelling symbolic operands by identity, and
preserving a neutral case. The source test exercises reification to the
original skolem through type checking. Retrying the API observation with a
supplied definition models resolution of a blocked variable. The probe
does not modify the GHC solver. The compiler bounds the number of steps
using `-freduction-depth`. Only a successful reduction to the original
rigid variable enters the source type. A residual case or application
still needs a representation and evidence before it could enter Core.
[Evaluator](../../../compiler/GHC/Core/OpenWhnf.hs) and
[type elaboration](../../../compiler/GHC/Tc/Gen/OpenTypeEval.hs).

## Evidence required

- [x] Reduce an ordinary polymorphic identity applied to a rigid variable.
      Preserve its binder identity under shadowing. Test a projection whose
      unused argument diverges.
- [x] At the evaluator API, suspend a case on a rigid scrutinee with its
      binder and alternatives. Observe the same case after supplying a
      previously unknown definition.
- [x] Retry a source call on a partial signature's named wildcard after
      inference determines its literal value. Reject an unresolved input
      at the call before generalization.
- [ ] Keep a call on a rigid parameter in an exported equality constraint.
      Check the equality when an importing client supplies a literal. The
      caller chooses this input after the producer has been compiled.
- [ ] Specify the typed input, lexical environments and which reductions
      produce weak-head normal form. Preserve Haskell's demand behavior.
      A strict pattern cannot discard the need to inspect its argument.
- [ ] Establish the evidence required when an application survives into
      Core. The fork's `TermAppCo` relates corresponding applications of a
      retained function. It supplies congruence alone. A beta step needs a
      checked justification if it changes a type that already appears in Core.

GHC issue #27413 explicitly asks which typed representation to evaluate
and how to handle metavariable holes. Agda's evaluator records why a
reduction is blocked. These are starting points for the first two checks.
[GHC #27413](https://gitlab.haskell.org/ghc/ghc/-/issues/27413)
and [Agda's weak-head reduction](https://github.com/agda/agda/blob/7273757e5ee247d321270ef76f792c6156613b7c/src/full/Agda/TypeChecking/Reduce.hs#L616).

## When to retry a blocked case

Consider an ordinary Core case that evaluates `f x` and returns `k` in
its default branch. If `f` is an inference variable, assigning `x` alone
gives the checker no reason to retry. If the solver equates `f` with a
second inference variable, the next observation should wait on that
second variable. A rigid `f` stays unknown throughout this check.

A separate API experiment exercised these cases with seven assertions and
actual GHC metavariable cells. The fixture supplied the assignments and
their interpretation as symbolic Core values. M104 checks for ready literals at the existing
generalization step. A general evaluator still needs to identify the
variable that blocks an observation and wake the affected constraints.
GHC already records unification and revisits affected constraints in
`kickOutAfterUnification`. A reduction that survives into FC also needs
checked nominal evidence for its endpoints.
[GHC solver](../../../compiler/GHC/Tc/Solver/Monad.hs),
[Eisenberg's Core Lint](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/coreSyn/CoreLint.lhs#L498-L548)
and [Agda blockers](https://github.com/agda/agda/blob/7273757e5ee247d321270ef76f792c6156613b7c/src/full/Agda/Syntax/Internal/Blockers.hs#L26).

## An equality that crosses a module boundary

Draft proposal #509 gives this producer. The consumer calls it with
`Proxy @'x'` and expects `'X'`.

```haskell
p3 :: forall c uc. (Data.Char.toUpper c ~ uc, KnownChar uc) => Proxy c -> Char
p3 _ = charVal (Proxy @uc)
```

The producer checks the body with the equality and dictionary supplied
by its caller. The caller must establish the equality after it chooses
`c`. The application must therefore survive in the producer's interface.
M104 rejected this signature at `Data.Char.toUpper`.
[Proposal #509](https://github.com/ghc-proposals/ghc-proposals/pull/509).

Start with an ordinary imported `Char -> Char` that changes one literal
and returns its input in the default branch. This is the shape of the
[Unicode worker](../../../libraries/ghc-internal/src/GHC/Internal/Unicode/Char/UnicodeData/SimpleUpperCaseMapping.hs)
behind the installed library function. This example has three checks.

- A rigid operand must stay unevaluated. In particular, choosing the
  default branch at this point could yield the wrong result.
- For a ground operand, Core Lint must be able to check the claimed
  reduction against the producer's typed definition. Check both the
  literal branch and the default, including a deliberately false result.
- A client must read the application from the interface. Changing the
  producer's selected branch without changing its signature must cause
  the unchanged client to check its equality again.

The [GHC API regression](../../../testsuite/tests/ghc-api/OpenCharBranchWitness.hs)
reads the desugared Core for an ordinary producer and its importer. The producer changed `'x'` to `'X'`, changed `'é'` to `'É'`
and returned its input in the default branch. The observer selects all three results and stays blocked on the exact rigid operand.
The test records a proposed ground reduction and replays its selected
alternatives against the linted definition. It rejects a changed
producer, a missing step and a wrong result. The trace lives in memory.
M109 uses an ordinary closed-family axiom to supply nominal evidence for this finite case.

The default branch exposed a demand requirement. GHC shares the default
code in a local function that takes an unboxed unit. The observer could
enter it with the constructor `(##)`. A test substituted an argument of
the same type that first inspects a rigid character. That argument could
not be discarded. Inspect the producer with `-ddump-ds-preopt` to see the
continuation before optimization.
[Desugaring shared pattern-match failure](../../../compiler/GHC/HsToCore/Utils.hs).

General open case reduction still needs a checked representation in
Core. The current `TermAppCo` proves congruence under one local head.
GHC's built-in literal rules give an example of checking proof endpoints.
A checked step for a case must also identify the imported definition and
preserve its demand on the scrutinee. [Core Lint](../../../compiler/GHC/Core/Lint.hs),
[GHC's literal rules](../../../compiler/GHC/Builtin/WiredIn/TypeLits.hs)
and [research references](../REFERENCES.md#checking-a-reduction-across-an-interface).

Fresh GHC API processes replayed the character cases from the ordinary
producer's `.hi` alone. A rebuild with the same body preserved the
declaration hash. Changing one branch invalidated the old hash, even
when a reader inspected another branch. The check failed as expected
when the producer omitted its optional unfolding. Persistent evidence
therefore needs a producer declaration that supplies the checked
information under the supported compilation flags.

For finite character mappings, R16 reuses the closed-family machinery
from R13. The producer derives one literal equation per branch and a final
identity equation. Core Lint can check the resulting nominal coercion
against that axiom. General open cases can have expressions outside this
fragment. [Source tables](../../../compiler/GHC/Tc/Gen/SourceTable.hs)
and [closed-family branch selection](../../../compiler/GHC/Core/FamInstEnv.hs).

The original package interfaces showed why producer publication matters.
`GHC.Internal.Unicode.toUpper` exports an unfolding that refers to
`toSimpleUpperCase`. That function exports a wrapper that calls
`$wtoSimpleUpperCase`. The worker's interface has its type and
strictness information but omits its body. The complete table is
present in the desugared Core of the original library source.
[Unicode worker source](../../../libraries/ghc-internal/src/GHC/Internal/Unicode/Char/UnicodeData/SimpleUpperCaseMapping.hs).

The [reproducible example](../examples/certified-character-case/README.md)
derives the equations from that desugared worker. A separate module exports
`p3 :: (Upper c ~ uc, KnownChar uc) => Proxy c -> Char`. Its client
typechecks with only interfaces available. It gets the mapped result for
`'x'`, for Unicode, and for the last literal in the table. It gets the
input back for `'!'`. GHC rejects both a wrong ground equality and a
default proof for an unknown `c`. Editing the original worker and
regenerating the family changes the unchanged client's result from
`'X'` to `'Y'`. This experiment established the finite case rule before
GHC could publish the family itself.

## Direct character cases in producer interfaces

An extension-enabled producer can now export `upper :: Char -> Char` with
a finite set of literal mappings and an identity default. GHC derives the
closed family from the desugared function and records it in the term's
`SourceTableId`. An exporter can write `CharacterCase.upper c ~ uc` in a
signature. A separate client can solve that constraint at a known character
with the extension switched off and optional unfoldings omitted. It still
cannot assume the identity equation for an unknown input.

The ordinary client reads only the interfaces during checking. Changing
`'x'` from `'X'` to `'Y'` in the worker changes the unchanged client's
output. A visible orphan rule can also change the worker. In that control
the ordinary runtime call returned `'Z'`, and GHC reported GHC-83605 at
the call that requested the equality. The producer, exporter and client also
passed a separate `-fno-code -fwrite-interface` check.
[Producer](../examples/open-evaluation/CharacterCase.hs),
[exporter](../examples/open-evaluation/CharacterExport.hs)
and [three-module regression](../../../testsuite/tests/dependent/should_run/NativeOpenCharFamily.sh).

## The installed Unicode function

The fork's Hadrian build enables equation publication in the two
`ghc-internal` modules that implement `Data.Char.toUpper`. The Unicode
module's `toUpper` is an exact alias of `toSimpleUpperCase` in another
module. It publishes a forwarding equation. The public `Data.Char`
module re-exports the same binding.

The exporter can now use the signature from proposal #509 verbatim.
The package regression makes a temporary registration with the installed
unit IDs. It supplies only `ghc-internal` interfaces to the exporter and
client during compilation. The exporter interface contains the equality
under the Unicode module's family name. The link then uses the ordinary
libraries. The client checks `'x'`, `'é'`, the default at `'!'`, and the
last mapping at U+1E943. Their result code points are 88, 201, 33
and 125217. At `p3 @'x' @'Y'`, GHC reports the actual and required
characters at the call.
[Exported signature](../examples/open-evaluation/UnicodeExport.hs),
[ordinary client](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledUpper.hs)
and [separate-package test](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledUpper.sh).

## Projecting a pair of unknown types

`first :: forall a b. a -> b -> Prelude.fst '(a,b)` checks with the body
`first x _ = x`. GHC reads the ordinary library definition. Its case
finds the pair constructor and returns the caller's original `a`.
`Prelude.snd '(a,b)` selects `b`. Changing an imported projection from
first to second changes the type of its unchanged client.
[Source examples](../examples/open-evaluation/OpenSource.hs)
and [the ordinary projections](../../../libraries/ghc-internal/src/GHC/Internal/Data/Tuple.hs).

A variable `p :: (Type, Type)` supplies no constructor. A function can
ignore `p` and return another argument of kind `Type`. For a producer
that supplies only an ordinary unfolding, asking for `OpenProducer.pairFirst p`
stops at a case on `p` and reports GHC-70566. The observer leaves a
divergent field unevaluated when the function selects the other field.
For installed Prelude projections, GHC can now preserve the application
in an exported constraint. [Source diagnostics](../examples/open-evaluation/check-source.sh)
and [Core observer checks](../../../testsuite/tests/ghc-api/OpenTermWhnf.hs).

For an exported `fst p ~ x`, M119 uses a checked family equation for
the actual projection. Its kind supports both `'(Int,Bool)` and `'( 'True, 'x')`. A general R16 implementation
must also represent cases that have no such family. It needs a stable
reference to the definition and nominal evidence for the alternative
selected when the caller supplies `p`. The observer's suspended case
contains a local environment and cannot serve as interface evidence.
[Function promotion](https://ghc.serokell.io/dh#function-promotion)
and [the observer's case representation](../../../compiler/GHC/Core/OpenWhnf.hs).

### Interface experiment and rewrite rules

A separate compiler based on `7d109d984b` published a nominal equation
for an ordinary user-defined projection. It checked the typed Core for
a case on the pair with a single alternative returning one of its field
binders. It preserved the order and visibility of the function's type
parameters. An ordinary client used the exporter and producer interfaces
to select fields at `Type`, `Bool` and `Char`. Changing the producer
from first to second invalidated the unchanged client. A function that
also forced the ignored field did not receive the equation.

The experiment exposed a compatibility requirement. Consider an ordinary
`choose :: (a, a) -> a` that returns the first field. The published
compiler checks the following equality, even if a rule for
`choose @Bool` appears later in the module or in a declaration splice.

```haskell
proof :: Prelude.id (Worker.choose (Prelude.quotRem 17 5)) :~: 3
proof = Refl
```

Recording only the owner of the family equation made GHC-83605 reject
that proof. Reusing the ordinary evaluator for an already imported rule
fixed the imported case. Recording the concrete first type-argument
head at nominal reductions also preserved the proof for both the later
written and splice-generated `Bool` rules. Rules that could change a
nominal `Bool` proof still caused that proof to be rejected.

A rule can also match the ground application. This rule preserves the
result of the projection. It appears after the proof in the same module.

```haskell
{-# RULES "choose/first" [~1] forall x y. Worker.choose @Natural (x,y) = x #-}
```

The published compiler evaluates the ground call and accepts the module.
The first candidate used nominal evidence and rejected it at `Refl`.
The next candidate ran the ordinary projection when its home-module object
was available. Both compilers then accepted the rule and recorded it in
the client interface. Without the object, the candidate continued to check
an open equality from the producer and exporter interfaces alone.

One-shot compilation exposed a further distinction. A worker object can
remain on disk after an object that it imports has been removed. With
only the interfaces present, the client accepted the nominal proof.
Restoring just the worker object made the candidate try to execute it.
GHC-70565 then named the missing transitive object. Restoring both objects
made the same proof pass.

The revised bootstrap compiler uses GHC's linker dependency plan before
it chooses ordinary evaluation. All three cases now pass. In one-shot
compilation a serialized `.gbc` and `.hi` also suffice. A worker already
loaded by a Template Haskell splice remains executable while its object
is absent. The probe can discover embedded Core without compiling it
or extracting foreign objects. The interpreter loads the code only when
the type checker selects ordinary evaluation.

Another regression test supplies a checked `Data.Char.toUpper 'a'` call
as input to an ordinary function returning a pair. A projection of that
pair must preserve a later matching rule. The published compiler accepted
it, the first candidate rejected it with GHC-83605, and the revised
bootstrap compiler accepts it. The client interface retains the rule.

### Exporting a projection of an unknown pair

The in-tree compiler certifies the ordinary definitions of
`Prelude.fst` and `Prelude.snd` in `GHC.Internal.Data.Tuple`. The
ordinary boxed case in each definition returns one field unchanged.
The exported equation reduces at a pair constructor. When the pair
`p` is unknown, the application can remain in an exported constraint.

```haskell
first :: forall k l (p :: (k, l)) (x :: k).
         Prelude.fst p ~ x => Proxy p -> Proxy x -> x :~: Prelude.fst p
first _ _ = Refl
```

The exporter uses the given equality. Removing it makes GHC reject
`Refl` at the binding. A client can use `first` and `second` with
`ExperimentalDependentTypes` off. It checks `Int` and `Bool` for
`'(Int, Bool)` and checks `'True` and `'x'` for `'( 'True, 'x')`.
It reads only the producer and exporter interfaces while checking
these equalities. It links with the original objects to run the program.
The package test also preserves previously accepted calls to `fst` and
`snd` on the result of `quotRem` under later rewrite rules.
[Exporter](../examples/open-evaluation/PreludePairExport.hs),
[ordinary client](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledPair.hs)
and [installed package check](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledPair.sh).

Both the internal interpreter and a prebuilt matching external interpreter
passed the installed check. This GHC normally builds the external interpreter
at startup. Building it requires the original libraries. Supplying the
executable with `-pgmi` allows the test to remove those libraries from
the consumer's package database.
[Interpreter setup](../../../compiler/GHC/Runtime/Interpreter/Init.hs)
and [the closed evaluator](R15-closed-term-evaluator.md#returning-a-pair).

### Composing a projection with a swap

The producer check now admits a strict case that constructs a boxed
pair from its two bound fields. It records which input supplies each
output. This admits `swap (x,y) = (y,x)` and `copyFirst (x,y) = (x,x)`.
Forcing an unused field changes which inputs terminate and disqualifies
that definition. A client with a constructor in hand can reduce both
checked cases even when the fields are unknown.

```haskell
known :: forall (x :: Bool) (y :: Char). Proxy x -> Proxy y
      -> Prelude.fst (Data.Tuple.swap '(x, y)) :~: y
known _ _ = Refl
```

With an unknown pair `p`, the two constructor equations both get stuck.
An interface that exports `get :: Proxy q -> Proxy (Prelude.fst q)`
also needs the equality when a later client instantiates `q` with
`Data.Tuple.swap p`. Rewriting the original source expression would miss
that use.

The proof uses a nominal Core coercion for the law and its dual.
The checker reads the actual field order in the three family equations.
The compiler with only the constructor equations rejected both proofs
at `Refl` with GHC-83865. The experimental rule passed both, including
a client that obtained the composition by substituting into an imported
signature. A matching local rule and an orphan rule introduced later
both invalidated the corresponding source proof. The structural checker
also rejected the wrong field of a homogeneous pair and a forged
representational premise.

The ordinary test client has the extension off. It imports `get`,
`swapIndex`, and `secondIndex` from the dependent exporter. The type
of `get (swapIndex p)` contains the composition, even though the client
only manipulates proxies. The test asks GHC to give it the same index
as `secondIndex p`. Adding a local rule that makes `fst` select the
second field causes GHC-83605 at the use in the ordinary client.
Both sides of the checked equality force the same pair shell
and select the same field. An arbitrary boxed pair can diverge when
forced.
[Core law](../CORE.md#nominal-evidence-for-a-composition-of-strict-pair-cases),
[ordinary client](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledPair.hs)
and [Agda's projection and eta rules](../REFERENCES.md#closed-constructor-arguments-and-composition-of-strict-pair-cases).

### Following a projection through strict pair constructors

`duplicateFirst (x, _) = (x, x)` produces a pair whose two fields come
from the same input. Thus `snd (duplicateFirst p) ~ fst p` holds for
an unknown `p`. Replacing the right side with `snd p` fails even when
the two fields have the same kind. The actual branch binders identify
the field. Their kinds alone cannot identify it.

A proof can cross several strict cases on either side of the equality.
For example, applying `duplicateRight` after `swap` makes both fields
refer to the original first field. When two paths agree at a common
input, the proof can leave that input opaque. The solver records only
the source equations needed to reach that agreement. In an ordinary
client, the two projections of `duplicate (exchange p)` still agreed
under a rewrite rule for `exchange`. Adding a rule that could change the
fields of `duplicate` invalidated the unchanged client at its call.
[Compiled source examples](../../../testsuite/tests/dependent/should_run/NativeOpenPairCoercionUsage.hs)
and [checked Core premises](../../../testsuite/tests/dependent/should_run/NativeOpenPairCoercion.hs).

The same interface format can carry the new named proof. A consumer
built before this change can still read newly compiled proofs of the old
exchange law. It reports an unknown rule when it encounters a new
reconstruction proof. The new reader also accepts existing exchange
proofs. [Core contract](../CORE.md#nominal-evidence-for-a-composition-of-strict-pair-cases).

### Observing an independent local definition

The observer can also use a body supplied by R21. In the same source
module, `type Selected a b = LocalOpen.select a b` reduces to `a` when
the ordinary definition is `select x _ = x`. The two symbolic operands
each have kind `Type`. This lets the caller supply two different types
to the ordinary homogeneous function `select :: a -> a -> a`. If the definition
instead demands its unknown argument, observation reports that demand
at the call. The compiled synonym can be used from an ordinary module.
[Source example](../examples/open-evaluation/LocalOpen.hs)
and [declaration and splice boundaries](R21-dependency-analysis.md).

M128 extends the same observer to chains of local helpers. It records
each definition it unfolds. An undemanded argument can refer to another
checked helper without adding that helper to the proof's rule dependencies.
[Two local selections](../examples/open-evaluation/LocalOpen.hs)
and [helper and rewrite-rule controls](R21-dependency-analysis.md#m128-following-a-local-dependency-chain).

M131 applies the existing open reduction limit to recursive value groups.
A terminating observation can use a mutually recursive function even
when a different call to that group would diverge. A demanded
monomorphic recursion binder contributes its source function to the
late rewrite-rule check.
[Recursive source examples](R21-dependency-analysis.md#m131-mutually-recursive-value-bindings).

### Eliminating a known user constructor

A data declaration can supply the outer constructor while its fields
remain symbolic. For example, `pick ('LeftFields a b)` selects `b`
from the second field. The ordinary producer publishes its Core
unfoldings in the interface. A client with an interface that omits
the needed unfolding gets GHC-70566 at the call.
The same ordinary `pick` can select `b` from
`'RightFields b a`, where it is the first field. Neither call needs a
runtime value for `a` or `b`. A nested constructor can expose another
case without forcing its fields.
[Ordinary producer](../examples/open-evaluation/OpenConstructorWorker.hs)
and [type-level calls](../examples/open-evaluation/OpenConstructorSource.hs).

The source checker uses the promoted constructor's resolved DataCon,
its parent kind, and its field types. It builds a saturated ordinary
worker with suspended fields. The Core observer uses the same worker
check. It selects a case by DataCon identity and checks the alternative
binders before binding fields in declaration order. It also recognizes
a nullary ordinary worker without reading an unfolding. A rigid
scrutinee still blocks the case. A strict pattern on a symbolic field
still blocks the reduction. This slice checks lazy boxed constructors
with ordinary representations and leaves strict or unpacked constructor
fields, GADT evidence, and representation-changing wrappers outside
the operand decoder.
[Core observer](../../../compiler/GHC/Core/OpenWhnf.hs)
and [checked operand decoder](../../../compiler/GHC/Tc/Gen/OpenTypeEval.hs).

The compiler returns only a supplied rigid type. It does not create
a new coercion or an equation for an unknown constructor. The existing
source-rule checker checks the definitions that the observer demands,
including imported definitions. It records the first known type argument
of each demand, including a repeated demand of a cached definition. An
unknown occurrence keeps all rules for that definition in scope. The
regression checks an interface-only exporter, both field orders, and
strict and rigid demand. It also checks matching and disjoint rules on
the imported eliminator, including two demands at different types.
Changing one field's strictness makes the unchanged exporter recompile
from the new interface.
[Regression](../../../testsuite/tests/dependent/should_run/NativeOpenConstructor.sh).

### Reading back an ordinary constructor result

An imported ordinary builder can return a boxed constructor with
symbolic fields. In [OpenBuildSource.hs](../examples/open-evaluation/OpenBuildSource.hs),
`Make a b` reduces to `'Shell a b`. The example also returns a nested
constructor and returns an existing rigid operand of kind `Shell Type Type`.
An ordinary client can import the elaborated type synonym and proof.

The observer suspends each lazy field in its own lexical environment.
When the result must become a type, readback observes those fields as
needed to reconstruct the promoted constructor. The initial observation
and all field observations share the same reduction budget. A field
which demands an unknown value or exceeds that budget stops readback.
A hidden constructor stops readback before the observer demands its fields.

Readback checks the result kind, constructor identity, field types,
and ordinary constructor visibility. The typechecker records actual
unfolding uses and the declarations of the constructors it returns.
That records rewrite-rule dependencies on the demanded term definitions
and interface dependencies on the returned data declarations. This slice
covers lazy boxed constructors with ordinary representations. It adds
no coercion for a neutral computation.


The same readback accepts an ordinary boxed pair from two rigid fields,
or from a swap applied to a known promoted pair. Pair syntax uses the
wired-in constructor. Without an exported equation, the observer stops
at a case on an unknown pair.

## M142. Exporting a swap from an ordinary module

```haskell
type Swap :: (k, l) -> (l, k)
type Swap p = B.swapPair p
```

The ordinary producer defines `swapPair (x, y) = (y, x)`. GHC inspects
the desugared definition at the end of typechecking. It can publish the
existing boxed-pair equation even with the dependent extension off in
the producer. The dependent exporter needs no constructor for `p`.
Its ordinary client checks `Swap '( 'True, 'x') :~: '( 'x', 'True)`
from interfaces alone. The exporter cannot prove `Swap p :~: p`
for an arbitrary homogeneous pair.

Changing the producer body to force the first field before constructing
the result removes the equation. GHC rejects the unchanged export after
reading the new interface. Arbitrary open cases still need an account
of the scrutinee, branch selection and demand.
[Ordinary producer](../examples/open-evaluation/OpenBuildWorker.hs),
[exporter](../examples/open-evaluation/OpenBuildSource.hs)
and [separate compilation and strictness controls](../../../testsuite/tests/dependent/should_run/NativeOpenBuild.sh).

The equation also works when the producer has no `INLINE` pragmas and
omits its executable unfolding. For a pair of two unknown Booleans,
the client rejects a local rule that changes the Boolean swap. A
specialization for pairs of characters cannot match those inputs. GHC
checks the ordinary function's type arguments and reads the solved type
of a rule binder before final zonking to distinguish those cases.

An ordinary interface built by M139 has no pair equation. An unchanged
home producer must therefore run through the new compiler once. The
self-recompilation fingerprint records this change for ordinary home
modules, including modules that publish no equations. A second build
can reuse their interfaces. The change preserves the binary interface
format, so an ordinary client can still read a prebuilt M139 interface.
Obtaining a new equation from a binary-only package requires rebuilding
its producer. The two-compiler check accepts paths to the M139 compiler,
the current compiler and the compatible library directory. It checks
ordinary binary-only use, source upgrade and reuse without a file edit.
[Recompilation key](../../../compiler/GHC/Iface/Recomp/Flags.hs)
and [upgrade check](../examples/open-evaluation/check-pair-upgrade.sh).

## M144. Deferring a projection over an ordinary sum

```haskell
data Choice a b = LeftFields a b | RightFields b a

pick :: Choice a b -> b
pick (LeftFields _ y) = y
pick (RightFields y _) = y
```

The ordinary producer exports the datatype, its constructors and the
function. A dependent module can now define `type Pick p = W.pick p`
with an unknown `p :: W.Choice Bool Char`. A later ordinary client
can prove both `Pick ('W.LeftFields 'True 'x') :~: 'x'` and
`Pick ('W.RightFields 'y' 'False) :~: 'y'` from stripped interfaces.
Each equation matches one constructor. With only the variable `p`,
the application stays in the exported type.

At M144, GHC checked one direct Core case on the function's argument. Every
constructor must have an alternative that returns one of its bound
fields. The datatype must be local and exported with every constructor.
Its fields must be lazy boxed values at its type parameters. A hidden
constructor, extra demand, nested match or rewritable producer can
block publication. A consumer also checks applicable rewrite rules
before using the equation. The order of explicit type arguments follows
the function signature. [Producer](../examples/open-evaluation/OpenConstructorWorker.hs)
and [separate clients, rule checks and strictness controls](../../../testsuite/tests/dependent/should_run/NativeOpenConstructor.sh).

When upgrading from M142, GHC checks an old home interface for a local
exported parameterized sum with a field in each constructor. A possible
producer recompiles once, including when the Core check yields no
equation. An abstract or unrelated module can reuse its old interface.
Binary-only packages need a producer rebuild to gain the new equation.
[Two-compiler upgrade check](../examples/open-evaluation/check-sum-upgrade.sh).

## M146. Nested constructor cases with a fallback

GHC can now publish a residual for the ordinary
`pickNested :: forall a. Nested a -> a -> a`. The producer's checked
Core tests the outer `Nested` and then its `Maybe` field. One equation
matches `Nested (Just x)` and returns `x` without inspecting the
fallback. The other matches `Nested Nothing` and returns the fallback.
An ordinary downstream module can check both branches through stripped
interfaces. A residual remains stuck while either demanded constructor
is unknown.

The constructor projection check follows exhaustive Core cases on
an argument or a field those cases bind. Each equation repeats each
constructor test along its branch. A branch can select an in-scope
field or the optional second argument. The same check accepts a
single-constructor projection or a binary selector with no nested case.
A branch can leave other fields unknown and can return the fallback
after a nullary constructor, including one with a phantom parameter.
The check requires a local exported input datatype and visible
constructors for the ordinary boxed datatypes in the patterns. It
rejects strict fields, evidence, default alternatives and cases on
expressions other than bound operands. Rule checks use the invisible
prefix before the one or two visible operands.
[Producer](../examples/open-evaluation/OpenConstructorWorker.hs)
and [interface, demand and rule tests](../../../testsuite/tests/dependent/should_run/NativeOpenConstructor.sh).

An older interface may lack these equations even if it passed an earlier
upgrade check. M144 reused a local one-constructor wrapper built with
M142. The current compiler checks both generations when the old
interface declares a parameterized datatype with its constructors
exported. A module that hides its constructors can keep its old interface.
The next invocation reuses the rebuilt modules. [Three-compiler upgrade](../examples/open-evaluation/check-case-upgrade.sh).

A branch that constructs a result, such as `firstMaybe (More x _) =
Just x`, needs to represent that constructor and its symbolic fields on
the right-hand side of the equation. M146 accepted a bound field or
fallback there. M148 adds constructed results. General normalization
of a case on an unknown term remains open.

## M148. Constructor results from a checked case

An ordinary `firstMaybe :: Spine a -> Maybe a` builds `Nothing`
after `Empty` and `Just x` after `More x rest`. A dependent module
can export the residual with a rigid input. An ordinary consumer can then
prove both equations from stripped interfaces, including the second
equation with a rigid tail. Building `Just x` adds no demand on either
field. The same check covers nested results such as `Just (Just x)`.

GHC checks a saturated application of the actual DataCon worker. It
checks the constructor's type arguments, field kinds, result kind,
ordinary lazy representation and visibility. Result expressions can use
scoped fields and build further checked constructors. A strict result
constructor or a private constructor cannot enter the published
equations. A strict match on the input tail still blocks the certificate.

GHC rechecks potentially eligible local producers once when upgrading
from the previous constructor case format. The migration uses the same
necessary local datatype check. Binary-only producers still need a
rebuild to publish the new equations.
[Stripped-interface tests](../../../testsuite/tests/dependent/should_run/NativeOpenConstructor.sh).

Changing only the constructed result from `Just (Just x)` to
`Just Nothing` invalidates the client's previous equality at `Refl`.
A result such as `Just (helper x)` needs evidence for the helper as
well as the constructors. With only a stripped worker interface, GHC
reports GHC-70566 at the call on a rigid spine. Its current suggestion
to use a rigid variable does not identify the missing equation. M151 adds helper evidence and refines the observer diagnostic.
[Ordinary worker](../examples/open-evaluation/SpineWorker.hs)
and [compiler upgrade](../examples/open-evaluation/check-case-upgrade.sh).

## M151. Checked helper calls inside constructor results

A branch can now build `Just (helper x)` when GHC has a checked
equation for the exact saturated helper call at the required kind.
For a local polymorphic identity, GHC checks that its solved Core
returns its own argument. A case equation can also reference an
already certified imported helper. The case branch retains the
helper's family application beneath the constructor.

The existing family dependency check follows those applications.
It checks helper rules, preserves the defining Id in the interface,
and retains the dependency when a separate module exports an alias.
A private helper remains absent from source exports. Changing its
identity body to force the argument, or adding a rewrite rule that
replaces it, removes the equation in this subset. Potentially
eligible producers recompile once to add this support.

This check follows the demand in GHC's desugared case tree. Agda
likewise uses the selected path in a compiled case tree to determine
which clauses give definitional reductions. The helper here needs
separate nominal evidence. [Agda case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees)
and [stripped-interface helper test](../../../testsuite/tests/dependent/should_run/NativeOpenConstructorHelpers.sh).

The [ordinary worker](../examples/open-evaluation/SpineWorker.hs) also
defines `firstViaHelper` for the Showcase. In the separate-module
test, changing an imported helper to select the other field invalidates
the unchanged client's proof through both interfaces. M154 supplies
the equation that a case needs to export `Just (wrap x)` for a local
`wrap x = Just x`.

## M154. A helper can construct its result

The private helper in `firstViaWrap (More x _) = Just (wrap x)` can
itself build a result. The checked body of `wrap` constructs `Just x`.
The same check accepts a public user-defined constructor in
`firstViaMark`. An ordinary client proves these results through a
separate alias and stripped producer interface with both head and
tail rigid.

The helper uses the same typed constructor check as a case branch.
Its signature must have a lifted type parameter and ordinary term
arguments. Its solved Core can return an input variable or saturate public lazy
constructor workers. It cannot scrutinize a variable, use a binding
outside its inputs, or call another helper. This keeps the helper
proof finite and excludes cycles. Existing rules and interface
dependencies still apply to the helper's exact Id.
[Producer](../examples/open-evaluation/SpineWorker.hs)
and [source gate](../../../testsuite/tests/dependent/should_run/NativeOpenConstructorHelpers.sh).

## M155. Reachable helper chains

A checked case can now use a helper that calls another helper. The
ordinary `firstViaMark` retains the three private equations for
`mark`, `markResult` and `markField`. A separate alias and ordinary
consumer check the result from stripped interfaces. A changed leaf
result invalidates the unchanged proof. A rule on a distant helper
or a cyclic call blocks publication.

GHC starts from the actual free local Ids in the desugared case.
It checks the reachable helpers in dependency order and rejects cyclic
groups. The existing result checker still requires saturated calls,
public lazy constructors and scoped arguments. Only the dependencies
of emitted equations enter the interface. Agda also checks its
reduction dependencies, with a separate termination check that can
prove certain recursive cycles. This subset requires an acyclic graph.
[Agda termination checking](https://agda.readthedocs.io/en/v2.8.0/language/termination-checking.html)
and [stripped-interface test](../../../testsuite/tests/dependent/should_run/NativeOpenConstructorHelpers.sh).

The next case is a private helper with clauses `choose No = Nothing`
and `choose (Some x) = Just x`. An exported `first (Box v) = Just (choose v)`
needs its case equations. On the M155 bootstrap compiler, an alias for
`first b` still reports GHC-68947 at the call when the worker has only
a stripped interface.

## M158. A private helper can match a constructor

`secondOrFirst` returns `Nothing` for an empty spine. For a nonempty
spine, it constructs `Just (headOr xs x)`. The private helper inspects
`xs` to choose the second element or the fallback `x`. The public
equation keeps this helper call inside the lazy `Just` field. A client
can choose either helper branch using only the stripped interfaces.
With a rigid `xs`, GHC cannot conclude that the result contains the
fallback.

GHC checks private case helpers in dependency order with the same
Core case checker as public workers. The helper needs complete
alternatives and a checked lazy representation. An extra demand on
a field, a missing alternative or a rewrite rule on the helper
prevents its caller from publishing checked equations. The helper's
equation survives in the interface without a source export.

When upgrading from M155, GHC rechecks an older home producer with a
local exported datatype eligible for case equations. Plain and
abstract modules can reuse their interfaces, and a warm build reuses
the updated producer.
[Producer](../examples/open-evaluation/SpineWorker.hs),
[stripped-interface tests](../../../testsuite/tests/dependent/should_run/NativeOpenConstructorHelpers.sh),
and [interface upgrade](../examples/open-evaluation/check-case-upgrade.sh).

## M159. Structural recursion under a result constructor

The ordinary `erase :: Spine a -> Spine Bool` replaces every element
with `True`. A checked equation for `More x xs` returns
`More True (erase xs)`. An ordinary interface-only consumer can prove
that equation with a rigid tail. The recursive family application
stays in the result. It needs the tail constructor before choosing a
branch.

The checker recognizes the exact unary Core self-call at its original
type arguments. The operand must be a proper bound field of the
matched first argument with the original input type. The self-call
must occur under a checked lazy constructor. A rebuilt argument
cannot establish the decrease, and an incomplete case or a case that
forces the recursive field cannot publish the equation. The existing
rule check still guards the recursive function.

An eligible M158 producer recompiles once on upgrade. Plain and
abstract modules can reuse their old interfaces, and a warm build
reuses the updated producer.
[Producer](../examples/open-evaluation/SpineWorker.hs),
[stripped-interface tests](../../../testsuite/tests/dependent/should_run/NativeOpenConstructorHelpers.sh),
and [interface upgrade](../examples/open-evaluation/check-case-upgrade.sh).


## M162. Structural recursion with an unchanged argument

The ordinary `fill :: Spine a -> b -> Spine b` replaces each element
with a supplied value. Its checked equation for `More x xs` and `tag`
returns `More tag (fill xs tag)`. The interface-only proof leaves both
the tail and the tag rigid.

The recursive call must use the original type arguments and pass the
same second Core binder. The first operand must still be a proper field
of the matched input. A replacement tag, a rebuilt first operand, or a
test that forces the tag cannot supply this equation.

The compiler rechecks v7 home interfaces with an eligible exported
datatype. Unrelated modules and subsequent warm builds can reuse their
interfaces.
[Producer](../examples/open-evaluation/SpineWorker.hs),
[stripped-interface tests](../../../testsuite/tests/dependent/should_run/NativeOpenConstructorHelpers.sh),
and [interface upgrade](../examples/open-evaluation/check-case-upgrade.sh).

### Passing a function as the carried operand

`Prelude.map Prelude.not` needs the function value of `not` before it
has a Boolean argument. There are two source tests. For a closed list,
the ordinary evaluator can in principle pass the function to `map`
inside one Core expression and read the resulting list. For an open
list, an exported `NegateAll xs` must keep the function application in
the interface until a client supplies a constructor. The second test
also needs the rules for unapplied functions and their equality.
The R23 card owns the distinction between matchable and unmatchable
arrows. [R23 implementation and compatibility](R23-unsaturated-type-families.md)
and [the function promotion roadmap](https://ghc.serokell.io/dh#function-promotion).

`map` also scrutinizes its second argument. Its first argument is the
function carried into the recursive call. The M162 check scrutinizes the
first argument and carries the second. A separately defined worker can
exercise the general argument order without Prelude's fusion rules.
The Prelude case must also account for the rules attached to the
installed definition.

M163 reports GHC-37479 at `P.not` for both the closed list and the
open alias. A rule-free ordinary worker with either of two different
callbacks reports the same error at the callback. An explicit
unmatchable arrow also leaves the separate family-saturation test
blocked: GHC-27346 requires the Boolean family `Flip` to receive its
argument. The companion negative test correctly rejects deriving
`a :~: b` from `f a :~: f b` for an unmatchable `f`.

M165 checks the closed list. The open alias now resolves the callback
and stops at the unknown list. Two checks remain. First, certify the
ordinary rule-free worker. Its cons equation applies the original
callback to the head and recurses on the tail. Second, account for the
rewrites available to the installed `Prelude.map`.

The `map` fusion rule introduces `build`, `foldr`, and `mapFB`.
In the native test, plain and optimized calls both return `False` at
the head of `map not (True : error "tail")`. A client adds a rule for
`build` at `Bool`. Both the fusion rule and the client rule fire.
The same expression returns `True`. Even an undefined input spine now
produces a cons. A proof about this call must account for the visible
rules on the functions introduced by the first rewrite.
[Reproduce the three builds](../examples/open-evaluation/check-map-fusion.sh)
and [installed rule definitions](../../../libraries/ghc-internal/src/GHC/Internal/Base.hs).

The next proof experiment compares the ordinary definition with the
fusion rule's right-hand side. Two expressions can both wait for
`xs` and then return different heads. Check both continuations on
`[]` and on the same cons cell with a rigid head and tail. For the
cons branch, compare field demand and the application of the callback.
A recursive proof step would require both sides to produce the same
lazy constructor and recurse on the corresponding input tail. Record
any helper used to unfold the rule's right-hand side. Its visible
rules can change either branch. This is a proposed proof check.
[HERMIT's proof of map composition and its scope](../papers/README.md#checking-ghc-rewrite-equations-with-hermit-haskell-2015).

### Checking an ordinary mapper across interfaces

M168 follows the ordinary worker's case on its second argument.
For the empty branch, the result is the empty list. For the cons
branch, the output constructor contains the original callback applied
to the input head. The recursive call carries the same callback into
the input tail. The compiler follows a local variable binding that
names the callback before the case. It rejects a worker that forces the
callback, replaces it, or recurses on the original list.
[Worker](../examples/open-evaluation/OpenMapWorker.hs)
and [ordinary client's two general proofs](../examples/open-evaluation/OpenMapProof.hs).

M169 lets a named function with checked equations serve as the
callback. One example changes the list's element type. For any tail
`xs` of traffic lights, the client proves
`Signals ('Red ': xs) :~: ('Stop ': Signals xs)`. The ordinary mapper,
callback and alias interfaces suffice. A rule for the callback can
invalidate the cons proof. The empty-list proof still checks because
that branch never applies the callback. Changing a callback clause
requires rechecking an unchanged client even when its alias interface
is unchanged. [Source aliases](../examples/open-evaluation/OpenMapSource.hs)
and [client and recompilation checks](../../../testsuite/tests/dependent/should_run/NativeOpenMap.sh).

Both the mapper and the callback can come from ordinary modules.
`Heads rows` maps the polymorphic `firstMaybe` over a list of rows.
Knowing the first element of the first row gives a `Just` result
without supplying its tail or any other row. The ordinary client
compiles from interfaces at `-O0`, `-O1`, and `-O2`.
[Row-head proof](../examples/open-evaluation/OpenMapProof.hs).

### M170. Use the installed Boolean callback

`InstalledNotSource` defines `NegateAll xs = W.each P.not xs`. The
mapper and the callback come from ordinary Haskell modules. An ordinary
client proves `NegateAll ('True ': xs) :~: ('False ': NegateAll xs)`
from interfaces alone. The defining module of `P.not` must have been
compiled with the new checker. An older installed package reports
GHC-64152 at the written callback. [Alias](../examples/open-evaluation/InstalledNotSource.hs)
and [ordinary client](../examples/open-evaluation/InstalledNotProof.hs).

An orphan rule can change `not True` to `True`. The regression test
imports that rule through a re-export and checks that the ordinary
optimizer fires it. The empty-list proof still compiles. The proof
that uses the head fails with GHC-83605. A separate test replaces
the object for a closed call while keeping its interface fixed.
[Installed callback and rule](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledNot.sh)
and [closed call and interface invalidation](../../../testsuite/tests/dependent/should_run/NativeOpenMap.sh).

The producer checks the argument and result constructors of ordinary
enumerations. The same code handles `Bool -> Bool` and `Lamp -> Signal`.
Recompilation also finds functions on imported wired-in enumerations,
including `Ordering`. [Upgrade check](../examples/open-evaluation/check-enum-upgrade.sh)
and [Agda comparison](../REFERENCES.md#an-ordinary-enumeration-function-as-a-callback).

An ordinary worker with two unrestricted, unmatchable arguments can
publish a certificate for public enumerations. It checks the first
argument exhaustively. Each branch returns an enumeration constructor
or the second argument. A branch that returns a constructor can leave
the second variable unused or write it as a wildcard. The checker
rejects a case that forces this argument, including demands from a
bang pattern or `Strict`. A third argument or a rule on the worker in
its defining module also makes the worker ineligible.

A consumer can supply the first argument and pass the remaining
function to a higher-order type family. The consuming proof checks any
rules visible at the point where it uses the equation. A closed call
with object code records the object dependency.
[Binary enumeration regression](../../../testsuite/tests/dependent/should_run/NativeBinaryEnum.sh).

The ordinary function `mayProceed Red _ = False` skips its second
argument. The other branch returns it. A client can save
`mayProceed 'Green` as a partial type-level function and prove that it
returns an as-yet unknown permission. The runtime example calls the red
branch with an undefined permission and gets `False`.
[Ordinary producer](../examples/open-evaluation/OpenMapWorker.hs),
[partial function](../examples/open-evaluation/OpenMapSource.hs)
and [client proof](../examples/open-evaluation/OpenMapProof.hs).

The remaining part of the roadmap example is the mapper. The installed
`Prelude.map` has fusion rules. Its interface can omit the recursive
unfolding. Evidence for the mapper must survive compilation of its
defining module and account for rules visible where a proof uses it.

The ordinary optimizer supplies a concrete limit on such evidence.
For ``g c n = c `seq` n``, evaluating `foldr k 'z' (build g)` with an
undefined `k` returns `'z'` at `-O0`. At `-O1`, the stock `fold/build`
rule fires and the same observation raises an exception. A conversion
rule for the mapper must justify each use of fusion at the producer
it reaches. The type of `g` alone cannot establish that condition.
[Reproduce both observations](../examples/open-evaluation/check-map-fusion.sh)
and [research on polymorphism with seq](../papers/README.md#free-theorems-and-seq-popl-2004).

A private instrumented build compared the original `GHC.Internal.Base.map`
with its own final Core at both `-O1` and `-O2`. Both inspect the input
spine, apply the same callback to the cons head and recurse on its
original tail. The checker also rejected changes to the callback, the
demand or the tail. This gives the producer a place to check the
ordinary mapper before its unfolding can disappear from the interface.
### M173. Check the installed Prelude mapper before publishing its equations

`PreludeMapSource` exports the alias
`PreludeNegateAll xs = Prelude.map Prelude.not xs`. Its ordinary client
proves both the empty-list equation and the result for a `True` head
with an unknown tail. The compiler checks the ordinary mapper's shape
after desugaring and again after optimization. A change to the optimized
body withdraws the equations. The ordinary module still builds. A
fresh no-code build also omits this evidence.
[Source alias](../examples/open-evaluation/PreludeMapSource.hs),
[ordinary proof](../examples/open-evaluation/PreludeMapProof.hs),
and [withdrawal and no-code checks](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledMap.sh).

On upgrade from M169 or M172, GHC recompiles a ruled mapper once to
check its optimized body. The new interface records the certificate.
The next build reuses it. The older reader diagnoses the new format.
[Two-compiler upgrade check](../examples/open-evaluation/check-map-upgrade.sh).

An optimized module can export the alias. The module that uses the
mapper's equation at `Refl` must disable rewrite rules, for example
with `-O1 -fno-enable-rewrite-rules`. With rules enabled, GHC reports
GHC-83605 at the proof and explains the required flag. A closed
application can still take the executable route.
An optimized client can also consume an explicit witness from another
module. A caller's rule on `build` can change the runtime result in
that same client. [Compiler Note](../../../compiler/GHC/Tc/Gen/SourceTable.hs)
and [witness and rewrite control](../examples/boundary-evidence/check-map-witness.sh).

The consumer can infer the public list index in `checkedFirst checked`
from the producer's interface. The same inference works inside `Maybe`
and a list. A fresh proof at the concrete result still requires the
mapper's equation. A check with rewrite rules enabled rejects it at the
source `Refl`. [Optimized consumer](../examples/boundary-evidence/UseMapWitness.hs).

The GHC Core replay for the concrete map builder checks both endpoints
of `foldr/build` with an unknown combiner and seed. It rejects a changed
combiner, a changed seed, and a new demand on the combiner. Other rules
can still apply to a producer supplied by a caller. In particular, a
non-parametric builder can use `seq` to distinguish a constructor from
a partial combiner. Rule-enabled conversion remains open.
[Fusion and definedness](../papers/README.md#short-cut-fusion-and-definedness).

### Checking an identity traversal with rules enabled

An ordinary `-O1` client can use a checked equation from an ordinary
list mapper. The interface must expose its active rules. The maintained
test uses a producer whose only active rule replaces
`mapper (\x -> x) xs` with `xs`. On an empty spine each side
returns the empty list. On a cons each side returns the original head
and leaves the tail lazy. This check depends on the compiled rule's
binders, arguments and result. Its name has no bearing on the proof.

The same client failed when the rule returned the input list for Boolean
negation, or evaluated the head before returning a cons. An imported
orphan rule and a rule declared after a splice also blocked the proof.
Changing the producer's rule invalidated the cached ordinary client.
Adding an orphan rule to an already imported module also recompiles the
client. [Maintained mapper test](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledMap.sh).
The check covers the identity traversal. The fusion rules on the
installed `Prelude.map` introduce other functions and need separate checks.

### Using the mapper's cons equation as a rewrite rule

An ordinary mapper can export a rule for its cons branch.

```haskell
{-# RULES "mapper/cons" [~1] forall f x xs.
    mapper f (x : xs) = f x : mapper f xs #-}
```

The defining module already certifies the lazy list equations against
its final Core. The reader checks the rule against the cons equation.
It checks both list constructors, the callback application, and the
recursive call on the input tail. The callback and fields can diverge.
The rule supplies the input constructor and leaves both output fields
lazy. The recursive call uses the original input tail.

An ordinary client proves the result for a `True` head and an abstract
tail with rewrite rules enabled and only the producer and alias
interfaces. The native test checks the optimizer fires the rule even
with a divergent callback and input head, then observes two cons cells.
Replacing the rule with one that forces the head or drops the tail
invalidates the unchanged client's proof. A second rule that changes
the result also blocks the proof, even when it activates in a later
phase. [Mapper regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledMap.sh).

Agda can register a proved equality as a rewrite rule. GHC's user rules
only require well-typed endpoints, so the mapper check also verifies
that the two endpoints have the same demand. This check reads the
compiled rule and uses no property of a particular callback. The rules
for the installed `Prelude.map` still need a proof for the fusion
helpers they introduce. [Agda rewriting](https://agda.readthedocs.io/en/latest/language/rewriting.html)
and [GHC rewrite rule semantics](../../users_guide/exts/rewrite_rules.rst).

### Composing two traversals over an unknown list

An ordinary mapper can replace two traversals with one.

```haskell
{-# RULES "mapper/compose" [~1] forall f g xs.
    mapper f (mapper g xs) = mapper (\x -> f (g x)) xs #-}
```

The checker requires the exact mapper in all three calls, with the
original input list and callbacks. Both sides inspect that list and
return an empty list for an empty input. On a cons they both produce
the lazy head `f (g x)`. The same argument applies to the tail, including
when only part of the input list is defined. A divergent `g` is permitted
when `f` ignores its argument. [HERMIT's map fusion proof](../papers/README.md#checking-ghc-rewrite-equations-with-hermit-haskell-2015).

An ordinary client imports an alias for two applications of the mapper
with `Prelude.not`. It proves that a `True` head remains `True` for any
tail with rules enabled. This uses the mapper's two existing equations.
An equality between arbitrary applications on a fully unknown spine
still needs separate evidence. A replacement rule that forces `g x`
changes the native result and blocks the unchanged proof.
[Mapper regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledMap.sh).

### Keep a checked mapper equation consistent with specialization

An orphan rule can replace `mapper not xs` with `xs`. The input
`[True]` distinguishes the results. At `ac2753c565`, an ordinary
client compiled at `-O1 -fno-enable-rewrite-rules` could construct
`Refl` for a `False` head and return `True` from the same runtime call.
The specializer applied the user rule while the simplifier had rules
disabled.

The specializer now checks the rewrite flag before applying a user
rule. Built-in rules can still select dictionary fields. With rewriting
disabled, the bootstrap compiler accepts the proof and the program
returns `False`. With rewriting enabled, the type checker rejects
the fresh proof. An ordinary call in that configuration still returns
`True` when the orphan rule fires.
[Provider, proof and runtime check](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledMap.sh).

### Prove that map preserves length

An ordinary client proves that the list produced by `Prelude.map`
has the original length for an arbitrary callback. Its `Spine`
witness carries the constructors of a finite list and omits its
elements. The empty case uses the installed map equation. The cons
case uses that equation once, so it adds a successor on each side.
Matching the recursive `Refl` supplies the equality for the tails.
Neither step needs an equation for the callback.

The same induction checks a direct retained list at `-O0`. A retained
`Bool -> Bool` callback works when the proof defines one equation
and matches the list in its body. A value of the generic retained
function domain `a -> b` still requires separate support. The
ordinary client can quantify over that arrow in a type or kind.

The ordinary regression imports only the alias interface. It rejects
an added successor at the empty clause and counts two mapped elements
with a bottom callback. [Alias](../examples/open-evaluation/PreludeMapLengthSource.hs),
[proof](../examples/open-evaluation/PreludeMapLengthProof.hs), and
[prior art](../REFERENCES.md#induction-on-the-spine-of-a-mapped-list).

### Filter one known head and keep the recursive tail

`Prelude.filter Prelude.not` keeps a `False` head and skips a `True`
head. An ordinary client proves both equations with `Refl` and an
unknown tail. The true predicate branch returns a cons without
evaluating the tail. The false branch leaves the recursive call on the
tail. An unknown predicate leaves the branch unresolved.
[Aliases](../examples/open-evaluation/PreludeFilterSource.hs) and
[proofs](../examples/open-evaluation/PreludeFilterProof.hs).

GHC checks the guarded filter before and after optimizing the defining
module. The guard tests the original predicate on the original head.
Both branches recurse with the same predicate and tail. The compiled
interface carries the checked equations even when it omits the term
unfolding. A normal build omits both equations if a producer rule changes
the checked body.

The proof module disables rewrite rules locally. With rules enabled,
GHC reports GHC-83605 at `Refl` because the installed fusion rules still
need a check. The regression also verifies that a kept runtime head can
be observed with a bottom tail.
[Interface and runtime regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledMap.sh)
and [prior art](../REFERENCES.md#list-filtering-with-a-known-predicate-result).

### Use an ordinary identity as an open callback

`Prelude.filter Prelude.id` keeps a `True` head and skips a `False`
head without knowing the tail. `Prelude.map Prelude.id` preserves a
rigid head at any lifted element type. The ordinary proof modules
need only the [filter alias interface](../examples/open-evaluation/PreludeFilterSource.hs)
or the [map alias interface](../examples/open-evaluation/PreludeMapSource.hs).

GHC checks the ordinary definition before and after optimization. It
requires an unrestricted function with one lifted type parameter and
a body that returns its bound argument. When the optimizer shares the
body, GHC follows aliases to a checked projection in the same module.
This is how the installed `id` retains its equation after GHC shares
its implementation with `breakpoint`. The interface can omit the
term unfolding. A no-code producer cannot publish this equation.

The proof site disables rewrite rules. An old source provider is
rechecked if an exported signature can be the polymorphic identity and
its interface has no checked equation. Clients with only an old
interface need the producer to publish the equation before they can
prove it. See the [installed list regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledMap.sh)
and [prior art](../REFERENCES.md#exporting-an-ordinary-identity-equation).
