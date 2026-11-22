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
and [witness and rewrite control](../examples/open-evaluation/check-map-witness.sh).

The consumer can infer the public list index in `checkedFirst checked`
from the producer's interface. The same inference works inside `Maybe`
and a list. A fresh proof at the concrete result still requires the
mapper's equation. A check with rewrite rules enabled rejects it at the
source `Refl`. [Optimized consumer](../examples/open-evaluation/UseMapWitness.hs).

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
has the original length for an arbitrary callback. `Count` refers to
the checked equations of an ordinary `size :: [a] -> Size`. The
provider uses `-fwrite-source-equations` to export those equations
with `ExperimentalDependentTypes` disabled.
The proof uses the equations through `Count`, and the runtime example
calls the same `size`. Its Peano result uses `Zero` and `Succ`.

The `Spine` witness carries the constructors of a finite list and omits
its elements. The empty case uses the installed map equation and
returns `Refl`. The cons case adds a successor on each side. Matching
the recursive `Refl` supplies the equality for the tails. Neither step
needs an equation for the callback.

An earlier check used the same induction with a direct retained list
at `-O0`. A retained `Bool -> Bool` callback works when the proof defines
one equation and matches the list in its body. A value of the generic
retained function domain `a -> b` still requires separate support. The
ordinary client can quantify over that arrow in a type or kind.

The public 308-line compiler at `68357dc283` checks the ordinary client
using just the provider and alias interfaces, built with `-O1` and
`-fomit-interface-pragmas`. That compiler used extension-enabled source
to build the provider. The producer option runs the same checker and
uses the same interface format. A recursive unfolding alone does not
supply the generic equation for `size`. Adding a successor to the law
gives GHC-83865 at the empty clause, where `Zero` and `Succ Zero` differ.
Removing the induction hypothesis gives GHC-05617 at the cons clause, with
`size (map f xs1) ~ size xs1` still to prove. At runtime the regression
prints `(Refl,Succ (Succ Zero))` with a bottom callback.
[Size definition](../examples/open-evaluation/PreludeMapLengthSize.hs),
[aliases](../examples/open-evaluation/PreludeMapLengthSource.hs),
[proof](../examples/open-evaluation/PreludeMapLengthProof.hs),
[regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledMap.sh),
and [prior art](../REFERENCES.md#induction-on-the-spine-of-a-mapped-list).

The standalone compiler reproduced all 312 Showcase lines after
bootstrapping and after building itself. The self-hosted runs covered
GHCi and native `-O0`, `-O1`, and `-O2`. All 68 selected normal-way tests
passed. The dependent directory had 295 passes and the same 20 failures
as the public scoped-signature compiler. The polykinds directory had
224 passes and the same 18 failures as the preceding measured compiler.
Changing the list step to count each element twice invalidated the
unchanged singleton proof through an intermediate alias. Withdrawing
the equation invalidated the alias. Both native and no-code builds
accepted the restored definition.

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

### Stop a list prefix at the first rejected head

The checked equations for `Prelude.takeWhile` return `[]` when the
predicate rejects the head. When it accepts, the equations return the
original head and leave the recursive call on the original tail. GHC
checks the branch bodies before and after optimization.

`observePrefix` checks the installed `(&&)` with a runtime gate and the
current head. The gate remains in the predicate's type. An ordinary
consumer opens either equation through a stripped interface with rewrite
rules disabled. Both branches leave a bottom tail lazy. When the gate
is false, the predicate also leaves a bottom head lazy. A decision about
a different input gives no equation for the current head. A producer
that forces the tail before returning `[]` publishes neither prefix
equation. The installed module rebuilds once to publish the new equations.
[Example](../examples/open-evaluation/PreludeTakeWhile.hs),
[regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledTakeWhile.sh),
and [prior art](../REFERENCES.md#ending-a-list-prefix-at-a-decision).

### Recover the input from its prefix and suffix

The same guarded-list checker certifies `Prelude.dropWhile`. On a cons,
it returns the original list when the predicate rejects the head. When
the predicate accepts, it recurses on the original tail. The checker
validates each result before and after optimization. It accepts the
empty list, the original cons, a recursive call on the original tail,
or the original head prepended to that call. A rule can change the
result to another of these shapes. The compiler then drops both equations
because the final result differs from the initial equation.

An ordinary client proves `takeWhile p xs ++ dropWhile p xs = xs`
with a decision for each inspected head. The proof recurses while the
predicate accepts. The first rejection supplies both equations and the
proof leaves the remaining decisions lazy. The runtime suffix also
leaves both fields of the rejected cons lazy. An accepted head requires
the next spine cell. The regression rejects a different predicate input,
changed recursion arguments, and a forced tail in the rejecting branch.
The Showcase computes decisions with `Prelude.not` from the same retained
runtime list as the proof. It observes `Refl` and the first two elements
of the original and reconstructed lists. The tail that follows the
rejected head stays undefined.
[Source alias](../examples/open-evaluation/PreludeDropWhileSource.hs),
[proof](../examples/open-evaluation/PreludeDropWhileProof.hs),
[runtime observation](../examples/open-evaluation/PreludeDropWhileRuntime.hs),
[regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledDropWhile.sh),
and [Agda's prefix and suffix law](../REFERENCES.md#ending-a-list-prefix-at-a-decision).

### A runtime list at its exact contents

`negatedCons` prepends a Boolean and preserves the caller's tail in the index.

```haskell
negatedCons :: foreach (b :: Bool) -> foreach (bs :: [Bool]) -> CheckedList (not b ': bs)
negatedCons b bs = CheckedList (not b : bs) Refl
```

An ordinary caller can read the first cell without evaluating the tail.
Testing for an empty list also leaves the head unevaluated. Changing the
result index to `b ': bs` gives `GHC-83865` at the call to `negatedCons`
because `not b` cannot match an unknown `b`.
[Source](../examples/open-evaluation/IndexedPrefix.hs)
and [equality witness](../REFERENCES.md#a-list-indexed-by-its-contents).

### Project and rejoin a guarded split

The ordinary proof can put the checked prefix and suffix in a promoted
pair. The installed `fst`, `snd`, and `(++)` equations project and
rejoin it. The same prefix induction proves that the result is the
original list. The proof can use the provider's stripped interface.
The closed evaluator also checks `Prelude.span Prelude.not` on the
literal list `[False, True, False]`. The same projection and append
equations check that its two lists reconstruct the literal input.

The checked equations for installed `Prelude.span` use the original
predicate and matched tail. The proof of the accepted prefix exposes the
original head with an unknown tail. The accepted suffix equals the second
projection of the recursive split. The rejected suffix is the original
cons. An ordinary client proves that appending the two projections
reconstructs the input. The proof inspects each decision up to the first
rejection and leaves the remaining decisions lazy.
The Showcase prints both accepted projection equalities and the
reassembly proof. It observes the first element of each result and the
first two elements after append, with an undefined tail after the rejected
head.

The accepting branch returns the pair and the first prefix cons before
matching the recursive pair. The selectors for the recursive fields remain
inside the result fields. A strict recursive case forces the undefined
suffix before it returns the first cons. The initial list case and
predicate must finish before `span` returns the pair.
For example, matching the result of `span p undefined` forces the list.
A pair built directly from the two list calls exposes its constructor
before either call runs.
The checker verifies the ordinary body and the optimized wrapper and
worker with their unboxed pair result. It uses the existing checked
`fst` and `snd` tables in the accepted branch. A strict recursive
body publishes no equation. A proof with an unknown Boolean decision
cannot select the branch.
[Proof](../examples/open-evaluation/PreludeSpanProof.hs),
[regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledDropWhile.sh),
and [Agda's `span-defn`](../REFERENCES.md#ending-a-list-prefix-at-a-decision).

### Return the matched tail from an ordinary producer

An ordinary module can opt into source equations with
`-fwrite-source-equations`. This example drops at most one head.

```haskell
dropOneIf :: (a -> Bool) -> [a] -> [a]
dropOneIf _ [] = []
dropOneIf p input@(x : xs)
    | p x = xs
    | otherwise = input
```

The accepting equation returns the original tail. The rejecting equation
keeps the original cons and leaves both fields lazy. The accepted output
can expose the next element without forcing the removed head when the
predicate ignores it. Returning `seq x xs` instead needs the head and
withdraws both equations. The guard still requires its exact term owner
when neither branch recurses. An ordinary `Refl` proof uses an alias and
the producer's stripped interface at `-O0` and `-O2`.
[Separate compilation regression](../../../testsuite/tests/dependent/should_run/NativeOpenUserGuardedTail.sh).

### Check guarded lists at concrete element types

The ordinary producer exports a function on `[Bool]` and one on a list
of its own `Signal` datatype. Each callback has the exact element type
as its domain. The two signatures use closed, lifted element types and
the same input and output list type. GHC checks the actual guard and
branch results in Core before it publishes nominal equations. The
private guard reads its value operands without a kind parameter.

An ordinary client proves removal of `False` and preservation of an
`Entry` with a rigid tail. The Showcase exposes one output element for
each function and leaves the later tail unforced. A callback on a
newtype of `Bool` fails the kind check. A Boolean branch that forces the
removed head loses both equations. A no-code build withholds the list
equations.
[Producer](../examples/open-evaluation/ConcreteGuardedList.hs),
[ordinary proofs](../examples/open-evaluation/ConcreteGuardedListProof.hs),
[native regression](../../../testsuite/tests/dependent/should_run/NativeOpenConcreteGuardedList.sh),
and [nominal roles](../REFERENCES.md#nominal-domains-for-concrete-list-callbacks).

GHCi loads the producer from source with the bytecode backend. Its final
Core keeps debugger breakpoints around the predicate call, constructor
and branch results. The checker reads through those ticks and compares
both branches with the original equations. The ticks stay in the
executable Core. The interpreted strict-head variant still fails at the
alias, and the lazy variant passes with no object file.
[GHCi regression](../../../testsuite/tests/dependent/should_run/NativeOpenConcreteGuardedListGhci.sh)
and [Core breakpoint definition](../../../compiler/GHC/Types/Tickish.hs).

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

A saturated symbolic call can still use an ordinary unfolding with
rewrite rules enabled. The open evaluator follows the definitions it
needs and checks the rules for each observed binding. A rule on a
visited alias can block the call. In an implicitly quantified signature,
an unqualified type variable keeps its earlier scope.
[Open source regression](../examples/open-evaluation/check-source.sh).

### Prove that filter never increases length

`filterLengthBound` bounds `Count (FilterWith p xs)` by `Count xs`
for an arbitrary element type and predicate. The `FilterSteps` witness
contains the finite spine and the decision for each head. At the empty
list, both sizes are zero. When the predicate keeps a head, the proof
adds a successor to both bounds. When it skips the head, the proof
adds a successor only to the original size.

The caller can supply the given result `p x ~ 'True` or
`'False ~ p x` at a rigid predicate and head. The solver uses that
nominal equality for the Boolean operand in the checked filter guard.
The ordinary family equation then selects the branch. The original
`Keep` and `Skip` constructors still accept an explicit equation for
the step. The 311 compiler used that form because its rewriter left
`p x ~ b` inert.

The ordinary regression checks the proof from stripped provider
interfaces. It rejects a guessed decision, evidence about another
predicate or head, and representational equality. It also checks that
the Given stays inside its branch and that `p x ~ p y` cannot identify
`x` with `y`. A reversed bound for the dropped singleton `'[ 'True]`
gives GHC-83865 at `BothSucc ZeroBound`. The runtime example uses the
same `size` to observe that filtering `[False, True]` by `not` leaves
one of the two elements.
[Proof](../examples/open-evaluation/PreludeFilterLengthProof.hs),
[regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledMap.sh),
and [prior art](../REFERENCES.md#list-filtering-with-a-known-predicate-result).

`runtimeFilterBound` uses the same proof on a list read from an `IORef`.
It matches the retained list. At a cons, it passes the actual call
`Prelude.not x` to `booleanResult :: foreach (b :: Bool) -> BooleanResult b`.
Matching the returned GADT gives the nominal result equality for
`keepIfTrue` or `skipIfFalse`. A direct `case Prelude.not x` does not
give those equalities and fails at both step constructors. The generic
`FilterSteps` API still supports other predicates.

An ordinary module can open the packaged proof using only its interface.
Forcing the package does not inspect the list. Forcing the first proof
constructor inspects the first head and leaves the tail lazy for both
decisions. A complete proof requires a finite spine and defined Boolean
elements. The simplifier preserves the checked predicate call in the
retained operand. Its ordinary calls can still inline.
[Runtime example](../examples/open-evaluation/PreludeFilterRuntime.hs)
and [regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledMap.sh).

### Prove that filter distributes over append

`filterAppend` proves `filter p (xs ++ ys) = filter p xs ++ filter p ys`
using the installed functions. The ordinary proof takes one
`BooleanResult (p x)` for each head in the finite left spine. The
right list stays abstract and needs no decisions. In the empty case
both sides reduce to `filter p ys`. In the cons case a single decision
selects the same branch for the original tail and for that tail with
`ys` appended. The induction hypothesis equates the filtered tails.

The proof reads only stripped provider interfaces at `-O0` and `-O2`.
Ignoring the decision while keeping the induction hypothesis fails at
`Refl`. In the runtime check both computations skip a head and expose
the next cons before demanding the remaining left tail or right list.
[Proof](../examples/open-evaluation/PreludeFilterAppendProof.hs),
[regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledMap.sh),
and [prior art](../REFERENCES.md#distributing-filter-over-append).

`packFilterAppend` builds those decisions for `Prelude.not` from a
runtime list. `negationResult x` checks the installed predicate at the
current head `x`. The type checker rejects a decision about a different
head. An ordinary client matches the equality on lists read at runtime
using stripped producer interfaces at `-O0` and `-O2`. Forcing the proof
requires a finite left spine and defined left heads. It can leave the
right list undefined. The outer package can leave both inputs undefined.
[Runtime producer](../examples/open-evaluation/PreludeFilterAppendRuntime.hs)
and [regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledMap.sh).

### Supply a runtime filter decider

`runtimeFilterBoundWith` takes an erased required type argument `(type p)`
before the decider `foreach (x :: Bool) -> BooleanResult (p x)`. The caller
supplies `p` because GHC cannot infer an unmatchable predicate from `p x`.
Matching the result
supplies `p x ~ 'True` or `'False ~ p x` to the existing proof. An ordinary
client can choose an existentially packaged decider and open the bound
through interfaces. The example derives the runtime `Prelude.filter`
callback from that same decider. A decider indexed by negation fails to
check for the constant-true predicate. A bare `Bool -> Bool` also fails
because its result supplies no index.

The empty proof leaves the decider lazy. For a cons, the decider controls
demand for the head. Both first proof constructors leave the tail lazy.
The full proof requires a finite spine and a defined decision at every
head. The example's displayed bound and filtered size can evaluate the
pure decider separately. [Runtime decider](../examples/open-evaluation/PreludeFilterDecider.hs),
[regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledMap.sh),
and [prior art](../REFERENCES.md#checked-runtime-decisions-for-filter).

### Check a three-way runtime comparison

`decideOrdering` matches the actual result of `p x` and returns a witness
indexed by that same application. `compareFrom` captures a runtime pivot
in an ordinary `Prelude.compare` closure. An ordinary client can select
its own closed payload family from the witness. Its helper takes
`Witness o` and matches the three constructors. Each branch refines the
variable `o`, so GHC reduces `Select o` and checks the selected payload.
The other two payloads stay lazy.

A nominal Given `p x ~ 'LT` supplies `Refl :: p x :~: 'LT`. Bare `Refl`
for `Select (p x) :~: Int` can still get stuck when the application is
opaque. Pass the first proof to a congruence function over abstract
`Ordering` variables. Matching `Refl` there transports the equality through
the family. This is the same direction as Agda's explicit `rewrite` or
`cong`. An observation of `p y` supplies only the equality for `p y`.
[Provider](../examples/open-evaluation/RuntimeOrdering.hs),
[ordinary consumer](../examples/open-evaluation/OrderingPayload.hs), and
[Agda with-abstraction](https://agda.readthedocs.io/en/latest/language/with-abstraction.html#rewrite).

### Capture a runtime cutoff in the predicate

`packCutoff` closes over a Peano cutoff read from an `IORef`. It checks
each row with the ordinary `size` function. The same local function goes
in the proof index and in the installed `Prelude.filter` call. A strict
case on that function's result supplies the decision at each row. The
proof passes the element type and predicate explicitly. Omitting the
predicate would leave it ambiguous under the non-injective size equation.
A proof built with a different local closure fails with GHC-83865.

The Boolean control captures the argument of ordinary equality. Both
cutoffs run through the same function body. The direct type alias for the
unpromoted binary equality fails with open operands. Its partial
application fails because the closed evaluator requires both arguments.
The retained route uses the closure identity and the result observed at
each call. The proof accepts a decider for any element type. Constructing
a witness from an opaque function still requires a closed function domain
such as `[Bool] -> Bool`.

The Peano example keeps one, two, or three rows as the runtime cutoff
increases from zero to two. The first proof constructor leaves the outer
tail lazy. Zero rejects a nonempty row without inspecting its elements
or tail. A finite cutoff can reject an infinite row after a finite prefix.
The whole bound requires a finite outer spine and a defined decision for
each row. [Cutoff example](../examples/open-evaluation/PreludeFilterCutoff.hs),
[regression](../../../testsuite/tests/dependent/should_run/NativeOpenFilterCutoff.sh),
and [prior art](../REFERENCES.md#checked-runtime-decisions-for-filter).

### Prove that append adds the list lengths

`lengthAppend` proves `Count (Appended xs ys) :~: Plus (Count xs) (Count ys)`.
It matches a finite spine for `xs`. The right list stays abstract.
At the empty list, the append equation returns `ys` and addition by zero
on the left returns `Count ys`. At a cons, append preserves the head
and addition adds a successor. `Count` adds the same successor to the
left side. Matching the induction hypothesis proves the remaining
equality for the tail.

`Appended` refers to installed `Prelude.(++)`. GHC checks its two
clauses in both desugared and optimized Core. The cons clause must keep
the original head and call itself on the original tail and right operand.
It must produce a cons before inspecting either list further. The
producer's interface carries these checked equations. The proof disables
rewrite rules because the installed append fusion and literal rules still
require their own checks.

The ordinary proof compiles from stripped interfaces at `-O0` and
`-O2`. The regression rejects an extra successor, a cons clause without
its induction hypothesis, and the proof with rewrite rules enabled.
At runtime, counting the complete appended spine ignores bottom elements
in both lists. Counting only its first constructor also leaves the left
tail and the right list unevaluated.
A code build rechecks an old provider whose exported signature can
supply append equations. An old `GHC.Internal.Base` interface and object
can stay in place for this build. An unrelated module reuses its
interface. A subsequent build reuses the checked provider, including
one whose body could not supply equations. No-code builds neither
publish equations nor record the final Core check.
[Interface upgrade](../examples/open-evaluation/check-append-upgrade.sh).

[Aliases](../examples/open-evaluation/PreludeAppendLengthSource.hs),
[proof](../examples/open-evaluation/PreludeAppendLengthProof.hs),
[regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledAppend.sh),
and [prior art](../REFERENCES.md#induction-on-the-left-spine-of-append).

### Prove that empty vector append preserves its right operand

`appendEmpty` proves `P.appendVec 'P.VNil xs :~: xs` for a rigid
vector at any length. The producer checks the empty branch in
desugared and optimized Core. The published equation uses the
constructor's zero index and the nominal equality `Add 'Z n ~ n`.
A rigid left vector of length zero still leaves the equation stuck.
Closed calls with a written cons still use the closed evaluator.

`appendEmptyAt` binds the vector with `foreach`. An ordinary caller
can use the equality for the value it read at runtime. Evaluating the
proof at an undefined vector returns `Refl`. Appending with an
undefined left operand still forces that operand in the case expression.
GHC rejects a proof that identifies the result with a different vector
of the same length. [Runtime proof and optimization](../REFERENCES.md#empty-append-at-a-runtime-vector).

An ordinary client imports the proof and reads the head of an appended
vector with a suspended tail. The proof and its caller disable rewrite
rules locally.

`appendCons` exposes a second checked equation. It preserves the head
and suspends the recursive call on the exact left tail and right operand.
Its two `Proxy` parameters identify the independent input indices. The
result length does not determine them. Compiled Core must rebuild a
lazy constructor and make the same call before and after optimization.
The regression rejects an altered head or right operand and checks
that observing the result's outer constructor leaves the input head,
tail, and right operand suspended.

Agda uses the same cons equation for the recursive step in
`splitAt`'s reconstruction proof.
[Proof](../examples/packed-vector/OpenVectorAppend.hs),
[ordinary caller](../examples/packed-vector/OrdinaryOpenVectorAppend.hs),
[regression](../../../testsuite/tests/dependent/should_run/NativeOpenIndexedAppend.sh),
and [Agda vector append and split](https://agda.github.io/agda-stdlib/v2.3/Data.Vec.Base.html).

### Two append equalities at one runtime length

`appendEmptyAtBoth` matches one retained length and proves the empty
append equation for each of two vectors at that length. Each equality
names its own input vector. GHC rejects substituting the first vector
for the second in the proof.

The ordinary client imports both interfaces with the source files
absent. It checks the written cuts `Z`, `S Z`, and `S (S Z)`, then
reads zero and successor from an `IORef`. The proof forces the cut's
outer constructor and leaves both vectors and the successor's
predecessor unevaluated. The regression runs with Core and STG Lint
at `-O0`, `-O1`, and `-O2`.
[Prior art](../REFERENCES.md#two-append-equalities-at-one-runtime-length).

### Prove that map distributes over append

`mapAppend` proves `map f (xs ++ ys) = map f xs ++ map f ys`
by induction on the finite spine of `xs`. It uses both installed
functions through their aliases. The proof keeps `f` and `ys` abstract.
The empty case reduces both sides to `map f ys`. The cons case has
the same head `f x` on both sides. Matching the induction hypothesis
proves equality of the tails without evaluating `f x`.

The ordinary proof uses stripped provider interfaces and disables
rewrite rules. At runtime both list expressions return the same
Boolean values. Both expressions also give a cons with a bottom
callback, head, left tail, and right list.
[Proof](../examples/open-evaluation/PreludeMapAppendProof.hs),
[regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledAppend.sh),
and [prior art](../REFERENCES.md#distributing-map-over-append).

### Prove that map composes two traversals

`mapComposition` proves `map f (map g xs) = map (f . g) xs` using
the installed `Prelude.map` and `(.)`. A separately compiled ordinary module
matches a finite `Spine xs`. The empty case gives reflexivity. The
cons case has head `f (g x)` on both sides and uses the induction
hypothesis for the tail. Both callbacks remain abstract and can have
different input and output types.

GHC checks the ordinary body of `(.)` in desugared and optimized Core.
The published equation retains the argument order and the lazy call
to `g`. A partial application of the checked function can be passed to
the installed map. When a cons supplies the last argument, the solver
checks the rules for the exact operator in its defining interface.
A code build rechecks an older `GHC.Internal.Base` interface once to
publish this equation.

The regression checks stripped interfaces at `-O0` and `-O2` with
rewrite rules disabled in the proof module. Removing the induction
hypothesis fails at `Refl`. A composition function that forces the
result of `g` gets no equation. Both runtime traversals expose a cons
with bottom callbacks and fields. Both return the first mapped value
when `f` ignores a bottom result from `g`. HERMIT's Core proof also
covers a bottom input spine. The `Spine` proof here covers finite spines.
[Producer](../examples/open-evaluation/PreludeMapCompositionSource.hs),
[proof](../examples/open-evaluation/PreludeMapCompositionProof.hs),
[regression](../../../testsuite/tests/dependent/should_run/NativeOpenInstalledAppend.sh),
and [HERMIT's map fusion proof](../papers/README.md#checking-ghc-rewrite-equations-with-hermit-haskell-2015).

### Prove that reverse preserves list length

`lengthReverse` relates the result of installed `Prelude.reverse` to
the original list. The proof uses an accumulator invariant. The
worker's result has length `Plus (Count xs) (Count acc)`. The cons
clause moves the same head to the accumulator. The recursive step
uses right-successor transport, and the wrapper uses right-zero for
the empty accumulator.

The library names the existing tail-recursive worker `reverseOnto`
in `GHC.Internal.List`. The source aliases use its checked equations
and the checked equation for the public wrapper. The ordinary proof
exports a theorem about the result of the wrapper.

The compiler checks the exact worker, head, tail, and accumulator in
desugared and optimized Core. It checks that the wrapper calls that
worker with an empty accumulator. A finite spine exposes the reverse
result without evaluating its elements. A bottom tail prevents the
result from exposing a cons.
[Source aliases](../examples/open-evaluation/PreludeReverseLengthSource.hs)
and [ordinary proof](../examples/open-evaluation/PreludeReverseLengthProof.hs).

### Check the equations of a right list fold

The installed list `foldr` returns the seed at `[]`. At `x : xs` it
passes `x` and the fold of `xs` to the combiner. The tail, combiner,
and seed can stay unknown in the proof. The combiner has unmatchable
arrows. Its application gives no evidence that its arguments are equal.
The two checked equations also prove `foldr k z (xs ++ ys) =
foldr k (foldr k z ys) xs` by induction on the finite spine of `xs`.
[Agda's `foldr-++`](https://agda.github.io/agda-stdlib/v2.4/Data.List.Properties.html).

The checker accepts a direct recursive call with the same combiner and
seed, or a call to the local worker that captures both. It checks the
original head, the proper tail, and the absence of additional demand
before applying the combiner. The same check works for a separately
compiled ordinary fold. Changing a checked body during optimization
withdraws the equation. A fold that forces the step or seed before
matching the list fails at its open type alias. A no-code build withholds
both equations.

The source alias names `GHC.Internal.List.foldr`. Its defining term
is `GHC.Internal.Base.foldr`. The `Foldable []` dictionary selects that
term for the method re-exported by `Prelude`. The certificate belongs
to the list term. Using the class selector in a type requires separate
evidence for dictionary selection.

The proof disables rewrite rules because the installed fold has
fusion rules. A builder that uses `seq` can distinguish a partial
combiner from a constructor, so its polymorphic type cannot justify
`fold/build`. The native test checks that a non-strict combiner can
skip the element, seed, and tail. It also checks that the fold demands
the input spine and skips the combiner on an empty list.
[Source alias](../examples/open-evaluation/ListFoldrSource.hs),
[ordinary proof](../examples/open-evaluation/ListFoldrProof.hs), and
[regression](../../../testsuite/tests/dependent/should_run/NativeOpenListFoldr.sh).

GHCi loads direct and local-worker folds from source with the bytecode
backend. It checks both equations with the same Core matcher. The
matcher reads through breakpoints on the seed result, callback
application, and recursive call. The interpreter retains the
breakpoints. The regression proves the source equations, checks that
the strict step and seed variants still fail, and emits no object file.
[GHCi regression](../../../testsuite/tests/dependent/should_run/NativeOpenListFoldrGhci.sh).

### Fold with fixed element and seed types

An ordinary `foldBits` takes a Boolean step and seed over `[Bool]`.
The same equations hold for a local worker that folds `[Char]` with a
`Maybe Bool` seed. The element and seed types are closed. The first
callback argument must match the list element nominally. The second
callback argument and both results must match the seed type nominally.
A callback on a newtype of `Bool` fails at its use. The strict-step
and strict-seed variants still fail at their aliases. A compiler
upgrade rechecks an older interface with an eligible signature once.
[Concrete consumer](../../../testsuite/tests/dependent/should_run/NativeOpenListFoldrConcrete.hs)
and [native controls](../../../testsuite/tests/dependent/should_run/NativeOpenListFoldr.sh).

### Select the list instance of `Prelude.foldr`

The receiver kind identifies the list instance of the ordinary selector.
The compiler looks up its visible, context-free dictionary and checks the
method selected by the dictionary constructor. The method must have the
checked list fold equations at the selector's specialized type. The
ordinary solver must select the same dictionary in the written and lexical
evidence scopes. A given constraint can therefore block this reduction.

A code-generating build checks the dictionary's final Core constructor.
If its field names the checked list fold, the compiler records the selector
and worker Names in the dictionary's mandatory interface details. The
proof works at `-O0` with interface pragmas ignored. An older interface
can also supply the field through its dictionary unfolding when interface
pragmas are available. The proof disables rewrite rules for the list worker.
[Proof using `Prelude.foldr`](../examples/open-evaluation/PreludeFoldrProof.hs),
[class promotion roadmap](https://ghc.serokell.io/dh#promote-classes), and
[GHC's lexical scoping principle](https://github.com/ghc-proposals/ghc-proposals/blob/master/principles.rst#221lexical-scoping-principle-lsp).

### Prove an equation for `Prelude.zipWith`

When both lists start with a cons, the result starts with the callback
applied to those heads. The callback and both tails can stay abstract.
The installed function checks the left list first. A left nil returns nil
immediately. A right nil returns nil after the left cons is known. An
abstract left spine and a right nil leave the reduction stuck.

The compiler checks these three paths before optimization and checks
the same equations against final Core. The ordinary client uses a
separately compiled alias and disables rules because the installed
function has a fusion rule. The Showcase applies the proof and reads
the first result with undefined tails. See the [alias](../examples/open-evaluation/PreludeZipWithSource.hs),
[ordinary proof](../examples/open-evaluation/PreludeZipWithProof.hs), and
[Agda list equations](https://agda.github.io/agda-stdlib/v2.3/Data.List.Base.html#zipWith).

### Fold a mapped list

Induction on a finite list spine proves that `foldr k z (map f xs)` equals
`foldr (k . f) z xs`. The step, mapper, and seed can stay abstract. The
proof uses the installed `foldr`, `map`, and `(.)` with rewrite rules
disabled in the provider. Its generic proof exports retain their rule
dependencies. The Showcase imports two `OPAQUE` observations whose written
results are `'True :~: 'True` and `String`. The consumer can keep rewrite
rules enabled. In the provider, both folds skip an undefined seed and tail.
A step that ignores its head also skips the mapper and input head.
[Source proof](../examples/open-evaluation/PreludeFoldrProof.hs) and
[Agda's list law](../REFERENCES.md#right-fold-after-map).


## Select a mapping worker from a ground dictionary

An ordinary producer can define `mapLike = mapMaybeValue` in a
`MapLike Maybe` instance. Its reader can prove both constructor
equations with `Refl`. In the `Just` case both the callback and the
payload are type variables. The runtime example returns `Just False`.
The `Nothing` case leaves the callback unevaluated.
[Producer](../examples/open-evaluation/MappingWorker.hs)
and [proofs](../examples/open-evaluation/MappingProof.hs).

The instance must select a named worker with checked equations.
A local `MapLike Maybe` constraint leaves the method choice open.
GHC reports GHC-83605 at the call.
[Agda Maybe](https://github.com/agda/agda-stdlib/blob/v2.4/src/Data/Maybe/Base.agda)
has the same two constructor equations.

A mapping call can also use an ordinary imported dictionary with no
instance context. The solver must select that dictionary with the
lexical and written Givens in scope. GHC reads the method field from
its full interface. If the field names a worker of the specialized
method type, the Core observer can follow the worker's unfolding.
An applicable rule on the selector, dictionary, or worker blocks the
proof. The reader also detects a missing unfolding.

For `Prelude.fmap` with the installed `Functor Maybe` instance, the
observer reads back `Nothing` without evaluating the callback. The
proof uses an abstract callback. The runtime example passes a divergent
callback. On `Just x`, readback produces `Just (f x)` with
`f :: a %U -> b` and `x :: a` still abstract. A second call can
consume this value. For example, GHC accepts this result with `Refl`.

```haskell
mapTwice :: forall a b c (f :: a %U -> b) (g :: b %U -> c) (x :: a).
            Proxy f -> Proxy g -> Proxy x
         -> Prelude.fmap g (Prelude.fmap f ('Just x)) :~: 'Just (g (f x))
```

The constructor fields are lazy at runtime. A different callback
or a different payload gives a type error. An equality between two
mapped results gives no equality between their input elements.
A method that forces the callback result with `seq` rejects this
proof at the source call.
[Source proof](../examples/open-evaluation/PreludeMappingProof.hs),
[source-hidden regression](../../../testsuite/tests/dependent/should_run/NativePromotedClassBoundary.sh),
[installed instance](../../../libraries/ghc-internal/src/GHC/Internal/Base.hs),
and [Eisenberg's review](https://github.com/ghc-proposals/ghc-proposals/issues/383#issuecomment-746676314).

On upgrading from a compiler that omits this evidence, a source build
checks the worker and its instance again. A warm build reuses both
interfaces. The upgrade check also applies to `-fno-code -fwrite-interface`.
A separate Core experiment observed both outer constructors from the
installed `Prelude.fmap`.
[Core observations and readback](R05-dependent-core.md#open-observation-of-the-installed-maybe-method).

[Source upgrade regression](../examples/open-evaluation/check-mapping-upgrade.sh).

### A rule in the module importing a mapping proof

At compiler revision `bffa69e282`, `mapJust` proves
`Prelude.fmap f ('Just x) :~: 'Just (f x)`. GHC reports GHC-83605
if the proof module declares a competing rule for the same call.
An importing `Main` can still declare this rule and compile at `-O2` with the following rule.

```haskell
{-# RULES "transport/fmap-maybe" [~1] forall f (x :: Bool).
    Prelude.fmap f (Just x) = Nothing #-}
```

In one executable, the imported proof returns `Refl` and the client's
`Prelude.fmap not (Just True)` returns `Nothing`. R16 still needs to
track the selected method and instance when a client imports the proof
or changes its rules.
[Provider](../examples/open-evaluation/PreludeMappingProof.hs) and
[rewrite-rule semantics](../REFERENCES.md#rewrite-rules-and-imported-method-proofs).

### Replay rule dependencies for a closed method

The family-axiom prototype at `68c252d93d` records dependencies for a
single nullary equation. Its optimized Showcase failed in the specializer
on the added `IdDetails` case. The `376a415fcf` port moves the marker to
`IdInfo`. The matched `7cf1f57887` run reached GHC-83605 at the two
imported `PreludeFoldrProof` uses of `GHC.Internal.Base.foldr`. Disabling
rules in Showcase alone gave 500 output rows. The separate selected-method
prototype at `22ee2d86dd` reports that the installed `length` rule blocks
the closed `Prelude.length '[1, 2, 3]` proof. Its fixture tests that proof
with rules disabled. An older unmarked interface can already contain the
normalized method result. Keep the old provider as a separate upgrade
control and rebuild it to test the new dependency check. Find the
selected-method fixture with
`git show 22ee2d86dd:testsuite/tests/dependent/should_run/NativeClosedMethodRule.sh`.
[Installed length rules](../../../libraries/ghc-internal/src/GHC/Internal/List.hs#L284-L302)
and [checkpoint](../CHECKPOINTS.md).

Replay the `Origin.Image` interface from the selected-method fixture,
both as a synonym and as a nullary closed family. Build the provider at
`-O2` with rules disabled, `-fomit-interface-pragmas`, and
`-fno-code -fwrite-interface`. Give the ordinary client only its `.hi`.
The RHS is `Prelude.fmap Prelude.not ('Just 'True)`. The client checks
`Refl :: Image :~: 'Just 'False`. Try the rule
`forall f. fmap f (Just True) = Nothing` with `[~1]`. The negative control
must reject the proof with GHC-83605. Its runtime-only sibling must fire
the rule and return `Nothing` on `Just True`. For the positive control,
change just the pattern to `Just False`. The proof and its `Just True`
observation should succeed, and a second runtime call on `Just False`
must fire the rule and return `Nothing`. With `[~]` on the matching
pattern, the proof should succeed and neither call should fire the rule.

The archived `7cf1f57887` driver reproduced this call pair at `-O0`
and `-O2` with rewrite rules enabled. In all four form and optimization
combinations, the runtime-only variant fired the active rule at its
designated constructor. Both active variants gave GHC-83605 at `Refl`.
The `[~]` rule never fired and all four proofs passed. The separate
`22ee2d86dd` checker compares the rule head and first nominal type
constructor, so its source predicts the same rejection for both active
patterns.

The check can rule out an active rewrite by comparing the written
constructors of stable arguments. Unknown operands keep a conservative
rule dependency. The compiler also checks rules on the selected
selector, dictionary and worker. A fold proof that exposes an open
`foldr` dependency still needs a rule certificate when imported with
rules enabled. This includes the `foldr` append and map laws.
[Closed method controls](../../../testsuite/tests/dependent/should_run/NativeClosedMethodApartness.sh).

GHC tests each user rule's activation in the simplifier phase, then matches
its arguments. The rules are optimizer inputs whose equivalence GHC
assumes. Agda registers an equality proof as a computation rule and uses
it on terms left neutral by ordinary reduction. An Agda import brings in
all global rules transitively, including private ones, to preserve the
types of imported definitions during reduction. Agda can also check
confluence. [GHC rule phases and semantics](../../users_guide/exts/rewrite_rules.rst),
[GHC rule matching](../../../compiler/GHC/Core/Rules.hs#L739-L806)
and [Agda 2.8.0 rewriting](https://agda.readthedocs.io/en/v2.8.0/language/rewriting.html#importing-rewrite-rules).
#### A proof whose method is reduced in a type synonym

The private importer prototype records the checked method, dictionary
and worker when they are used in a complete function signature. It
reports GHC-83605 at an ordinary use of the imported proof when a
conflicting rule is active. It preserves the dependencies through
ordinary value bindings that forward the proof.

A nullary synonym can compute the same result before signature checking.

```haskell
type Image = Prelude.fmap Prelude.not ('Just 'True)

image :: Image :~: 'Just 'False
image = Refl
```

At private revision `e6d4218a72`, the interface writes `Image` as
`'Just 'False` and omits the method dependencies. A source-hidden
ordinary client with the rule above still prints `Refl` and `Nothing`
at `-O2`. The compiler needs to preserve the checked dependencies of
`Image` when it checks and exports `image`.

The private follow-up records the selector, the selected ground
dictionary and its direct worker when it checks the closed call.
A stripped interface carries those dependencies on `Image`.
An ordinary module can export `type Indirect = Image`, and another
ordinary module can export `image :: Indirect :~: 'Just 'False`.
A source-hidden client of that proof gets GHC-83605 at its use
with the conflicting rule enabled. Disabling rewrite rules preserves
the proof use. A different export retains its ordinary runtime behavior.

Changing the source from a literal synonym RHS to the checked call
changes its interface fingerprint even though both RHSs reduce to
`'Just 'False`. The compiler then checks each dependent module again.
The experiment covers direct ground dictionary workers and named
synonym references. The compiler relies on metadata written when it
checks the producer. See the interface upgrade case below.

Three method sources passed the private replay. A signature with a
rigid callback records the selected `Functor Maybe` method. The
installed list proof from `01c41a57a1` still requires rules off because
the active fusion rules fail the existing mapper check. A separate
list worker with a validated identity rule accepts at `-O2` and rejects
a new selector or strict cons rule. The closed Maybe call above takes
the interpreter path and records the actual ground dictionary and
worker from specialization.

Two further forms still compiled at `85db325d58` and printed `Refl`
and `Nothing` in the presence of the conflicting rule above.

1. Replace the private type synonym in the proof provider with a
   nullary closed family. The interface axiom contains the reduced
   RHS, and the ordinary client uses the proof with source hidden.

   ```haskell
   type family Image where
       Image = Prelude.fmap Prelude.not ('Just 'True)

   image :: Image :~: 'Just 'False
   image = Refl
   ```

2. Import only the marked type synonym. Define and use the proof in
   the same ordinary module as the rule. The local use does not pass
   through the imported value check.

   ```haskell
   import Origin (Image)

   image :: Image :~: 'Just 'False
   image = Refl

   main = do
       print image
       print (run True)
   ```

The eager nominal equality check now records the dependency when
expansion of the imported synonym immediately proves the equality.
This catches the named proof above, a let-bound proof, and a direct
`(Refl :: Image :~: 'Just 'False)` at the `Refl` site. These forms
still compile with rewrite rules disabled. An unrelated term import,
`Proxy Image`, and `Refl :: Image :~: Image` require no such check.

A Given can also expose the imported synonym during canonicalization:

```haskell
fromGiven :: forall (a :: Maybe Bool). a ~ Image => Proxy a -> a :~: 'Just 'False
fromGiven _ = Refl
```

The eager check leaves a Wanted. The solver rewrites it with
`a ~ Image` and expands `Image` to prove the result. When expansion of
a marked nullary synonym fills the Wanted's coercion hole with a
reflexive proof, GHC now checks the original `Refl` site. This uses the
normal rule flags. Two Givens that rewrite both sides to the same
`Image` can prove reflexivity without expanding it.

The single-equation nullary closed family now stores the dependency
on its nominal axiom. Its interface adds the
`source-proof-rule-axioms` requirement and retains the same result
type. The solver checks the rule when use of that axiom proves the
ordinary client's nominal equality. `Refl :: Image :~: Image` and
`Proxy Image` can still compile with the rule enabled. If a complete
signature's binding proves the equation in the producer, an ordinary
client that only imports the resulting proof gets GHC-83605 at the
term use. A different export remains usable.

An interface from the previous compiler can contain the same
normalized axiom and provider ABI for the promoted and literal
RHSs. A new compiler reads those existing interfaces in warm home
builds and in installed packages. Rebuild the checked provider to
write the dependency and protect future proof uses.
[Old provider rebuild control](../examples/open-evaluation/check-source-proof-provider-upgrade.sh).

The combined R11/R16 compiler also changes the global interface version
from v69 to v79. Run the same control with
`--different-interface-versions` when the old provider uses v69.
The v79 reader rejects the old interface at the import. After rebuilding
the provider, the client rejects the conflicting rule at `Refl` for
both a type synonym and a closed family. It accepts both proofs when
rewrite rules are disabled.

The new axiom fingerprint invalidates a dependent module with the same normalized
family result. The same writer uses the established interface
encoding for an ordinary literal equation.

A closed family with arguments records dependencies by axiom branch.
For example, `Image 'True` can use the checked method call while
`Image 'False` returns the same type from a literal. A conflicting
`fmap` rule prevents an ordinary client from proving the first
equation with `Refl`. The literal equation, reflexivity at
`Image 'True`, and a type-only use still pass. Disabling rewrite rules
allows both equations.

The nominal equality solver reads the index from the axiom in the
reduction proof. The proof can also include the checked branch when
another family reduces its argument. The solver records dependencies
at the original proof site when the rewritten equality is reflexive.
A checked method synonym also records its dependency when the
reduction uses an axiom from its expanded private family. For example,
a proof about `MappedEither f (Image 'True)` can use both the mapping
axiom and the checked branch of `Image`. A literal receiver branch
adds no dependency of its own.

The [Showcase provider](../examples/open-evaluation/CheckedEitherInput.hs)
uses the two branches to produce the same `Right` value. A
[source-hidden test](../../../testsuite/tests/dependent/should_run/NativeOpenEitherFmap.sh)
checks both branches through an arrow argument and a method synonym.
It also checks a family that inspects the synonym result and a
constant family that can select its equation without inspecting it.

The interface adds the `source-proof-rule-branches` requirement for
indexed dependencies. It keeps the old encoding for unmarked axioms
and marked nullary axioms. A reader without this feature rejects the
indexed interface before reading its declarations.

A warm home build rechecks an experimental module with a user closed
family in the newly checked subset. Its old interface may contain only
the normalized equation, so the check also covers eligible literal
providers. The review records `source-proof-branches-reviewed-v1` in
the interface. The full interface hash includes that field. A literal
provider keeps its ABI. A second unchanged build reuses the reviewed
interface. This migration leaves
the earlier single nullary equation and synonym formats in place.

An older installed package can expose the same normalized equation
for `fmap not ('Just 'True)` and the literal `'Just 'False`. With
`-fno-write-if-self-recomp`, the two source modules produced identical
interface and object files under the same module and unit name. An
ordinary client can accept the old equation even when a local rule
changes the call. Rebuilding the provider restores the dependency and
GHC reports GHC-83605 at the proof use.

To rebuild the provider in a Cabal project,
list the provider's source checkout as a local package alongside the
client. For example, put `packages: ./provider ./client` in
`cabal.project`. Build both packages in a fresh build directory with
the matching tools from the new compiler installation.

```sh
cabal build all --builddir=dist-r16 \
  --with-compiler=/path/to/new/bin/ghc --with-hc-pkg=/path/to/new/bin/ghc-pkg
```

[Two-compiler warm check](../examples/open-evaluation/check-source-proof-branch-upgrade.sh)
and [home recompilation check](../../../compiler/GHC/Iface/Recomp.hs).
