# R21. Dependency analysis

Serokell marks this card accepted. It requires the compiler to order
top-level declarations when a type depends on a term. R13 and R20 both
list R21 as a dependency. M126 extends the fork's declaration ordering
to qualified local calls handled by the open evaluator.
[Serokell card](https://ghc.serokell.io/dh#dependency-analysis),
[declaration analysis](https://serokell.io/blog/dependency-analysis-haskell),
and [declaration scheduling](../../../compiler/GHC/Tc/Module.hs).

## M126 source check

```haskell
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures #-}
module Example where
import Data.Kind (Type)

type Result :: Type -> Type
type Result a = Example.keep a

keep :: a -> a
keep x = x
```

The renamer records the resolved term Name and its source location.
When an independent producer with a complete signature belongs to the
same declaration group, the compiler checks it before the dependent
type, regardless of source order. The compiler solves and zonks the
producer before it gives the observer checked Core. The observer
returns the original rigid type argument or reports a demand it cannot
satisfy. The producer's ordinary forall binds a type parameter.
[Open evaluator](../../../compiler/GHC/Tc/Gen/OpenTypeEval.hs).

A later group can use a producer whose constraints the compiler solved
before an intervening top-level splice. The compiler reuses the checked
bindings and evidence on demand. It rejects pending typed splices in a
producer before zonking can execute them. A definition after a splice
stays out of scope before that splice. A direct cycle through the
producer's signature is reported at the type-level call.
[Local open evaluation example](../examples/open-evaluation/LocalOpen.hs)
and [source and interface regression](../../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh).

An ordinary client compiled with only the exported interface. Changing
the producer to select its second argument changed the exported synonym.
The unchanged client then failed where it supplied a Boolean for the
newly selected `Char` type.

## M128. Following a local dependency chain

The scheduler starts at the function mentioned in a type and follows
its local dependencies. It uses the renamer's existing value-binding
groups and their order. Ordinary inference can supply the types of
both the function and its helpers. A shared signature can cover a
selected function and an unrelated one. Each group keeps the part
that belongs to its own bindings.

For example, `type LastOf a b = lastOf a b` observes the ordinary
definition `lastOf x y = select y x`. When `select x _ = x`, the
result type is `b`. This unqualified use follows the usual term lookup
with `RequiredTypeArguments`. An explicit `forall a.` also lets `pick a`
refer to the local function. In an ordinary implicitly quantified
signature, `pick` can still bind a type variable. The regression supplies
`@Maybe` for that variable with visible type application. An explicitly
bound type variable takes precedence too. The fallback for an unqualified
ordinary function applies to a spine of named variables or pairs of
named variables. Closed constructor inputs retain the checked-equation
lookup and its diagnostics for incomplete clauses or invalid recursion.
[Source example](../examples/open-evaluation/LocalOpen.hs).

Scheduling a helper makes its checked body available. Its rewrite rules
affect the proof only if the observer uses that body. In the regression,
`select x (unused y)` returns `x`. A rule on `unused` is accepted.
Adding a rule that changes `select` to choose its second argument is
rejected at the rule. Changing the two-calls-deep helper's definition
also invalidates the unchanged ordinary client.
[Rule and recompilation checks](../../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh).

The scheduler can follow previously solved helpers across a top-level
splice. A pending typed splice in a reachable body still blocks early
observation. This slice reports a recursive term group and a cycle through
a type declaration separately. Mixed dependencies on same-group type
declarations, local instances, nonempty deriving, defaults and deferred
errors remain open.

## Local synonyms in a retained field worker

A retained GADT field can call a local worker whose signature uses
`WorkerArgument Bool` for `type WorkerArgument a = a`. The scheduler
checks the synonym and its renamed dependencies before the worker.
The source controls also use a nullary synonym. GHC uses the worker's
checked equations to check the retained field and the remaining
declarations in the same group.

A locally declared synonym can also come from an earlier group. GHC
uses its checked type constructor to verify saturation without moving
that declaration. The example gives the same result with an empty splice
after the synonym. Both qualified and unqualified calls to a
worker in a later group give GHC-76037 at the call head. A separate type
occurrence can establish an implicit constructor binder of the same name.
Such a binder still gives GHC-97233 when the retained case cannot lift
that application. A worker whose helper needs the unfinished field
constructor still gives GHC-68947. The ordinary client uses stripped
interfaces and observes a character at False and an integer at True.
[Source example](../examples/open-evaluation/LocalAliasField.hs) and
[source controls](../../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh).

## M131. Mutually recursive value bindings

GHC can typecheck a recursive group of ordinary functions before
observing a call. It keeps every member of the renamer's group together.
In `select x (loop y)`, the selector returns `x` without evaluating the
recursive argument. A call that does demand the loop spends the open
evaluator's reduction budget and reports GHC-70566 at the call. This
checks the particular observation. A declaration cycle in which a type
requires its own unfinished producer still fails before observation.
[Runnable selection](../examples/open-evaluation/LocalOpen.hs)
and [cycle and demand controls](../../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh).

Signed and inferred mutually recursive functions both work. For an
inferred group, the typechecker already records the monomorphic binder
that implements each polymorphic source function. The observer uses
that correspondence when it demands a recursive reference. A rule on
the other function therefore fails if its body is read and succeeds
if the argument containing that call is ignored.
[Inferred group with a source rule](../../../testsuite/tests/dependent/should_run/NativeLocalOpenSibling.hs).

## M134. Local type and value dependencies

A function used in a type can use constructors and type synonyms from the
same module. In the source example, `route` passes the constructor
`Second` to a helper. The helper's signature mentions the local datatype
`Route` and the local synonym `ResultOf`. The renamer has already put each
type declaration with its standalone kind signature, role annotations,
and mutually recursive peers. The scheduler moves the required groups
into scope before checking the helper and observing `route`.
[Local routing example](../examples/open-evaluation/LocalOpen.hs).

Sometimes a prerequisite type itself calls another local function.
For example, `type Final a = outer a`, `outer :: a -> Middle a`,
and `type Middle a = inner a` require two passes. First GHC checks
`inner`. With its definition available, GHC can check `Middle`, then
`outer`, then `Final`. The declarations can appear in reverse order.
If a helper's result synonym refers back to the consumer itself, GHC
reports the type dependency cycle at the original call. Local instances
and nonempty deriving clauses still require more declaration analysis.
[Acyclic and cyclic source checks](../../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh).

## Closed calls

An ordinary qualified call on a known closed literal still requires an
imported definition. GHC-70565 reports that restriction at the call
head. The regression first compiles the producer without the type-level
call, then adds a closed call whose equality would hold if the compiler
used the stale object. The compiler rejects the call with the old
object present. R15 tracks interpreter execution separately.
[Type-functions draft](https://github.com/ghc-proposals/ghc-proposals/pull/509)
and [R15](R15-closed-term-evaluator.md).

## M138. Deriving clauses and module defaults

A local function can now occur in a type in a module that derives
instances or declares defaults. The scheduler checks the datatypes
needed by attached and standalone deriving declarations in the first
pass. It checks the types named by default declarations there too.
It passes these declarations to GHC's ordinary instance and default
checking code. Subsequent passes inherit the resulting environments.
[Source scheduler](../../../compiler/GHC/Tc/Module.hs).

The source regression derives stock instances and `Generic`, derives
`Show` separately for a second datatype, and derives a local class
with a default method. A local type synonym supplies `Double` in
`default (ReportNumber)`. The ordinary interface-only consumer uses
the selected result at `Bool`, calls the derived methods, and observes
`"1.0"` and `"2.0"` from overloaded expressions checked in different
passes. The Showcase uses a local `Show` instance at `Routed Bool String`.
[Producer](../../../testsuite/tests/dependent/should_run/NativeLocalOpenDerived.hs),
[ordinary consumer and cycle control](../../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh)
and [Showcase library](../examples/open-evaluation/LocalOpen.hs).

A deriving clause may itself need the unfinished function. GHC reports
which declaration needs that function at the type-level call. A local
explicit instance still blocked this scheduling path at M138. Using a derived
instance to typecheck ordinary code also leaves a separate evaluator
question. An open observation that actually calls the class selector
needs a definition the evaluator can follow.

## M147. Separate an instance head from its methods

In the [source example](../examples/open-evaluation/LocalInstance.hs),
`keep x = const x (enabled IdentityPass)` needs the local `Pass IdentityPass`
instance to typecheck. Evaluating `keep` on an unknown type returns that
type without calling `enabled`. This lets the module define
`type Input a = LocalInstance.keep a` before it checks either method.

The scheduler obtains the dependencies of the instance head from its
renamed type and modifiers. It checks the prerequisite type declarations,
then adds the instance head to GHC's ordinary instance environment. The
instance methods can now follow the remaining value bindings. One method
in the example uses `LocalInstance.later a` directly in a type. The type
of `later` in turn uses `Input`, so the compiler checks the two
producers in different groups. The compiler keeps both checked
definitions in scope for the deferred method pass. Each method is
checked once within the same splice group.

A caller with the extension off can import `passValue` and use it at
`Int` and `Bool`. Making `keep` inspect `enabled IdentityPass` forces a
dictionary whose method still needs `Input`. The compiler rejects this
cycle at the local call in `Input`. It also checks an unused method body
and finishes the methods before a following top-level splice.

An associated type equation can also supply a prerequisite for the
producer. In the regression, `not (enabled Token)` needs
`Flag Token ~ Bool`. The instance gives `type Flag Token = Answer`
and a local synonym gives `type Answer = Bool`. GHC checks the synonym
and installs the equation before checking the producer. If the equation
instead calls the unfinished `keep`, GHC names that dependency at the
source call. M150 extends scheduling to associated data instances and
standalone family instances.
[Associated equation](../examples/open-evaluation/LocalAssociated.hs).
[Scheduler](../../../compiler/GHC/Tc/Module.hs)
and [consumer and strict cycle](../../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh).

## M150. Constructors and selectors of family instances

The ordinary term `StampToken True == StampToken False` needs the data
constructor of an associated data instance and its derived `Eq` instance.
A standalone data instance defines the record selector `contents`. Its
field type reduces through a standalone type instance. GHC checks all
three declarations before the local producer `keep`. The instance
method can then use `keep` in a type. An ordinary module imports the
result and runs the method, equality, and derived `Show` code.
[Source example](../examples/open-evaluation/LocalDataFamily.hs).

The scheduler reads constructor field types and deriving clauses with
their instance. GHC's ordinary instance pass installs the constructors
and selectors. A field type that calls the unfinished producer creates
a cycle. The diagnostic names the producer and identifies the kind of
instance at the source call. Data-family constructor promotion is a
separate upstream feature, R14. The example uses the constructors in
ordinary terms. [Registration and cycle checks](../../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh).

## M152. Independent local producers for type instances

A standalone equation such as `type instance Payload Int = M.ident Bool`
can use an independent local producer. For an open type family declared
in the same splice group, the scheduler can defer the equation and its
family until the producer has checked. It passes the solved producer
Core to the ordinary family instance checker. The symbolic observer
can return a checked closed type such as `Bool` or `Maybe Bool`.
A strict producer that demands this operand still stops. A type supplies
no runtime value. The scheduler keeps all same-group equations for the
family together and preserves their renamed
dependencies. An ordinary client can use the resulting axioms through
the producer's interface. GHC rejects conflicting equations in its
usual family consistency check. The example gives its later synonym a
standalone kind signature so its operand is a rigid binder during open
observation.

This split requires the family to be absent from the early dependency
closure, which includes the instance, deriving and default declarations
checked early. If an earlier declaration depends on that family, GHC
rejects the postponement at the original dependent call. This split
covers standalone type equations for a local family. [Source and cycle
checks](../../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh).

In the [shared example](../examples/open-evaluation/IndependentFamily.hs)
the second producer uses the family after its equations are installed.

## M156. Independent producers for imported type families

An imported open family can have a local equation that calls an
independent producer. GHC checks an early batch containing only
functions with complete, family-free variable-and-arrow signatures
and bodies made of named variables, applications, and ordinary lambdas.
The checker verifies the solved Core, including binder types and
absence of evidence casts, before open evaluation uses it. The
remaining group keeps its class instances, deriving, defaults and
family instances in their ordinary order.

The first three equations in the example use a reordered projection
through a local helper, a lambda application and an identity. Its
ordinary class method needs the Boolean equation. The first three
results have types `Bool`, `Ordering`, and `Char` in an ordinary
client that sees only the interfaces.
[Imported family example](../examples/open-evaluation/ImportedFamily.hs)
and [separate compilation checks](../../../testsuite/tests/dependent/should_run/NativeImportedFamily.sh).

A producer that uses an imported operation or an explicit annotation
can have a dependency on the postponed family. The early fragment
rejects both the hidden-dictionary example below and a producer that
ignores a term annotated at `Payload Int`. Optimization could remove
the annotation's evidence after checking it. The scheduler therefore
checks source syntax and evidence before optimization.

An explicit declaration-group boundary already works for the independent
case. Put `keep x = x` before `$(pure [])` and the equation
`type instance Payload Int = M.keep Bool` after it. A separate ordinary
client checks the Boolean result using the interfaces alone. Moving such
a boundary changes which instances are visible to earlier declarations.
[GHC's declaration-group rules](../../users_guide/exts/template_haskell.rst).

An ordinary example shows why the dependency can be invisible in the
producer source. An imported `hidden` needs `Branch (Payload Int)`.
The producer writes `choose x y = if hidden Proxy then x else y`.
The provider has an incoherent blanket instance that returns `False`
and a `Branch Bool` instance that returns `True`. Before the equation
`type instance Payload Int = Bool`, the ordinary program returns
`False` for `choose True False`. Adding that equation gives `True`
with the same producer body. Checking whether `Payload` occurs in the
source of `choose` misses the dependency. An imported-family scheduler
must preserve the dictionary selected under the complete instance
environment. A second producer that calls `const x (hidden Proxy)`
returns `x` in both runs. Observing that result alone cannot detect
the difference in dictionary selection.

## M160. Independent imported function types

The scheduler can use the signature in a stripped interface to check the
local binding. Computing its result still requires the imported body
or a checked equation. The example provider exposes the unfoldings of its
two selectors in object and bytecode builds. GHCi otherwise reports
GHC-70566 at the family equation when it tries to read `importedFirst`.

An early producer can call an imported function whose complete
interface type belongs to the family-free fragment. The check covers
invisible binder kinds as well as all input and result types. It
admits lifted type variables and ordinary unrestricted arrows. Class
contexts and family-dependent types stay outside the fragment.

The shared example uses `Prelude.id` and an ordinary imported
projection. The scheduler checks their types before moving the local
producer. Open evaluation checks each demanded unfolding and records
the original function for RULE validation. A strict imported helper
cannot inspect the supplied type operand, and a matching RULE on an
observed helper rejects the proof. Changing the imported projection
to choose the other operand changes the family equation to `Char`.
The original Boolean method then fails to typecheck. [Separate
compilation, demand and RULE checks](../../../testsuite/tests/dependent/should_run/NativeImportedFamily.sh).

## M163. Ordinary data in an independent producer

The early check now admits saturated boxed data types such as `Maybe a`
when their kind telescopes and arguments are independent. For a source
constructor use, it resolves the constructor to its exact worker and
checks the saturated application in solved Core. The constructor must
have an ordinary boxed representation and lazy fields.

The fourth equation uses `Prelude.maybe x Prelude.id (Just x)` to
return the supplied type `Maybe Bool`. The ordinary interface-only
client checks that component at `Maybe Bool`. The negative calls an imported function
whose declared type contains `Maybe (Payload Int)`. Although the
producer never writes the family name, its early check rejects the
dependency at the equation's call.
[Source checks](../../../testsuite/tests/dependent/should_run/NativeImportedFamily.sh).

## M164. Inferred independent producer types

An independent local producer can use an implicit signature or omit
the signature. The scheduler preserves the original top-level binding
groups and flags. GHC uses ordinary type inference and generalization,
including under `MonoLocalBinds`. The checker verifies the solved
monomorphic binding and its wrapper before the open evaluator can
observe it. The source syntax still excludes overloaded literals,
annotations and splices, and the imported type check still excludes
hidden family arguments and dictionaries.

The example checks `viaMaybe :: a -> a` and infers the same type for
an unsigned producer. The resulting equations remain available to
ordinary client code through the interface. The client prints
`(True,EQ,'x',Just False,LT)`. A signed mutually recursive source
group remains outside the early batch. The scheduler checks resolved
body dependencies even when signatures break the renamer's typing
cycle. [Interface and rejection checks](../../../testsuite/tests/dependent/should_run/NativeImportedFamily.sh).

## Mixed type and term declarations

An imported family equation can use a local function that mentions a
local data constructor. In `MixedFamily`, `Payload Double` depends on
`keep`. The body of `keep` mentions `Envelope`. An unused constructor
of `Envelope` has a `Wrap a` field. GHC checks `Wrap` and `Envelope`,
then `keep`, then the family equation. This lets a later constructor
accept `True :: Payload Double`. An ordinary client reads the field
through the interface. [Source](../examples/open-evaluation/MixedFamily.hs)
and [consumer](../../../testsuite/tests/dependent/should_run/NativeImportedFamily.sh).

Move the whole group of mutually dependent type declarations identified
by the renamer. Keep its role annotations. Check imported references
in the field types as well. If the field of `Wrap` uses `Payload Double`,
there is a real cycle even though `keep` never applies that constructor.
GHC reports GHC-70566 at the reference to `keep` in the family equation.
An expression signature such as `(id :: a -> a)` can precede the
equation when `a` is the producer's scoped type variable.

A helper signature can also depend on a local closed family. GHC checks
the family, the types used in its equations and the helper before the
imported instance. In `FamilySignature.direct`, the call itself requires conversion. GHC uses the closed-family axiom
`DirectInput a = a` to adapt the function to the checked argument.
An ordinary client reads `True :: Payload Word` from the resulting
interface. A constructor field that refers back to the imported family
still creates a cycle. Forcing the symbolic argument and enabling a
matching term rewrite rule each block the proof.

The closed family can have a standalone kind signature such as
`type DirectInput :: Type -> Type`. Move the signature with its
renamed type group and check its references before moving it.
The earlier scheduler rejected this forward dependency with
GHC-70566. A signature that instead mentions the postponed
`Payload Int` equation still rejects, as does recursion through
two signed function bodies. The signature sets the kind and leaves
the body's value dependencies in place.
[Proposal #54](https://ghc-proposals.readthedocs.io/en/latest/proposals/0054-kind-signatures.html)
and [its review discussion on polymorphic recursion](https://github.com/ghc-proposals/ghc-proposals/pull/54#issuecomment-308906690).
[Checked declarations](../examples/open-evaluation/FamilySignature.hs)
and [interface, cycle, demand and rule checks](../../../testsuite/tests/dependent/should_run/NativeImportedFamily.sh).

A helper signature can also use a saturated local synonym. In
`FamilySignature`, `InputAlias a` uses `ForwardInput (DirectInput a)`.
GHC checks the synonyms and the closed family before the helper.
It checks every synonym argument for dependencies. The equation still
creates a cycle through `Discard a (Payload Int)`, even if `Discard`
omits that argument from its result. Synonym saturation and cycle
checks still apply.
[Proposal #425 on arity inference](https://ghc-proposals.readthedocs.io/en/latest/proposals/0425-decl-invis-binders.html#arity-inference)
and [the review on arity and scoping](https://github.com/ghc-proposals/ghc-proposals/pull/425#issuecomment-973555158).

## A datatype, its workers and their proof in one group

The private source at `60cc9f5d23` declares `Tree`, `Size`, addition,
`mapTree`, `nodes` and `nodesMap` in one declaration group. The
datatype dependencies determine when GHC can check the workers.
The type of `nodesMap` uses their checked equations. No splice or
separate provider module is needed for this example.

The scheduler retains each existing group of mutually dependent type
declarations. A regular Haskell98 datatype can move early when its
single parameter and recursive fields have the verified shape.
A genuine data/function cycle still rejects. Job 1160 checked the
example with HaskLib's `withEquality` at both optimization levels.
The client counted two forks and obtained `Refl` without forcing the
callback. The 555 compiler includes this source. The separate
[imported-rule check](R16-open-term-evaluator.md#an-imported-proof-and-a-client-rewrite-rule)
applies when another module uses the proof.
