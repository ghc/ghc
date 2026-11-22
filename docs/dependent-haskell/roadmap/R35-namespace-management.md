# R35. Namespace management

A library can export a type and a constructor with the same spelling.
A client can give each name a distinct qualifier, even if another
module re-exports both. This matters for a required type argument,
where the caller can write a type in an expression.

```haskell
-- Provider exports both names from data Key = Key.
import qualified Forward as Ty  (type Key)
import qualified Forward as Val (data Key)

keep :: forall a -> a -> a
keep (type a) x = x

example = keep (type Ty.Key) Val.Key
```

The regression builds the provider and re-exporter first. The client
receives their interfaces and object files. It uses `Val.Key` as a
promoted index and as a constructor. Template Haskell looks up
`''Ty.Key` and `'Val.Key` separately. Each name carries a different
annotation. The executable prints `("type","constructor")` with both
pun warnings treated as errors. A second client instead imports both
names unqualified. At `use = Key`, GHC-41293 identifies the selected
constructor import and the competing type import.
[Source-hidden regression](../../../testsuite/tests/rename/namespace-interfaces/NamespaceInterfaces.sh)
and [client](../../../testsuite/tests/rename/namespace-interfaces/Main.hs).

## Scope and checks

Serokell lists four prerequisites for this card. R37 supplies namespace
specifiers on imports. R38 allows a module to turn off list and tuple
puns. R42 supplies term variable capture. R36 adds the two optional
warnings that detect reliance on the separate namespaces.
[Serokell scope](https://ghc.serokell.io/dh#namespace-management)
and [R36 checks](R36-pun-warnings.md).

Job 957 ran 25 normal-way registrations on compiler `2b64ffc406`.
All 25 passed. The table groups them by the source construct they check.

| Source construct | Registered checks |
| --- | --- |
| Imports, re-exports, promotion, required type arguments and annotations across interfaces | `NamespaceInterfaces`, `DisambiguatedPunUses` |
| Type and value fixities, quoted declarations and invalid namespace selectors | `T14032a`–`T14032f` |
| Namespace-specific warnings and deprecations | `T24396`, `T24396c` |
| Annotations and Template Haskell name quotes | `ann01`, `annfail02`, `TH_nameSpace` |
| List and tuple syntax | `ListTuplePunsFamiliesCompat`, `ListTuplePunsFail6` |
| Term variable capture | `T22513a`–`T22513j` |

Proposal #65 separately describes a multi-release transition for old
fixity declarations and pragmas that omit a namespace. These checks
cover the explicit syntax and its meaning. The release policy for old
source belongs to that transition. Agda also uses qualified names to
select definitions made available through modules. The Haskell spelling
and warning rules come from GHC proposals #581, #281 and #765.
[GHC proposal #65](https://ghc-proposals.readthedocs.io/en/latest/proposals/0065-type-infix.html#migration-plan),
[namespace-specified imports](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0581-namespace-specified-imports.rst),
[visible forall](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst),
[pun warnings](https://ghc-proposals.readthedocs.io/en/latest/proposals/0765-pun-warnings.html)
and [Agda's module system](https://agda.readthedocs.io/en/stable/language/module-system.html#basics).
