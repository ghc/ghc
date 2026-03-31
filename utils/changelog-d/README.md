# changelog-d

Adapted from [changelog-d](https://codeberg.org/fgaz/changelog-d).
Generates RST release notes from changelog fragments for GHC's Sphinx documentation.

## For contributors

Add a file in `changelog.d/` for each user-facing change in your MR.
Use a descriptive filename (e.g. `fix-datakindsinfix`, `add-poll-iomgr`).

```cabal
section: language
synopsis: Fix a bug where GHC would erroneously accept infix promoted
  data constructors without enabling :extension:`DataKinds`.
issues: #26737
mrs: !15609

description: {
  As a result, you may need to enable :extension:`DataKinds` in code that did not
  previously require it.
}
```

**Required fields:** `section`, `synopsis`, `mrs`, `issues`

**Optional fields:** `description`

If your MR doesn't need a changelog entry, apply the `no-changelog` label.

### Fields

| Field         | Format                          | Description                                    |
| ------------- | ------------------------------- | -----------------------------------------------|
| `synopsis`    | Free-form RST                   | Brief description of the change                |
| `mrs`         | `!N` (space-separated)          | MR number(s)                                   |
| `issues`      | `#N` (space-separated)          | Issue number(s)                                |
| `section`     | Section key (see below)         | GHC component                                  |
| `description` | Free-form RST in `{ ... }`      | Extended details. Printed after the main entry |

### Section keys

| Key                | Heading                          |
| ------------------ | -------------------------------- |
| `language`         | Language                         |
| `compiler`         | Compiler                         |
| `profiling`        | Profiling                        |
| `codegen`          | Code generation                  |
| `llvm-backend`     | LLVM backend                     |
| `js-backend`       | JavaScript backend               |
| `wasm-backend`     | WebAssembly backend              |
| `ghci`             | GHCi                             |
| `rts`              | Runtime system                   |
| `linker`           | Linker                           |
| `bytecode`         | Bytecode compiler                |
| `packaging`        | Packaging & build system         |
| `cmm`              | Cmm                              |
| `build-tools`      | Build tools                      |
| `base`             | ``base`` library                 |
| `ghc-prim`         | ``ghc-prim`` library             |
| `ghc-lib`          | ``ghc`` library                  |
| `ghc-heap`         | ``ghc-heap`` library             |
| `ghc-experimental` | ``ghc-experimental`` library     |
| `template-haskell` | ``template-haskell`` library     |
| `ghc-pkg`          | ``ghc-pkg``                      |
| `ghc-toolchain`    | ``ghc-toolchain``                |

### Entry format

Changelog entries use the same format as `.cabal` files, parsed by the Cabal
library's `Distribution.Fields` parser
(see [Cabal file format documentation](https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html#package-descriptions)):

- Each field is a `name: value` pair.
- To continue a field value on the next line, indent it relative to the
  field name.
- Lines whose first non-whitespace characters are `--` are comments.
- As an alternative to indentation, you can use explicit braces `{}` to
  delimit multi-line field values (see `description` in the example above).
  See the Cabal documentation on
  [layout](https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html#layout)
  for details.

## Configuration

The file `changelog.d/config` declares the structure of the generated release
notes: required fields, section names, preamble text, and the included-libraries
table. Edit it when adding new sections or changing release note formatting.

## For maintainers

### Hadrian targets

Generate release notes:
```
hadrian/build changelog                              # uses project version
hadrian/build changelog --changelog-version=10.2.1   # explicit version
```
Output: `docs/users_guide/<version>-notes.rst`

Clear entries after branch cut:

```
hadrian/build changelog-clear
```

Validate entries:

```
hadrian/build test --only=changelog-d
```
