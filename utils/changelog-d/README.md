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

**Optional fields:** `description`, `clc`

**Conditionally required**: entries with `section: base` MUST also include a `clc:`
field referencing the CLC proposal authorising the change.

If your MR doesn't need a changelog entry, apply the `no-changelog` label.

### Fields

| Field         | Format                     | Description                                           |
| ------------- | -------------------------- | ----------------------------------------------------- |
| `synopsis`    | Free-form RST              | Brief description of the change                       |
| `mrs`         | `!N` (space-separated)     | MR number(s)                                          |
| `issues`      | `#N` (space-separated)     | Issue number(s)                                       |
| `clc`         | `#N` (space-separated)     | CLC proposal number(s). Required for `section: base`. |
| `section`     | Section key (see below)    | GHC component                                         |
| `description` | Free-form RST              | Extended details. Printed after the main entry        |

### Section keys

The "Markdown" column indicates whether entries in that section also flow to
a per-library `changelog.md`. Sections without a
Markdown target appear only in the GHC release notes RST.

| Key                | Heading                      | Markdown target                                |
| ------------------ | ---------------------------- | ---------------------------------------------- |
| `language`         | Language                     | —                                              |
| `compiler`         | Compiler                     | —                                              |
| `profiling`        | Profiling                    | —                                              |
| `codegen`          | Code generation              | —                                              |
| `llvm-backend`     | LLVM backend                 | —                                              |
| `js-backend`       | JavaScript backend           | —                                              |
| `wasm-backend`     | WebAssembly backend          | —                                              |
| `ghci`             | GHCi                         | —                                              |
| `rts`              | Runtime system               | —                                              |
| `linker`           | Linker                       | —                                              |
| `bytecode`         | Bytecode compiler            | —                                              |
| `packaging`        | Packaging & build system     | —                                              |
| `cmm`              | Cmm                          | —                                              |
| `build-tools`      | Build tools                  | —                                              |
| `base`             | ``base`` library             | `libraries/base/changelog.md`                  |
| `ghc-internal`     | ``ghc-internal`` library     | `libraries/ghc-internal/CHANGELOG.md`          |
| `ghc-prim`         | ``ghc-prim`` library         | `libraries/ghc-prim/changelog.md`              |
| `ghc-lib`          | ``ghc`` library              | —                                              |
| `ghc-heap`         | ``ghc-heap`` library         | —                                              |
| `ghc-experimental` | ``ghc-experimental`` library | `libraries/ghc-experimental/CHANGELOG.md`      |
| `template-haskell` | ``template-haskell`` library | `libraries/template-haskell/changelog.md`      |
| `ghc-pkg`          | ``ghc-pkg``                  | —                                              |
| `ghc-toolchain`    | ``ghc-toolchain``            | —                                              |

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
notes: required fields, section names, preamble text, the included-libraries
table, and the `markdown-targets:` mapping that wires sections to per-library
`changelog.md` files. Edit it when adding new sections or changing release-note
formatting.

The `markdown-targets:` block is the source of truth for "which section's
entries get a Markdown emission, and which extra fields (e.g. `clc`) are
required for that section." Each line is `<section-key> <path> [<extra-required-field>...]`.

## For maintainers

### Hadrian targets

Generate RST release notes (existing behaviour):
```
hadrian/build changelog                              # uses project version
hadrian/build changelog --changelog-version=10.2.1   # explicit version
```
Output: `docs/users_guide/<version>-notes.rst`

Generate per-library Markdown bullets:

```
hadrian/build libraries-changelog-markdown
```

Output is one stream containing every configured `markdown-targets:` section.

Clear entries after branch cut:

```
hadrian/build changelog-clear
```

Validate entries:

```
hadrian/build test --only=changelog-d
```

### RST -> Markdown rewrite rules

`--libraries-changelog-markdown` rewrites the inline RST in each entry to Markdown:

| RST                                              | Markdown                                                                                               |
| -------------------------------------------------| ------------------------------------------------------------------------------------------------------ |
| ``code`` (double-backtick)                       | `code` (single-backtick)                                                                               |
| `text <url>`_                                    | [text](url)                                                                                            |
| :ghc-ticket:`N`                                  | [#N](https://gitlab.haskell.org/ghc/ghc/issues/N)                                                      |
| :ghc-mr:`N`                                      | [!N](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/N)                                            |
| :ghc-wiki:`p`                                    | [p](https://gitlab.haskell.org/ghc/ghc/wikis/p)                                                        |
| :clc:`N`                                         | [CLC proposal #N](https://github.com/haskell/core-libraries-committee/issues/N)                        |
| :ghc-flag:`-foo`                                 | `-foo`                                                                                                 |
| :extension:`E`                                   | `E`                                                                                                    |
| :ghci-cmd:`X`, :rts-flag:`X`                     | `X`                                                                                                    |
| :base-ref:`Mod.id` ``                            | `Mod.id`                                                                                               |
| :th-ref:, :cabal-ref: ,:ghc-prim-ref:            | `ref`                                                                                                  |
| .. code-block:: lang + indented body             | Triple-backtick fenced block with `lang`                                                               |
| .. note:: / .. warning::                         | `> **Note:**` / `> **Warning:**` blockquote                                                            |


