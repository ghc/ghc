FileCheck in the GHC testsuite
==============================

Some tests check their output with LLVM's [FileCheck][1] instead of a golden
`.stderr`/`.stdout` file: the test file carries `CHECK:` directives next to
the code they test, and only the lines that matter are pinned. Such tests use
`filecheck_stderr` or `filecheck_stdout` in their `all.T` entry.

This document covers getting the `FileCheck` binary and writing such tests.

  [1]: https://llvm.org/docs/CommandGuide/FileCheck.html


Installing FileCheck
--------------------

Without a `FileCheck` binary every test that uses it is reported as a
framework failure and skipped:

    FileCheck not found: pass FILECHECK=... to configure, or run
    testsuite/driver/build_filecheck.py

Any LLVM 12 or newer should do. The directives and the regex dialect have not
changed in over a decade, and the youngest flag the driver passes,
`--allow-unused-prefixes`, appeared in LLVM 12. FileCheck is independent of
the LLVM used by GHC's `-fllvm` backend, so it need not fall into GHC's
supported LLVM version range.

### Where the driver looks

In this order, taking the first that exists:

1. The `FILECHECK` environment variable at test time, a path or a command
   name.
2. `_build/filecheck/bin/FileCheck` in the GHC tree, where
   `build_filecheck.py` installs to.
3. What `./configure` found, passed as `config.filecheck` by hadrian and by
   `make -C testsuite/tests`, which reads it from `hadrian/cfg/system.config`.
   Configure searches `PATH` the way it does for `llc`: `FileCheck-23` down
   to `FileCheck-12` (the Debian and Ubuntu names), `FileCheck23` down to
   `FileCheck12` (the FreeBSD names), then plain `FileCheck`, and rejects a
   version outside that range. `./configure FILECHECK=/path/to/FileCheck`
   overrides the search, which is needed where the binary is not on `PATH`
   (Alpine, Homebrew). The result is in configure's summary under "Using
   LLVM tools".

### Distribution packages

Checked against the package file lists or build recipes on 2026-11-05, not by
installing each one. Versions below LLVM 12 do not work (see above).

| System | Package | Binary | Found by configure? |
| --- | --- | --- | --- |
| Debian 11+, Ubuntu 20.04+ | `llvm-NN-tools` | `/usr/bin/FileCheck-NN` | yes |
| Debian 9, 10, Ubuntu 18.04 | `llvm-NN-tools` from [apt.llvm.org](https://apt.llvm.org) | `/usr/bin/FileCheck-NN` | yes |
| Fedora | `llvm` | `/usr/bin/FileCheck` | yes |
| Rocky 8, 9 | `llvm` | `/usr/bin/FileCheck` | yes |
| Alpine 3.16+ | `llvmNN-test-utils` | `/usr/lib/llvmNN/bin/FileCheck` | no, pass `FILECHECK=` |
| Arch | `llvm` | `/usr/bin/FileCheck` | yes |
| Homebrew | `llvm` or `llvm@NN` (keg-only) | `$(brew --prefix llvm)/bin/FileCheck` | no, pass `FILECHECK=` |
| Nix | `llvmPackages.llvm` | `bin/FileCheck` of that package | yes, once in `PATH` |
| FreeBSD, DragonFly | `llvmNN` (`LIT` option, on by default) | `/usr/local/bin/FileCheckNN` | yes |
| MSYS2 (all environments, including CLANGARM64) | `mingw-w64-<env>-llvm` | `FileCheck.exe` in the environment's `bin` | yes, in that shell |

The Debian and Ubuntu packages exist for amd64, i386, arm64, armhf, ppc64el,
s390x, riscv64 and loong64 (loong64 in unstable only).

Sources that do **not** ship FileCheck, so use the build script below:

- the official `clang+llvm-*` release tarballs and the LLVM Windows
  installer, which are built without `LLVM_INSTALL_UTILS`
- llvm-mingw
- MacPorts `llvm-NN`
- OpenBSD's `devel/llvm`, which removes it on purpose
- Alpine 3.12 to 3.15, whose only `-test-utils` package is LLVM 10

The Debian package is the cheapest route: it is a few hundred kB and does not
pull in the rest of LLVM. For example, on Ubuntu 24.04:

    sudo apt install llvm-18-tools

### Building from source

`testsuite/driver/build_filecheck.py` builds FileCheck from a pinned LLVM
release when no package is available:

    python3 testsuite/driver/build_filecheck.py

It needs Python 3, CMake 3.20 or newer, Ninja or Make, and a C++17 compiler.
It downloads the LLVM source tarball (about 180 MB), checks its SHA-256,
builds only the FileCheck target (about ten minutes on eight cores) and
installs a static binary of about 1 MB into `_build/filecheck/bin`. The
sources and build tree are deleted afterwards unless `--keep` is given. See
`--help` for `--prefix`, `--jobs` and `--tarball`.

### Checking the setup

    FileCheck --version
    hadrian/build test --only="T7803 cmm_sink_sp"

The second command fails with the framework failure above if the driver
cannot find the binary.


Writing FileCheck tests
-----------------------

### The `all.T` side

    test('T7803', [ filecheck_stderr() ], multimod_compile,
         ['T7803', '-O2 -v0 -dsuppress-all -dsuppress-uniques -dno-typeable-binds'])

`filecheck_stderr(check_file=None, prefixes=None)` replaces the comparison
against the golden `.stderr` file with a FileCheck run. `filecheck_stdout`
does the same for stdout. Both can be used in one test.

- `check_file` is the file holding the directives, relative to the test's
  directory. By default it is the test's own source file (`T7803.hs`), where
  directives live in comments next to the code. Give a separate file when
  the test has no suitable source file, for instance `ghci024.check` for a
  makefile test, or when several tests share one check file.
- `prefixes` lists the enabled check prefixes, by default just `CHECK`. With
  `prefixes=['ASM']` the `CHECK:` lines are ignored, so one file can carry
  checks for several configurations. T15155 and T15155l share `T15155.hs`
  this way, with `ASM:` and `LLVM:` lines.
- `WS32` or `WS64` is always enabled as well, matching the word size of the
  target, for the cases that used to need a `.stderr-ws-64` golden file. A
  prefix that no line uses is fine.

The input FileCheck sees is the *normalised* output, that is the same text a
golden file would be compared against: the standard normalisers plus any
`normalise_errmsg_fun` and friends the test adds. Empty output is accepted,
so a file with only `CHECK-NOT:` lines passes against it. Use a
`CHECK-LABEL:` or `CHECK:` on something that must be there to guard against
an empty dump.

### Directives

Directives sit anywhere in a line, so the comment syntax of the language does
not matter. The text after `PREFIX:` is the pattern, with leading and
trailing whitespace stripped. Horizontal whitespace in a pattern matches any
run of horizontal whitespace in the input.

| Directive | Meaning |
| --- | --- |
| `CHECK: text` | `text` occurs somewhere after the previous match |
| `CHECK-NEXT: text` | on the line right after the previous match |
| `CHECK-SAME: text` | on the same line, after the previous match |
| `CHECK-NOT: text` | does not occur between the previous and the next match |
| `CHECK-LABEL: text` | like `CHECK`, but splits the input: later checks cannot match before it, earlier ones not after it |
| `CHECK-DAG: text` | occurs, in any order relative to adjacent `CHECK-DAG` lines |
| `CHECK-EMPTY:` | the next line is empty |
| `CHECK-COUNT-N: text` | `N` consecutive occurrences |
| `COM: text` | a comment, never a check |

Every prefix takes the same suffixes: `WS64-NEXT:`, `ASM-NOT:`.

Patterns are literal text. A regex goes inside `{{...}}`, and `[[NAME:regex]]`
captures a match for later `[[NAME]]` references, which is how a unique or a
generated name is pinned across lines:

    -- CHECK: [[WRK:\$wfoo[0-9]*]] =
    -- CHECK: case [[WRK]] x of

Literal `[[` or `{{` in the output must be written as `{{\[\[}}` and `{{\{\{}}`.
Directives are not line-anchored. `{{^}}foo` anchors at a line start, since
FileCheck's `.` and `^` never cross a newline. In the converted tests the
dumps are made stable with `-dsuppress-uniques`, `-dsuppress-all` or
`-dno-typeable-binds` where that is enough, and with captures where not.

The full set of directives, numeric substitutions and the exact matching
rules are in the [FileCheck guide][1].

### Regular expressions

FileCheck uses POSIX extended regular expressions, not Python's or Perl's.
The important differences:

| Wanted | Perl, Python | FileCheck |
| --- | --- | --- |
| digit | `\d` | `[0-9]` or `[[:digit:]]` |
| whitespace | `\s` | `[[:space:]]` or `[ \t]` |
| word character | `\w` | `[[:alnum:]_]` |
| word boundary | `\bfoo\b` | `[[:<:]]foo[[:>:]]` |
| non-capturing group | `(?:a\|b)` | `(a\|b)` |
| non-greedy | `.*?` | not available, use `[^,]*` or split the check over `CHECK`/`CHECK-SAME` |

`\d`, `\s`, `\w` and `\b` are **not errors**: FileCheck reads them as the
quoted letter, so `{{\bjp\b}}` matches the literal text `bjpb` and a
`CHECK-NOT` built on it passes vacuously. `(?:` and `*?` are rejected with an
"invalid regex" error. `$`, `{n,m}`, alternation, back-references and the
POSIX classes work as usual.

### Platform differences

Use `WS64:`/`WS32:` where the output depends on the word size:

    -- CHECK-LABEL: T25650.baz_foo_closure:
    -- CHECK-NEXT:  const T25650.C_con_info;
    -- WS64-NEXT:   const 10 :: W32;
    -- WS32-NEXT:   const 10;
    -- CHECK-NEXT:  const 1 :: W8;

A `{{_?}}` absorbs the leading underscore of Darwin symbols. There are no
OS or architecture prefixes yet, so a test whose output differs beyond that
still needs `when(opsys(...), ...)` in `all.T`.

### Reading a failure

A failing check appears in the test summary under `FileCheck report:` with
FileCheck's own diagnostics: the directive that failed, where the previous
match ended, and an annotated dump of the input around the failure. For a
`CHECK-NEXT:` that matched one line too late:

    T.hs:2:16: error: CHECK-NEXT: is not on the line after the previous match
    -- CHECK-NEXT: baz
                   ^
    in.txt:3:1: note: 'next' match was here
    ...
    Input was:
    <<<<<<
              1: foo = 1
    next:2'0       {       search range start (exclusive)
              2: bar = 2
              3: baz = 3
    next:2'1     !~~       error: match on wrong line
    next:2'2             } search range end (exclusive)
    >>>>>>

`-dump-input=help` on the FileCheck command line explains the annotations.

The driver leaves the normalised input in the test's `.run` directory, for a
compile test `T7803.run/T7803.comp.stderr.normalised`, and the report next to
it with a `.filecheck` suffix. To iterate on the directives without
recompiling, run FileCheck by hand on that file with the driver's flags:

    FileCheck T7803.hs --input-file T7803.run/T7803.comp.stderr.normalised \
        --check-prefixes=CHECK,WS64 --allow-unused-prefixes --allow-empty \
        --dump-input=always

`--dump-input=always` shows the whole annotated input, useful when the first
`CHECK:` already fails and the five lines of context around it are not
enough.

### Converting a golden-file test

Pin what the test is about, not the whole dump. Read the original ticket or
the comment in the test for the property being checked, express it in as few
directives as it takes, and delete the golden file. The dumps still need to be
deterministic in the parts the directives touch, so keep or add
`-dsuppress-uniques` where the golden test had it. Replace a
`-ws-32`/`-ws-64` golden pair with `WS32:`/`WS64:` lines, and a
`check_errmsg`/`grep_errmsg` with `CHECK:`/`CHECK-NOT:`. Run the test on both
the current compiler and one that had the bug, if available, to see that the
directives fail for the right reason.
