# Contributing to Haddock

Thank you for contributing to Haddock! Here is the information you will need in
order to make your contribution

## Code of Conduct

We need you to read, acknowledge, and abide by our [Code of Conduct][CoC].

## Reporting issues

Please open a ticket if you get an unexpected behaviour from Haddock!  
You should ideally include a [Short, Self Contained, Correct (Compilable), Example][SSCCE]
in your ticket, so that the maintainers may easily reproduce your issue.

Here is a list of things you should include in your ticket

* Your GHC version.

* Your platform, OS and distribution if applicable.

* Your cabal version if applicable.

* Include any other info you think might be relevant (sandbox? unusual setup?).

## Hacking

To get started you'll need the latest GHC release installed.

Clone the repository:

```bash
git clone https://github.com/haskell/haddock.git
cd haddock
```

### Git Branches

If your patch consists of glue code and interface changes with GHC, please
open a Pull Request targeting the `ghc-head` branch.

Otherwise, for improvements to the documentation generator,
please base your pull request on the current GHC version branch
(`ghc-9.0` for instance). The PR will be forward-ported to `ghc-head`
so that documentation built within GHC can benefit from it.

### Building the packages

#### Using `cabal`

Requires cabal `>= 3.4` and GHC `== 9.4`:

You can install the latest build of GHC via ghcup using this command:

```bash
ghcup install ghc -u "https://gitlab.haskell.org/ghc/ghc/-/jobs/artifacts/master/raw/ghc-x86_64-deb9-linux-integer-simple.tar.xz?job=validate-x86_64-linux-deb9-integer-simple" head
```

```bash
cabal v2-build all --enable-tests
cabal v2-test all
```

### Updating golden testsuite outputs

If you've changed Haddock's output, you will probably need to accept the new
output of Haddock's golden test suites (`html-test`, `latex-test`,
`hoogle-test`, and `hypsrc-test`). This can be done by passing the `--accept`
argument to these test suites. With a new enough version of `cabal-install`:

```
cabal v2-test html-test latex-test hoogle-test hypsrc-test \
  --test-option='--accept'
```


[SSCCE]: http://sscce.org/
[CoC]: ./CODE_OF_CONDUCT.md
