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

Pull requests are to be opened against the `main` branch, from which are forked
GHC-specific branches (like `ghc-9.2`, `ghc-9.4`, etc).

### Building the packages

#### Using `cabal`

First update the package list:

```bash
cabal v2-update
```

This is needed as haddock@ghc-9.2 uses the
[ghc.head](https://ghc.gitlab.haskell.org/head.hackage/) package repository.

```bash
cabal v2-build all --enable-tests
cabal v2-test all
```

### Updating golden test suite outputs

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
