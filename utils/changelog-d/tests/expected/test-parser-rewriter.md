## ?.?.? *TBA*

* Self-test fixture exercising the parser/rewriter. Uses double-backtick `code`,
  RST hyperlinks [the changelog wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/contributing/changelog),
  GHC-flavoured roles [#12345](https://gitlab.haskell.org/ghc/ghc/issues/12345), [!6789](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6789), [commentary/compiler](https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler),
  [CLC proposal #123](https://github.com/haskell/core-libraries-committee/issues/123), `-fxxx`, `TypeApplications`, `:type`,
  `-N`, haddock cross-refs `Data.Maybe.fromMaybe`,
  `Language.Haskell.TH.Lib`, `Distribution.Simple`,
  `GHC.Prim`, the internal-doc role, and an :unknown-role:`pass-through`. ([#26002](https://gitlab.haskell.org/ghc/ghc/issues/26002)) ([!15830](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/15830)) ([CLC proposal #0](https://github.com/haskell/core-libraries-committee/issues/0))

  This description block exercises block-level rewrites and inline rewrites
  inside a multi-line braced field.

  Inline forms inside the description: `inline code`, `DataKinds`,
  [!15830](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/15830), `Control.Applicative`, and a [bare RST link](https://example.invalid/).

  > **Note:**
  > This is an RST note admonition. It should render as a Markdown
  > blockquote prefixed with `> **Note:**`.
  > 
  > **Warning:**
  > This is an RST warning admonition. It should render as a Markdown
  > blockquote prefixed with `> **Warning:**`.
  > 
  ```haskell
  foo :: Int -> Int
  foo x = x + 1
  bar :: String
  bar = "hello"

  ```
  After the code block, plain prose continues. Verify that the renderer
  exits the fenced block correctly and resumes paragraph flow here.
