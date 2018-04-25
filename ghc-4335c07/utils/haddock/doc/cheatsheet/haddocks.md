# Code Sections

```
  -- * Section
  -- ** Sub-section
  -- *** Sub-sub-section
  -- et cetera
```

# Named Documentation Chunks

```
  -- $name
[...]
  -- $name
  -- Here is the documentation text
  -- which is embedded elsewhere
```

# Code Blocks

```
With internal markup:
  -- @
  --   fact n = product [1..n]
  -- @
With literal text:
  -- > fact n = product [1..n]
```

# REPL Examples

```
  -- >>> fact 5
  -- 120
```

# Properties

```
  -- prop> a + b = b + a
```

# Hyperlinked Identifiers

```
  -- The value 'x' of type 'T'
  -- The out-of-scope 'MyModule.x'
  -- The "MyModule" module
```

# Textual Markup

```
  -- Emphasis: /forward slashes/.
  -- Bolding: __underscores__.
  -- Monospaced text: @ampersands@.
```

# Links and Images

```
  -- A raw link <http://example.com>
  -- [a link](http://example.com)
  -- ![description](imagepath.png)
```

# Lists

```
itemized with "*" or "-"
  -- * first item
  -- * second item
numbered with "(n)" or "n."
  -- 1. first item
  -- 2. second item
definitions with "[thing]"
  -- [one] first item
  -- [two] second item
```

# Mathematics/LaTeX

```
  -- \[
  --   f(n) = \Sum_{i=1}^{n} i
  -- \]
  \\ when \(n > 0\)
```

# Headings in Documentation

```
  -- = Heading
  -- == Sub-heading
  -- === Sub-sub-heading
```

# Metadata

```
  -- @since 1.2.3
```

# Module Attributes

```
{-# OPTIONS_HADDOCK hide #-}
  Omit this module from the docs
{-# OPTIONS_HADDOCK prune #-}
  Omit definitions without docs
{-# OPTIONS_HADDOCK ignore-exports #-}
  Treat this module as though all
  top-level items are exported
{-# OPTIONS_HADDOCK not-home #-}
  Do not treat this module as the
  "home" of identifiers it exports
{-# OPTIONS_HADDOCK show-extensions #-}
  Show all enabled LANGUAGE extensions
```
