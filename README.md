# GHC branch with linear types

This is an *in-development* branch of GHC with support for linear
types. This branch is not supported by GHC HQ or anyone else. Please
don't submit tickets about issues in this branch to GHC Trac.

Building & Installing
=====================

There are two ways of running this locally:

1. By using a pre-built docker image, which integrates nicely
   with [Stack](https://www.haskellstack.org/). (**recommended** since
   building GHC takes quite a long time)
2. By cloning this repository and building locally from source (extra
   instructions and a good chunk of time required)

## Using stack with the Docker image

1. Setup Docker locally if you haven't got this already (see
   documentation for your distribution/Docker instructions).
2. Grab the Docker image from Docker Hub with `docker pull tweag/linear-types`.
3. In your new/existing stack project, edit the `stack.yaml` so to
   instruct stack to use a docker build of GHC:

```
[...]
resolver: ghc-8.2
compiler: ghc-8.2
system-ghc: true

docker:
  enable: true
  image: tweag/linear-types
[...]
```

The version numbers will depend on the specific version you want to
use, and what the relevant image actually builds.

4. You should now be able to build your project using the GHC from our
   Docker image simply by running `stack build`!


## Building from source

0. Verify that you have all the build dependencies:
  * autoconf
  * automake
  * ncurses
  * happy
  * alex
  * cabal-install
  * ghc >= 8.0.2

1. Clone the GHC repository mirror from GitHub:
  Note: cloning GHC from Github requires a special setup. See [Getting a GHC
  repository from Github][7].

  ```
  $ git clone https://github.com/ghc/ghc
  ```

2. Add this branch as a remote source:

  ```
  $ cd ghc
  $ git remote add tweag https://github.com/tweag/ghc.git
  $ git fetch tweag linear-types
  $ git checkout tweag/linear-types
  ```

3. Because of differing naming conventions in GHC and GitHub we need to give `git` some extra pointers:

  ```
  $ git config --global url."git://github.com/ghc/packages-".insteadOf     git://github.com/ghc/packages/
  $ git config --global url."http://github.com/ghc/packages-".insteadOf    http://github.com/ghc/packages/
  $ git config --global url."https://github.com/ghc/packages-".insteadOf   https://github.com/ghc/packages/
  $ git config --global url."ssh://git\@github.com/ghc/packages-".insteadOf ssh://git\@github.com/ghc/packages/
  $ git config --global url."git\@github.com:/ghc/packages-".insteadOf      git\@github.com:/ghc/packages/
  ```

4. Update and initialise all the submodules:

  ```
  $ git submodule update --init
  ```

5. Then setup the project for building:

  ```
  $ ./boot
  $ ./configure
  ```

6. Run the build with `make`. This will take between a few and several
   hours to complete depending on your machine. You probably want to
   utilise more than one core, which can be done by passing `-j N`
   (for maximal speed, try N = #physical cores + 1.

7. Now you can use `./inplace/bin/ghc-stage2` as your GHC, or add
   `--interactive` to use `ghci`.


## Issues & further building instructions

If you run into trouble building, please consult
the
[GHC Building Guide](https://ghc.haskell.org/trac/ghc/wiki/Building)
for possible answers.


Usage documentation
===================

The extention is provided as-is, but here is some basic documentation of how
to use it.


## Activating the extension
The extension is _always on_ for the time being, so no need to set any
flags or pragmas. In the future it will be an ordinary extension.


## Writing linear functions
Only the unicode lollipop arrow `⊸` is currently recognised. In the future there
will be an ASCII version, but no decision has been taken on notation.

When writing a function, you declare in the types which arguments are to be
checked for linearity. If you do not treat these as such in your
implementation, the type checker will nag on you:

```
λ> let frugal :: a ⊸ (a,a); frugal a = (a,a)

<interactive>:2:33: error:
    • Couldn't match expected weight ‘1’ of variable ‘a’ with actual weight ‘ω’
    • In an equation for ‘frugal’: frugal a = (a, a)


λ> let wasteful :: a ⊸ b ⊸ a; wasteful a b = a

<interactive>:3:39: error:
    • Couldn't match expected weight ‘1’ of variable ‘b’ with actual weight ‘0’
    • In an equation for ‘wasteful’: wasteful a b = a
```

As you can see, the type checker gives some hints on _how_ you treated your
variable wrongly, multiplicity `ω` means it was used more than once, or a
varying number in different code branches.

This `expected ‘1’ but actually ‘ω’`-error also occurs when the variables are
passed to unrestricted functions, this is not always very obvious so be
mindful of that possibility:

```
λ> let plus1 :: Int ⊸ Int; plus1 x = x + 1

<interactive>:12:31: error:
    • Couldn't match expected weight ‘1’ of variable ‘x’ with actual weight ‘ω’
    • In an equation for ‘plus1’: plus1 x = x + 1
```

This unfortunately applies to _all of prelude_ (`+` is the offender
here), which means you might see this a few times.


Currently the type checker does not infer linearity when not asked to do so,
hence it is a requirement to explicitly declare the types of linear functions.
Otherwise, they are assumed to be unrestricted which can give you errors from
using functions that are _implemented_ linearly, but not declared as such.
In particular, functions in `where`-clauses are easy to forget to annotate
with signatures (`-XPartialTypeSignatures` or `-XScopedTypeVariables` are
useful for this). Lambda expressions are linearly inferred using type
information from their context. Worth noting is that there is currently a
caveat here, see section on Bugs below.


## Calling them
By design, there is no obligations put on the caller of a linear function.
This means any linear function can take unrestricted arguments, which makes
all first-order ones "linearly polymorphic". However, passing a _linear_
variable as an _unrestricted_ argument is illegal and will give you an error,
as shown above. The key takeaway here is that a linear function can also act
as an unrestricted one; just pass it unrestricted arguments.


## Unimplemented features & known limitations
There are some parts of Haskell that are not yet implemented linearly, meaning
you cannot use them with linear variables.

### `case` and `let`
The probably most noteworthy things are `case` and `let`:

```
λ> let f :: a ⊸ a; f x = case x of x -> x

<interactive>:27:19: error:
    • Couldn't match expected weight ‘1’ of variable ‘x’ with actual weight ‘ω’
    • In an equation for ‘f’: f x = case x of { x -> x }


λ> let f :: a ⊸ a; f x = let y = x in y

<interactive>:28:19: error:
    • Couldn't match expected weight ‘1’ of variable ‘x’ with actual weight ‘ω’
    • In an equation for ‘f’: f x = let y = x in y
```

### Records
Record syntax itself works fine, but please note that type constructors are by
default linear; mixed-linearity records could be possible but would need a new and
probably complicated syntax. This does however affect record _projections_, which
must be considered unrestricted, because in general they are. Consider something
like

```
data P a b = P { p1 :: a, p2 :: b }
p1 :: P a b -> a
p2 :: P a b -> b
```

Here both the `a` and `b` are linear, but the corresponding projections are
not, because they discard the other field. However, your can always work around
this by writing your own, linear, projections like so:

```
unP :: P a b ⊸ (a, b)
```

It may in the future be possible to special-case single field records,
which could be useful for convenient `newtype`s.


### Pattern synonyms
Pattern synonyms are also not compatible with linear types
(and probably incorrect). These are the current _known_ limitations; there may
be more but it has not been thoroughly tested yet.


## Bugs
There is currently one particular bug causing headaches, where linear and
non-linear arrows get equalised under certain conditions. It happens when you
pass a non-linear function where a linear function is expected, and the
linearity checker happily accepts this, making stuff like this look fine while
it really is not:

```
λ> let skip :: a ⊸ (); skip = const ()
```

This also applies to lambda expressions under certain conditions.


`@`-patterns also do not work as expected, as this passes just fine while
being incorrect:

```
λ> let dup :: a ⊸ (a,a); dup y@x = (y,x)
```

There are probably more bugs; if you find something that definitely looks off,
we would appreciate a well-documented bug report to
[the issue tracker](https://github.com/tweag/ghc/issues) of this repository.
