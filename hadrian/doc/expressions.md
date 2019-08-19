# Expressions

`Expr c b a` is a computation that produces a value of type `Action a` and can
read parameters of the current build `Target c b`, but what does that mean
exactly? Here's its definition from `hadrian/src/Hadrian/Expression.hs`:

```haskell
newtype Expr c b a = Expr (ReaderT (Target c b) Action a)
    deriving (Applicative, Functor, Monad)
```

So `Expr c b a` is a `newtype` wrapper around a `ReaderT (Target c b) Action a`.
In practice within Hadrian `c` is always `Context` and `b` is always `Builder`.
The extra parameterisation is there so that hopefully one day the general
functionality of Hadrian (eg. compiling a Haskell library) will be available
to Shake users via a library.

A type synonym from `hadrian/src/Expression/Type.hs` is often used to avoid
writing `Context` and `Builder` everywhere:

```haskell
type Expr a = H.Expr Context Builder a
```

Where `H.Expr` is the `Expr c b a` defined above. The following references to
`Expr` will generally refer to this type synonym unless there is extra
parameterisation.

Let's break down the type a bit, working from the outside in, left to right.

## ReaderT

Put simply, `ReaderT (Target c b) Action a` adds a read-only environment
`Target c b` (in the case of Hadrian: `Target Context Builder`) to values of
type `Action a`. It's the equivalent of threading through a `Target c b`
parameter to all our functions, but we only have to worry about it when we need
it, using `ask :: Monad m => ReaderT r m r` (where `r` is `Target c b` and `m`
is `Action` in this case) or other functions based on it. `ReaderT` and `ask`
are defined in [`Control.Monad.Trans.Reader`](https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-Reader.html).

So, instead of:

```haskell
foo :: Target Context Builder -> Action ()
foo target = do
  liftIO $ putStrLn "Some message"
  bar target

bar :: Target Context Builder -> Action ()
bar target' = do
  liftIO $ putStrLn "Some other message"
  baz target'

baz :: Target Context Builder -> Action ()
baz target'' = do
  liftIO $ putStrLn "Yet another message"
  liftIO $ print target
```

We can write:

```haskell
foo :: ReaderT (Target Context Builder) Action ()
foo = do
  liftIO $ putStrLn "Some message"
  bar

bar :: ReaderT (Target Context Builder) Action ()
bar = do
  liftIO $ putStrLn "Some other message"
  baz

baz :: ReaderT (Target Context Builder) Action ()
baz = do
  liftIO $ putStrLn "Yet another message"
  target <- ask
  liftIO $ print target
```

And to make those into Hadrian Expressions all we have to do is change the type
and add the constructor:

```haskell
foo :: Expr ()
foo = Expr $ do
  liftIO $ putStrLn "Some message"
  bar

bar :: Expr ()
bar = Expr $ do
  liftIO $ putStrLn "Some other message"
  baz

baz :: Expr ()
baz = Expr $ do
  liftIO $ putStrLn "Yet another message"
  target <- ask
  liftIO $ print target
```

## Target

From `hadrian/src/Hadrian/Target.hs`:

> Each invocation of a builder is fully described by a `Target`, which
> comprises a build context (type variable `c`), a builder (type variable `b`),
> a list of input files and a list of output files. For example:
>
> ```haskell
> preludeTarget = Target (GHC.Context) (GHC.Builder)
>     { context = Context Stage1 base profiling
>     , builder = Ghc Stage1
>     , inputs = ["libraries/base/Prelude.hs"]
>     , outputs = ["build/stage1/libraries/base/Prelude.p_o"] }
> ```

The data type is as follows and is fairly self-explanatory:

```haskell
data Target c b = Target
    { context :: c          -- ^ Current build context
    , builder :: b          -- ^ Builder to be invoked
    , inputs  :: [FilePath] -- ^ Input files for the builder
    , outputs :: [FilePath] -- ^ Files to be produced
    } deriving (Eq, Generic, Show)
```

So we have some `inputs` to our target, some `outputs` that it will produce, a
context for the build (in Hadrian: `Context`), and the builder (in Hadrian:
`Builder`).

### Context

From `hadrian/src/Context/Type.hs`:

```haskell
data Context = Context
    { stage   :: Stage   -- ^ Currently build Stage
    , package :: Package -- ^ Currently build Package
    , way     :: Way     -- ^ Currently build Way (usually 'vanilla')
    } deriving (Eq, Generic, Show)
```

So Context is a data type that stores a Stage, Package, and a Way, i.e. the
context for some particular `Target`.

#### Stage

From `hadrian/src/Stage.hs`:

```haskell
data Stage = Stage0 | Stage1 | Stage2 | Stage3
    deriving (Show, Eq, Ord, Enum, Generic, Bounded)
```

#### Package

From `hadrian/src/Hadrian/Package.hs`:

```haskell
data Package = Package {
    -- | The package type. 'Library' and 'Program' packages are supported.
    pkgType :: PackageType,
    -- | The package name. We assume that all packages have different names,
    -- hence two packages with the same name are considered equal.
    pkgName :: PackageName,
    -- | The path to the package source code relative to the root of the build
    -- system. For example, @libraries/Cabal/Cabal@ and @ghc@ are paths to the
    -- @Cabal@ and @ghc-bin@ packages in GHC.
    pkgPath :: FilePath
    } deriving (Eq, Generic, Ord, Show)
```

`PackageType` is simply defined as:

```haskell
data PackageType = Library | Program deriving (Eq, Generic, Ord, Show)
```

This doesn't quite reflect how Cabal packages are actually structured, as
discussed in https://github.com/snowleopard/hadrian/issues/12, but Hadrian can
still function treating packages as either libraries or programs.

Both `PackageName` and `FilePath` are just type synonyms of `String`.

#### Way

From `hadrian/src/Way/Type.hs`:

```haskell
newtype Way = Way IntSet
```

Where `Way` is a set of enumerated `WayUnit`s wrapped in a `newtype`.

`WayUnit` is defined as:

```haskell
data WayUnit = Threaded
             | Debug
             | Profiling
             | Logging
             | Dynamic
             deriving (Bounded, Enum, Eq, Ord)
```

There are also some helper functions in this module to abstract away this
complexity. For example:

```haskell
import qualified Data.IntSet as Set

wayFromUnits :: [WayUnit] -> Way
wayFromUnits = Way . Set.fromList . map fromEnum
```

`wayFromUnits` converts the `[WayUnit]` into `[Int]` using `map fromEnum`,
creates an `IntSet` from them using `Set.fromList`, and then wraps the `IntSet`
with the `Way` constructor. So we can use `wayFromUnits` to create a `Way` that
builds Hadrian with both multi-threading and profiling by simply writing
`wayFromUnits [Threaded, Profiling]`.

We can also check if a `Way` contains a particular `WayUnit` by using
`wayUnit :: WayUnit -> Way -> Bool`. This is useful if we need to do something
when we're building with a particular `WayUnit`, but not otherwise.

For example, using `getWay :: Expr Context b Way` from `hadrian/src/Context.hs`:

```haskell
foo :: Expr ()
foo = do
  w <- getWay
  if wayUnit Profiling w
    then liftIO $ putStrLn "We're building this target with profiling"
    else liftIO $ putStrLn "We're not building this target with profiling"
```

### Builder

From `hadrian/src/Builder.hs`:

> A `Builder` is a (usually external) command invoked in a separate process
> via `cmd`. Here are some examples:
> * `Alex` is a lexical analyser generator that builds `Lexer.hs` from `Lexer.x`.
> * `Ghc` `Stage0` is the bootstrapping Haskell compiler used in `Stage0`.
> * `Ghc` `StageN` (N > 0) is the GHC built in stage (N - 1) and used in `StageN`.
>
> The `Cabal` builder is unusual in that it does not correspond to an external
> program but instead relies on the Cabal library for package configuration.

The data type itself is simply a long set of constructors that may or may not
be parameterised:

```haskell
data Builder = Alex
             | Ar ArMode Stage
             | Autoreconf FilePath
             | DeriveConstants
             | Cabal ConfigurationInfo Stage
             ...
             | Ghc GhcMode Stage
             ... etc.
             deriving (Eq, Generic, Show)
```

## Action

`Action` comes from Shake, the library underlying Hadrian. It can perform `IO`
using `liftIO` and keeps track of the dependencies for a rule. For more
information on `Action`, see the Shake docs:
https://hackage.haskell.org/package/shake-0.18.3/docs/Development-Shake.html

# Predicates

One useful kind of Hadrian expression is `Predicate`, which is just a type
synonym for `Expr Bool`. These expressions can read from the `Target` and
possibly perform `IO` or any other `Action` to return a `Bool`.

A particularly useful operator for using `Predicate`s is `?`. Its real type and
implementation can be found in `hadrian/src/Hadrian/Expression.hs`, but for the
sake of illustrating how it works in most cases, imagine it's defined like
this:

```haskell
(?) :: Monoid a => Predicate -> Expr a -> Expr a
predicate ? expr = do
  bool <- predicate
  if bool then expr else return mempty
```

If the `Predicate` returns `True`, we return the `Expr` we give it, otherwise
we return `mempty` (which is why we need the `Monoid` type constraint). In fact
thanks to some added type class complexity in the real definition, we can
give `?` a `Bool` instead of a `Predicate` and it works the same way.

To show how we might use `Predicate`s and `?` in practice, say we want to
compile all the Haskell modules in `compiler/` with `-O0` during stage 0. We can
do that by going to `UserSettings.hs` (see
[the user settings docs](user-settings.md)) and changing `userArgs` to:

```haskell
userArgs :: Args
userArgs = package compiler ? builder (Ghc CompileHs stage0) ? arg "-O0"
```

`Args` is just a type synonym for `Expr [String]` and `arg` just lifts a
`String` into an `Args`.

`package :: Package -> Predicate` from `hadrian/src/Expression.hs` takes a
`Package` and returns a `Predicate` that will return `True` if the current
`Target` is part of that package and `False` otherwise. In this case we give
it `compiler` which is defined in `hadrian/src/Packages.hs` along with many
other convenient `Package` definitions.

`builder` comes from `hadrian/src/Expression.hs`:

> This type class allows the user to construct both precise builder
> predicates, such as `builder (Ghc CompileHs Stage1)`, as well as predicates
> covering a set of similar builders. For example, `builder (Ghc CompileHs)`
> matches any stage, and `builder Ghc` matches any stage and any GHC mode.

```haskell
class BuilderPredicate a where
    -- | Is a particular builder being used?
    builder :: a -> Predicate
```

Other useful `Predicate` functions can be found in `hadrian/src/Expression.hs`
and `hadrian/src/Hadrian/Expression.hs`.
