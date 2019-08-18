# Hadrian Expressions

`Expr c b a` is a computation that produces a value of type `Action a` and can
read parameters of the current build `Target c b`, but what does that mean
exactly? Here's its definition:

```
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

```
type Expr a = H.Expr Context Builder a
```

Where `H.Expr` is the `Expr c b a` defined above. All following references to
`Expr` will refer to this type synonym.

Let's break down the type a bit, working from the outside in, left to right.

## ReaderT

Put simply, `ReaderT (Target c b) Action a` adds a read-only environment
`Target c b` (in the case of Hadrian: `Target Context Builder`) to values of
type `Action a`. It's the equivalent of threading through a `Target c b`
parameter to all our functions, but we only get have to worry about it when we
need it, using `ask :: ReaderT (Target c b) Action (Target c b)` or other
functions based on it.

So, instead of:

```
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
  liftIO $ putStrLn "Yet another message
  liftIO $ print target''
```

We can write:

```
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

```
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
> ```
> preludeTarget = Target (GHC.Context) (GHC.Builder)
>     { context = Context Stage1 base profiling
>     , builder = Ghc Stage1
>     , inputs = ["libraries/base/Prelude.hs"]
>     , outputs = ["build/stage1/libraries/base/Prelude.p_o"] }
> ```

The data type is as follows and is fairly self-explanatory:

```
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

```
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

```
data Stage = Stage0 | Stage1 | Stage2 | Stage3
    deriving (Show, Eq, Ord, Enum, Generic, Bounded)
```

#### Package

From `hadrian/src/Hadrian/Package.hs`:

```
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

```
data PackageType = Library | Program deriving (Eq, Generic, Ord, Show)
```

This doesn't quite reflect how Cabal packages are actually structured, as
discussed in https://github.com/snowleopard/hadrian/issues/12, but Hadrian
can still function treating packages as either libraries or programs.

Both `PackageName` and `FilePath` are just type synonyms of `String`.

#### Way

From `hadrian/src/Way/Type.hs`:

```
newtype Way = Way IntSet
```

Where `Way` is a set of enumerated `WayUnit`s wrapped in a `newtype`.

`WayUnit` is defined as:

```
data WayUnit = Threaded
             | Debug
             | Profiling
             | Logging
             | Dynamic
             deriving (Bounded, Enum, Eq, Ord)
```

There are also some helper functions in this module to abstract away this
complexity. For example:

```
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

```
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

```
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
