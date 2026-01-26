{-# OPTIONS_HADDOCK not-home #-} -- we want users to import Language.Haskell.TH.Syntax instead
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs#-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedSums #-}

-- | This module is used internally in GHC's integration with Template Haskell
-- and defines the Monads of Template Haskell, and associated definitions.
--
-- This is not a part of the public API, and as such, there are no API
-- guarantees for this module from version to version.
--
-- Import "Language.Haskell.TH" or "Language.Haskell.TH.Syntax" instead!
module GHC.Internal.TH.Monad
    ( module GHC.Internal.TH.Monad
    ) where

#ifdef BOOTSTRAP_TH
import Prelude
import Data.Data hiding (Fixity(..))
import Data.IORef
import System.IO.Unsafe ( unsafePerformIO )
import Control.Monad.IO.Class (MonadIO (..))
import System.IO        ( hPutStrLn, stderr )
import qualified Data.Kind as Kind (Type)
import GHC.Types        (TYPE, RuntimeRep(..))
#else
import GHC.Internal.Base hiding (NonEmpty(..),Type, Module, sequence)
import GHC.Internal.Data.Data hiding (Fixity(..))
import GHC.Internal.Data.Traversable
import GHC.Internal.IORef
import GHC.Internal.System.IO
import GHC.Internal.Data.Foldable
import GHC.Internal.Data.Typeable
import GHC.Internal.Control.Monad.IO.Class
import GHC.Internal.Control.Monad.Fail
import GHC.Internal.Num
import GHC.Internal.IO.Unsafe
import qualified GHC.Internal.Types as Kind (Type)
#endif
import GHC.Internal.ForeignSrcLang
import GHC.Internal.LanguageExtensions
import GHC.Internal.TH.Syntax

-----------------------------------------------------
--
--              The Quasi class
--
-----------------------------------------------------

class (MonadIO m, MonadFail m) => Quasi m where
  -- | Fresh names. See 'newName'.
  qNewName :: String -> m Name

  ------- Error reporting and recovery -------
  -- | Report an error (True) or warning (False)
  -- ...but carry on; use 'fail' to stop. See 'report'.
  qReport  :: Bool -> String -> m ()

  -- | See 'recover'.
  qRecover :: m a -- ^ the error handler
           -> m a -- ^ action which may fail
           -> m a -- ^ Recover from the monadic 'fail'

  ------- Inspect the type-checker's environment -------
  -- | True <=> type namespace, False <=> value namespace. See 'lookupName'.
  qLookupName :: Bool -> String -> m (Maybe Name)
  -- | See 'reify'.
  qReify          :: Name -> m Info
  -- | See 'reifyFixity'.
  qReifyFixity    :: Name -> m (Maybe Fixity)
  -- | See 'reifyType'.
  qReifyType      :: Name -> m Type
  -- | Is (n tys) an instance? Returns list of matching instance Decs (with
  -- empty sub-Decs) Works for classes and type functions. See 'reifyInstances'.
  qReifyInstances :: Name -> [Type] -> m [Dec]
  -- | See 'reifyRoles'.
  qReifyRoles         :: Name -> m [Role]
  -- | See 'reifyAnnotations'.
  qReifyAnnotations   :: Data a => AnnLookup -> m [a]
  -- | See 'reifyModule'.
  qReifyModule        :: Module -> m ModuleInfo
  -- | See 'reifyConStrictness'.
  qReifyConStrictness :: Name -> m [DecidedStrictness]

  -- | See 'location'.
  qLocation :: m Loc

  -- | Input/output (dangerous). See 'runIO'.
  qRunIO :: IO a -> m a
  qRunIO = liftIO
  -- | See 'getPackageRoot'.
  qGetPackageRoot :: m FilePath

  -- | See 'addDependentFile'.
  qAddDependentFile :: FilePath -> m ()

  -- | See 'addDependentDirectory'.
  qAddDependentDirectory :: FilePath -> m ()

  -- | See 'addTempFile'.
  qAddTempFile :: String -> m FilePath

  -- | See 'addTopDecls'.
  qAddTopDecls :: [Dec] -> m ()

  -- | See 'addForeignFilePath'.
  qAddForeignFilePath :: ForeignSrcLang -> String -> m ()

  -- | See 'addModFinalizer'.
  qAddModFinalizer :: Q () -> m ()

  -- | See 'addCorePlugin'.
  qAddCorePlugin :: String -> m ()

  -- | See 'getQ'.
  qGetQ :: Typeable a => m (Maybe a)

  -- | See 'putQ'.
  qPutQ :: Typeable a => a -> m ()

  -- | See 'isExtEnabled'.
  qIsExtEnabled :: Extension -> m Bool
  -- | See 'extsEnabled'.
  qExtsEnabled :: m [Extension]

  -- | See 'putDoc'.
  qPutDoc :: DocLoc -> String -> m ()
  -- | See 'getDoc'.
  qGetDoc :: DocLoc -> m (Maybe String)

-----------------------------------------------------
--      The IO instance of Quasi
-----------------------------------------------------

--  | This instance is used only when running a Q
--  computation in the IO monad, usually just to
--  print the result.  There is no interesting
--  type environment, so reification isn't going to
--  work.
instance Quasi IO where
  qNewName = newNameIO

  qReport True  msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)
  qReport False msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)

  qLookupName _ _       = badIO "lookupName"
  qReify _              = badIO "reify"
  qReifyFixity _        = badIO "reifyFixity"
  qReifyType _          = badIO "reifyFixity"
  qReifyInstances _ _   = badIO "reifyInstances"
  qReifyRoles _         = badIO "reifyRoles"
  qReifyAnnotations _   = badIO "reifyAnnotations"
  qReifyModule _        = badIO "reifyModule"
  qReifyConStrictness _ = badIO "reifyConStrictness"
  qLocation             = badIO "currentLocation"
  qRecover _ _          = badIO "recover" -- Maybe we could fix this?
  qGetPackageRoot       = badIO "getProjectRoot"
  qAddDependentFile _   = badIO "addDependentFile"
  qAddTempFile _        = badIO "addTempFile"
  qAddTopDecls _        = badIO "addTopDecls"
  qAddForeignFilePath _ _ = badIO "addForeignFilePath"
  qAddModFinalizer _    = badIO "addModFinalizer"
  qAddCorePlugin _      = badIO "addCorePlugin"
  qGetQ                 = badIO "getQ"
  qPutQ _               = badIO "putQ"
  qIsExtEnabled _       = badIO "isExtEnabled"
  qExtsEnabled          = badIO "extsEnabled"
  qPutDoc _ _           = badIO "putDoc"
  qGetDoc _             = badIO "getDoc"
  qAddDependentDirectory _ = badIO "AddDependentDirectory"

instance Quote IO where
  newName = newNameIO

newNameIO :: String -> IO Name
newNameIO s = do { n <- atomicModifyIORef' counter (\x -> (x + 1, x))
                 ; pure (mkNameU s n) }

badIO :: String -> IO a
badIO op = do   { qReport True ("Can't do `" ++ op ++ "' in the IO monad")
                ; fail "Template Haskell failure" }

-- Global variable to generate unique symbols
counter :: IORef Uniq
{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0)


-----------------------------------------------------
--
--              The Q monad
--
-----------------------------------------------------

-- | In short, 'Q' provides the 'Quasi' operations in one neat monad for the
-- user.
--
-- The longer story, is that 'Q' wraps an arbitrary 'Quasi'-able monad.
-- The perceptive reader notices that 'Quasi' has only two instances, 'Q'
-- itself and 'IO', neither of which have concrete implementations.'Q' plays
-- the trick of [dependency
-- inversion](https://en.wikipedia.org/wiki/Dependency_inversion_principle),
-- providing an abstract interface for the user which is later concretely
-- fufilled by an concrete 'Quasi' instance, internal to GHC.
newtype Q a = Q { unQ :: forall m. Quasi m => m a }

-- | \"Runs\" the 'Q' monad. Normal users of Template Haskell
-- should not need this function, as the splice brackets @$( ... )@
-- are the usual way of running a 'Q' computation.
--
-- This function is primarily used in GHC internals, and for debugging
-- splices by running them in 'IO'.
--
-- Note that many functions in 'Q', such as 'reify' and other compiler
-- queries, are not supported when running 'Q' in 'IO'; these operations
-- simply fail at runtime. Indeed, the only operations guaranteed to succeed
-- are 'newName', 'runIO', 'reportError' and 'reportWarning'.
runQ :: Quasi m => Q a -> m a
runQ (Q m) = m

instance Monad Q where
  Q m >>= k  = Q (m >>= \x -> unQ (k x))
  (>>) = (*>)

instance MonadFail Q where
  fail s     = report True s >> Q (fail "Q monad failure")

instance Functor Q where
  fmap f (Q x) = Q (fmap f x)

instance Applicative Q where
  pure x = Q (pure x)
  Q f <*> Q x = Q (f <*> x)
  Q m *> Q n = Q (m *> n)

-- | @since 2.17.0.0
instance Semigroup a => Semigroup (Q a) where
  (<>) = liftA2 (<>)

-- | @since 2.17.0.0
instance Monoid a => Monoid (Q a) where
  mempty = pure mempty


-----------------------------------------------------
--
--              The Quote class
--
-----------------------------------------------------



-- | The 'Quote' class implements the minimal interface which is necessary for
-- desugaring quotations.
--
-- * The @Monad m@ superclass is needed to stitch together the different
-- AST fragments.
-- * 'newName' is used when desugaring binding structures such as lambdas
-- to generate fresh names.
--
-- Therefore the type of an untyped quotation in GHC is `Quote m => m Exp`
--
-- For many years the type of a quotation was fixed to be `Q Exp` but by
-- more precisely specifying the minimal interface it enables the `Exp` to
-- be extracted purely from the quotation without interacting with `Q`.
class Monad m => Quote m where
  {- |
  Generate a fresh name, which cannot be captured.

  For example, this:

  @f = $(do
    nm1 <- newName \"x\"
    let nm2 = 'mkName' \"x\"
    return ('LamE' ['VarP' nm1] (LamE [VarP nm2] ('VarE' nm1)))
   )@

  will produce the splice

  >f = \x0 -> \x -> x0

  In particular, the occurrence @VarE nm1@ refers to the binding @VarP nm1@,
  and is not captured by the binding @VarP nm2@.

  Although names generated by @newName@ cannot /be captured/, they can
  /capture/ other names. For example, this:

  >g = $(do
  >  nm1 <- newName "x"
  >  let nm2 = mkName "x"
  >  return (LamE [VarP nm2] (LamE [VarP nm1] (VarE nm2)))
  > )

  will produce the splice

  >g = \x -> \x0 -> x0

  since the occurrence @VarE nm2@ is captured by the innermost binding
  of @x@, namely @VarP nm1@.
  -}
  newName :: String -> m Name

instance Quote Q where
  newName s = Q (qNewName s)

-----------------------------------------------------
--
--              The TExp type
--
-----------------------------------------------------

type TExp :: TYPE r -> Kind.Type
type role TExp nominal   -- See Note [Role of TExp]
newtype TExp a = TExp
  { unType :: Exp -- ^ Underlying untyped Template Haskell expression
  }
-- ^ Typed wrapper around an 'Exp'.
--
-- This is the typed representation of terms produced by typed quotes.
--
-- Representation-polymorphic since /template-haskell-2.16.0.0/.

-- | Discard the type annotation and produce a plain Template Haskell
-- expression
--
-- Representation-polymorphic since /template-haskell-2.16.0.0/.
unTypeQ :: forall (r :: RuntimeRep) (a :: TYPE r) m . Quote m => m (TExp a) -> m Exp
unTypeQ m = do { TExp e <- m
               ; return e }

-- | Annotate the Template Haskell expression with a type
--
-- This is unsafe because GHC cannot check for you that the expression
-- really does have the type you claim it has.
--
-- Representation-polymorphic since /template-haskell-2.16.0.0/.
unsafeTExpCoerce :: forall (r :: RuntimeRep) (a :: TYPE r) m .
                      Quote m => m Exp -> m (TExp a)
unsafeTExpCoerce m = do { e <- m
                        ; return (TExp e) }

{- Note [Role of TExp]
~~~~~~~~~~~~~~~~~~~~~~
TExp's argument must have a nominal role, not phantom as would
be inferred (#8459).  Consider

  e :: Code Q Age
  e = [|| MkAge 3 ||]

  foo = $(coerce e) + 4::Int

The splice will evaluate to (MkAge 3) and you can't add that to
4::Int. So you can't coerce a (Code Q Age) to a (Code Q Int). -}

-- Code constructor
#if __GLASGOW_HASKELL__ >= 909
type Code :: (Kind.Type -> Kind.Type) -> forall r. TYPE r -> Kind.Type
  -- See Note [Foralls to the right in Code]
#else
type Code :: (Kind.Type -> Kind.Type) -> TYPE r -> Kind.Type
#endif
type role Code representational nominal   -- See Note [Role of TExp]
newtype Code m a = Code
  { examineCode :: m (TExp a) -- ^ Underlying monadic value
  }
-- ^ Represents an expression which has type @a@, built in monadic context @m@. Built on top of 'TExp', typed
-- expressions allow for type-safe splicing via:
--
--   - typed quotes, written as @[|| ... ||]@ where @...@ is an expression; if
--     that expression has type @a@, then the quotation has type
--     @Quote m => Code m a@
--
--   - typed splices inside of typed quotes, written as @$$(...)@ where @...@
--     is an arbitrary expression of type @Quote m => Code m a@
--
-- Traditional expression quotes and splices let us construct ill-typed
-- expressions:
--
-- >>> fmap ppr $ runQ (unTypeCode [| True == $( [| "foo" |] ) |])
-- GHC.Internal.Types.True GHC.Internal.Classes.== "foo"
-- >>> GHC.Internal.Types.True GHC.Internal.Classes.== "foo"
-- <interactive> error:
--     • Couldn't match expected type ‘Bool’ with actual type ‘[Char]’
--     • In the second argument of ‘(==)’, namely ‘"foo"’
--       In the expression: True == "foo"
--       In an equation for ‘it’: it = True == "foo"
--
-- With typed expressions, the type error occurs when /constructing/ the
-- Template Haskell expression:
--
-- >>> fmap ppr $ runQ (unTypeCode [|| True == $$( [|| "foo" ||] ) ||])
-- <interactive> error:
--     • Couldn't match type ‘[Char]’ with ‘Bool’
--       Expected type: Code Q Bool
--         Actual type: Code Q [Char]
--     • In the Template Haskell quotation [|| "foo" ||]
--       In the expression: [|| "foo" ||]
--       In the Template Haskell splice $$([|| "foo" ||])


{- Note [Foralls to the right in Code]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Code has the following type signature:
   type Code :: (Kind.Type -> Kind.Type) -> forall r. TYPE r -> Kind.Type

This allows us to write
   data T (f :: forall r . (TYPE r) -> Type) = MkT (f Int) (f Int#)

   tcodeq :: T (Code Q)
   tcodeq = MkT [||5||] [||5#||]

If we used the slightly more straightforward signature
   type Code :: foral r. (Kind.Type -> Kind.Type) -> TYPE r -> Kind.Type

then the example above would become ill-typed.  (See #23592 for some discussion.)
-}

-- | Unsafely convert an untyped code representation into a typed code
-- representation.
unsafeCodeCoerce :: forall (r :: RuntimeRep) (a :: TYPE r) m .
                      Quote m => m Exp -> Code m a
unsafeCodeCoerce m = Code (unsafeTExpCoerce m)

-- | Lift a monadic action producing code into the typed 'Code'
-- representation
liftCode :: forall (r :: RuntimeRep) (a :: TYPE r) m . m (TExp a) -> Code m a
liftCode = Code

-- | Extract the untyped representation from the typed representation
unTypeCode :: forall (r :: RuntimeRep) (a :: TYPE r) m . Quote m
           => Code m a -> m Exp
unTypeCode = unTypeQ . examineCode

-- | Modify the ambient monad used during code generation. For example, you
-- can use `hoistCode` to handle a state effect:
-- @
--  handleState :: Code (StateT Int Q) a -> Code Q a
--  handleState = hoistCode (flip runState 0)
-- @
hoistCode :: forall m n (r :: RuntimeRep) (a :: TYPE r) . Monad m
          => (forall x . m x -> n x) -> Code m a -> Code n a
hoistCode f (Code a) = Code (f a)


-- | Variant of '(>>=)' which allows effectful computations to be injected
-- into code generation.
bindCode :: forall m a (r :: RuntimeRep) (b :: TYPE r) . Monad m
         => m a -> (a -> Code m b) -> Code m b
bindCode q k = liftCode (q >>= examineCode . k)

-- | Variant of '(>>)' which allows effectful computations to be injected
-- into code generation.
bindCode_ :: forall m a (r :: RuntimeRep) (b :: TYPE r) . Monad m
          => m a -> Code m b -> Code m b
bindCode_ q c = liftCode ( q >> examineCode c)

-- | A useful combinator for embedding monadic actions into 'Code'
-- @
-- myCode :: ... => Code m a
-- myCode = joinCode $ do
--   x <- someSideEffect
--   return (makeCodeWith x)
-- @
joinCode :: forall m (r :: RuntimeRep) (a :: TYPE r) . Monad m
         => m (Code m a) -> Code m a
joinCode = flip bindCode id

----------------------------------------------------
-- Packaged versions for the programmer, hiding the Quasi-ness


-- | Report an error (True) or warning (False),
-- but carry on; use 'fail' to stop.
report  :: Bool -> String -> Q ()
report b s = Q (qReport b s)
{-# DEPRECATED report "Use reportError or reportWarning instead" #-} -- deprecated in 7.6

-- | Report an error to the user, but allow the current splice's computation to carry on. To abort the computation, use 'fail'.
reportError :: String -> Q ()
reportError = report True

-- | Report a warning to the user, and carry on.
reportWarning :: String -> Q ()
reportWarning = report False

-- | Recover from errors raised by 'reportError' or 'fail'.
recover :: Q a -- ^ handler to invoke on failure
        -> Q a -- ^ computation to run
        -> Q a
recover (Q r) (Q m) = Q (qRecover r m)

-- We don't export lookupName; the Bool isn't a great API
-- Instead we export lookupTypeName, lookupValueName
lookupName :: Bool -> String -> Q (Maybe Name)
lookupName ns s = Q (qLookupName ns s)

-- | Look up the given name in the (type namespace of the) current splice's scope. See "Language.Haskell.TH.Syntax#namelookup" for more details.
lookupTypeName :: String -> Q (Maybe Name)
lookupTypeName  s = Q (qLookupName True s)

-- | Look up the given name in the (value namespace of the) current splice's scope. See "Language.Haskell.TH.Syntax#namelookup" for more details.
lookupValueName :: String -> Q (Maybe Name)
lookupValueName s = Q (qLookupName False s)

{-
Note [Name lookup]
~~~~~~~~~~~~~~~~~~
-}
{- $namelookup #namelookup#
The functions 'lookupTypeName' and 'lookupValueName' provide
a way to query the current splice's context for what names
are in scope. The function 'lookupTypeName' queries the type
namespace, whereas 'lookupValueName' queries the value namespace,
but the functions are otherwise identical.

A call @lookupValueName s@ will check if there is a value
with name @s@ in scope at the current splice's location. If
there is, the @Name@ of this value is returned;
if not, then @Nothing@ is returned.

The returned name cannot be \"captured\".
For example:

> f = "global"
> g = $( do
>          Just nm <- lookupValueName "f"
>          [| let f = "local" in $( varE nm ) |]

In this case, @g = \"global\"@; the call to @lookupValueName@
returned the global @f@, and this name was /not/ captured by
the local definition of @f@.

The lookup is performed in the context of the /top-level/ splice
being run. For example:

> f = "global"
> g = $( [| let f = "local" in
>            $(do
>                Just nm <- lookupValueName "f"
>                varE nm
>             ) |] )

Again in this example, @g = \"global\"@, because the call to
@lookupValueName@ queries the context of the outer-most @$(...)@.

Operators should be queried without any surrounding parentheses, like so:

> lookupValueName "+"

Qualified names are also supported, like so:

> lookupValueName "Prelude.+"
> lookupValueName "Prelude.map"

-}


{- | 'reify' looks up information about the 'Name'. It will fail with
a compile error if the 'Name' is not visible. A 'Name' is visible if it is
imported or defined in a prior top-level declaration group. See the
documentation for 'newDeclarationGroup' for more details.

It is sometimes useful to construct the argument name using 'lookupTypeName' or 'lookupValueName'
to ensure that we are reifying from the right namespace. For instance, in this context:

> data D = D

which @D@ does @reify (mkName \"D\")@ return information about? (Answer: @D@-the-type, but don't rely on it.)
To ensure we get information about @D@-the-value, use 'lookupValueName':

> do
>   Just nm <- lookupValueName "D"
>   reify nm

and to get information about @D@-the-type, use 'lookupTypeName'.
-}
reify :: Name -> Q Info
reify v = Q (qReify v)

{- | @reifyFixity nm@ attempts to find a fixity declaration for @nm@. For
example, if the function @foo@ has the fixity declaration @infixr 7 foo@, then
@reifyFixity 'foo@ would return @'Just' ('Fixity' 7 'InfixR')@. If the function
@bar@ does not have a fixity declaration, then @reifyFixity 'bar@ returns
'Nothing', so you may assume @bar@ has 'defaultFixity'.
-}
reifyFixity :: Name -> Q (Maybe Fixity)
reifyFixity nm = Q (qReifyFixity nm)

{- | @reifyType nm@ attempts to find the type or kind of @nm@. For example,
@reifyType 'not@   returns @Bool -> Bool@, and
@reifyType ''Bool@ returns @Type@.
This works even if there's no explicit signature and the type or kind is inferred.
-}
reifyType :: Name -> Q Type
reifyType nm = Q (qReifyType nm)

{- | Template Haskell is capable of reifying information about types and
terms defined in previous declaration groups. Top-level declaration splices break up
declaration groups.

For an example, consider this  code block. We define a datatype @X@ and
then try to call 'reify' on the datatype.

@
module Check where

data X = X
    deriving Eq

$(do
    info <- reify ''X
    runIO $ print info
 )
@

This code fails to compile, noting that @X@ is not available for reification at the site of 'reify'. We can fix this by creating a new declaration group using an empty top-level splice:

@
data X = X
    deriving Eq

$(pure [])

$(do
    info <- reify ''X
    runIO $ print info
 )
@

We provide 'newDeclarationGroup' as a means of documenting this behavior
and providing a name for the pattern.

Since top level splices infer the presence of the @$( ... )@ brackets, we can also write:

@
data X = X
    deriving Eq

newDeclarationGroup

$(do
    info <- reify ''X
    runIO $ print info
 )
@

-}
newDeclarationGroup :: Q [Dec]
newDeclarationGroup = pure []

{- | @reifyInstances nm tys@ returns a list of all visible instances (see below for "visible")
of @nm tys@. That is,
if @nm@ is the name of a type class, then all instances of this class at the types @tys@
are returned. Alternatively, if @nm@ is the name of a data family or type family,
all instances of this family at the types @tys@ are returned.

Note that this is a \"shallow\" test; the declarations returned merely have
instance heads which unify with @nm tys@, they need not actually be satisfiable.

  - @reifyInstances ''Eq [ 'TupleT' 2 \``AppT`\` 'ConT' ''A \``AppT`\` 'ConT' ''B ]@ contains
    the @instance (Eq a, Eq b) => Eq (a, b)@ regardless of whether @A@ and
    @B@ themselves implement 'Eq'

  - @reifyInstances ''Show [ 'VarT' ('mkName' "a") ]@ produces every available
    instance of 'Show'

There is one edge case: @reifyInstances ''Typeable tys@ currently always
produces an empty list (no matter what @tys@ are given).

In principle, the *visible* instances are
* all instances defined in a prior top-level declaration group
  (see docs on @newDeclarationGroup@), or
* all instances defined in any module transitively imported by the
  module being compiled

However, actually searching all modules transitively below the one being
compiled is unreasonably expensive, so @reifyInstances@ will report only the
instance for modules that GHC has had some cause to visit during this
compilation.  This is a shortcoming: @reifyInstances@ might fail to report
instances for a type that is otherwise unusued, or instances defined in a
different component.  You can work around this shortcoming by explicitly importing the modules
whose instances you want to be visible. GHC issue <https://gitlab.haskell.org/ghc/ghc/-/issues/20529#note_388980 #20529>
has some discussion around this.

-}
reifyInstances :: Name -> [Type] -> Q [InstanceDec]
reifyInstances cls tys = Q (qReifyInstances cls tys)

{- | @reifyRoles nm@ returns the list of roles associated with the parameters
(both visible and invisible) of
the tycon @nm@. Fails if @nm@ cannot be found or is not a tycon.
The returned list should never contain 'InferR'.

An invisible parameter to a tycon is often a kind parameter. For example, if
we have

@
type Proxy :: forall k. k -> Type
data Proxy a = MkProxy
@

and @reifyRoles Proxy@, we will get @['NominalR', 'PhantomR']@. The 'NominalR' is
the role of the invisible @k@ parameter. Kind parameters are always nominal.
-}
reifyRoles :: Name -> Q [Role]
reifyRoles nm = Q (qReifyRoles nm)

-- | @reifyAnnotations target@ returns the list of annotations
-- associated with @target@.  Only the annotations that are
-- appropriately typed is returned.  So if you have @Int@ and @String@
-- annotations for the same target, you have to call this function twice.
reifyAnnotations :: Data a => AnnLookup -> Q [a]
reifyAnnotations an = Q (qReifyAnnotations an)

-- | @reifyModule mod@ looks up information about module @mod@.  To
-- look up the current module, call this function with the return
-- value of 'Language.Haskell.TH.Lib.thisModule'.
reifyModule :: Module -> Q ModuleInfo
reifyModule m = Q (qReifyModule m)

-- | @reifyConStrictness nm@ looks up the strictness information for the fields
-- of the constructor with the name @nm@. Note that the strictness information
-- that 'reifyConStrictness' returns may not correspond to what is written in
-- the source code. For example, in the following data declaration:
--
-- @
-- data Pair a = Pair a a
-- @
--
-- 'reifyConStrictness' would return @['DecidedLazy', DecidedLazy]@ under most
-- circumstances, but it would return @['DecidedStrict', DecidedStrict]@ if the
-- @-XStrictData@ language extension was enabled.
reifyConStrictness :: Name -> Q [DecidedStrictness]
reifyConStrictness n = Q (qReifyConStrictness n)

-- | Is the list of instances returned by 'reifyInstances' nonempty?
--
-- If you're confused by an instance not being visible despite being
-- defined in the same module and above the splice in question, see the
-- docs for 'newDeclarationGroup' for a possible explanation.
isInstance :: Name -> [Type] -> Q Bool
isInstance nm tys = do { decs <- reifyInstances nm tys
                       ; return (not (null decs)) }

-- | The location at which this computation is spliced.
location :: Q Loc
location = Q qLocation

-- |The 'runIO' function lets you run an I\/O computation in the 'Q' monad.
-- Take care: you are guaranteed the ordering of calls to 'runIO' within
-- a single 'Q' computation, but not about the order in which splices are run.
--
-- Note: for various murky reasons, stdout and stderr handles are not
-- necessarily flushed when the compiler finishes running, so you should
-- flush them yourself.
runIO :: IO a -> Q a
runIO m = Q (qRunIO m)

-- | Get the package root for the current package which is being compiled.
-- This can be set explicitly with the -package-root flag but is normally
-- just the current working directory.
--
-- The motivation for this flag is to provide a principled means to remove the
-- assumption from splices that they will be executed in the directory where the
-- cabal file resides. Projects such as haskell-language-server can't and don't
-- change directory when compiling files but instead set the -package-root flag
-- appropriately.
getPackageRoot :: Q FilePath
getPackageRoot = Q qGetPackageRoot

-- | Record external directories that runIO is using (dependent upon).
-- The compiler can then recognize that it should re-compile the Haskell file
-- when a directory changes.
--
-- Notes:
--
--   * ghc -M does not know about these dependencies - it does not execute TH.
--
--   * The dependency is shallow, based only on the direct content.
--     Basically, it only sees a list of names. It does not look at directory
--     metadata, recurse into subdirectories, or look at file contents. As
--     long as the list of names remains the same, the directory is considered
--     unchanged.
--
--   * The state of the directory is read at the interface generation time,
--     not at the time of the function call.
addDependentDirectory :: FilePath -> Q ()
addDependentDirectory dp = Q (qAddDependentDirectory dp)

-- | Record external files that runIO is using (dependent upon).
-- The compiler can then recognize that it should re-compile the Haskell file
-- when an external file changes.
--
-- Expects an absolute file path.
--
-- Notes:
--
--   * ghc -M does not know about these dependencies - it does not execute TH.
--
--   * The dependency is based on file content, not a modification time
addDependentFile :: FilePath -> Q ()
addDependentFile fp = Q (qAddDependentFile fp)

-- | Obtain a temporary file path with the given suffix. The compiler will
-- delete this file after compilation.
addTempFile :: String -> Q FilePath
addTempFile suffix = Q (qAddTempFile suffix)

-- | Add additional top-level declarations. The added declarations will be type
-- checked along with the current declaration group.
addTopDecls :: [Dec] -> Q ()
addTopDecls ds = Q (qAddTopDecls ds)


-- | Emit a foreign file which will be compiled and linked to the object for
-- the current module. Currently only languages that can be compiled with
-- the C compiler are supported, and the flags passed as part of -optc will
-- be also applied to the C compiler invocation that will compile them.
--
-- Note that for non-C languages (for example C++) @extern "C"@ directives
-- must be used to get symbols that we can access from Haskell.
--
-- To get better errors, it is recommended to use #line pragmas when
-- emitting C files, e.g.
--
-- > {-# LANGUAGE CPP #-}
-- > ...
-- > addForeignSource LangC $ unlines
-- >   [ "#line " ++ show (__LINE__ + 1) ++ " " ++ show __FILE__
-- >   , ...
-- >   ]
addForeignSource :: ForeignSrcLang -> String -> Q ()
addForeignSource lang src = do
  let suffix = case lang of
                 LangC      -> "c"
                 LangCxx    -> "cpp"
                 LangObjc   -> "m"
                 LangObjcxx -> "mm"
                 LangAsm    -> "s"
                 LangJs     -> "js"
                 RawObject  -> "a"
  path <- addTempFile suffix
  runIO $ writeFile path src
  addForeignFilePath lang path

-- | Same as 'addForeignSource', but expects to receive a path pointing to the
-- foreign file instead of a 'String' of its contents. Consider using this in
-- conjunction with 'addTempFile'.
--
-- This is a good alternative to 'addForeignSource' when you are trying to
-- directly link in an object file.
addForeignFilePath :: ForeignSrcLang -> FilePath -> Q ()
addForeignFilePath lang fp = Q (qAddForeignFilePath lang fp)

-- | Add a finalizer that will run in the Q monad after the current module has
-- been type checked. This only makes sense when run within a top-level splice.
--
-- The finalizer is given the local type environment at the splice point. Thus
-- 'reify' is able to find the local definitions when executed inside the
-- finalizer.
addModFinalizer :: Q () -> Q ()
addModFinalizer act = Q (qAddModFinalizer (unQ act))

-- | Adds a core plugin to the compilation pipeline.
--
-- @addCorePlugin m@ has almost the same effect as passing @-fplugin=m@ to ghc
-- in the command line. The major difference is that the plugin module @m@
-- must not belong to the current package. When TH executes, it is too late
-- to tell the compiler that we needed to compile first a plugin module in the
-- current package.
addCorePlugin :: String -> Q ()
addCorePlugin plugin = Q (qAddCorePlugin plugin)

-- | Get state from the 'Q' monad. The state maintained by 'Q' is isomorphic to
-- a type-indexed finite map. That is,
--
-- @
-- do putQ @Int 42
--    putQ @Char 'a'
--    getQ @Int      -- == (Just 42)
-- @
--
-- Note that the state is local to the Haskell module in which the Template
-- Haskell expression is executed.
getQ :: Typeable a => Q (Maybe a)
getQ = Q qGetQ

-- | Replace the state in the 'Q' monad. Note that the state is local to the
-- Haskell module in which the Template Haskell expression is executed.
putQ :: Typeable a => a -> Q ()
putQ x = Q (qPutQ x)

-- | Determine whether the given language extension is enabled in the 'Q' monad.
isExtEnabled :: Extension -> Q Bool
isExtEnabled ext = Q (qIsExtEnabled ext)

-- | List all enabled language extensions.
extsEnabled :: Q [Extension]
extsEnabled = Q qExtsEnabled

-- | Add Haddock documentation to the specified location. This will overwrite
-- any documentation at the location if it already exists. This will reify the
-- specified name, so it must be in scope when you call it. If you want to add
-- documentation to something that you are currently splicing, you can use
-- 'addModFinalizer' e.g.
--
-- > do
-- >   let nm = mkName "x"
-- >   addModFinalizer $ putDoc (DeclDoc nm) "Hello"
-- >   [d| $(varP nm) = 42 |]
--
-- The helper functions 'withDecDoc' and 'withDecsDoc' will do this for you, as
-- will the 'funD_doc' and other @_doc@ combinators.
-- You most likely want to have the @-haddock@ flag turned on when using this.
-- Adding documentation to anything outside of the current module will cause an
-- error.
putDoc :: DocLoc -> String -> Q ()
putDoc t s = Q (qPutDoc t s)

-- | Retrieves the Haddock documentation at the specified location, if one
-- exists.
-- It can be used to read documentation on things defined outside of the current
-- module, provided that those modules were compiled with the @-haddock@ flag.
getDoc :: DocLoc -> Q (Maybe String)
getDoc n = Q (qGetDoc n)

instance MonadIO Q where
  liftIO = runIO

instance Quasi Q where
  qNewName            = newName
  qReport             = report
  qRecover            = recover
  qReify              = reify
  qReifyFixity        = reifyFixity
  qReifyType          = reifyType
  qReifyInstances     = reifyInstances
  qReifyRoles         = reifyRoles
  qReifyAnnotations   = reifyAnnotations
  qReifyModule        = reifyModule
  qReifyConStrictness = reifyConStrictness
  qLookupName         = lookupName
  qLocation           = location
  qGetPackageRoot     = getPackageRoot
  qAddDependentFile   = addDependentFile
  qAddDependentDirectory = addDependentDirectory
  qAddTempFile        = addTempFile
  qAddTopDecls        = addTopDecls
  qAddForeignFilePath = addForeignFilePath
  qAddModFinalizer    = addModFinalizer
  qAddCorePlugin      = addCorePlugin
  qGetQ               = getQ
  qPutQ               = putQ
  qIsExtEnabled       = isExtEnabled
  qExtsEnabled        = extsEnabled
  qPutDoc             = putDoc
  qGetDoc             = getDoc


----------------------------------------------------
-- The following operations are used solely in GHC.HsToCore.Quote when
-- desugaring brackets. They are not necessary for the user, who can use
-- ordinary return and (>>=) etc

-- | This function is only used in 'GHC.HsToCore.Quote' when desugaring
-- brackets. This is not necessary for the user, who can use the ordinary
-- 'return' and '(>>=)' operations.
sequenceQ :: forall m . Monad m => forall a . [m a] -> m [a]
sequenceQ = sequence
