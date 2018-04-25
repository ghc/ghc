{-# LANGUAGE DeriveDataTypeable,
             DeriveGeneric, FlexibleInstances, DefaultSignatures,
             RankNTypes, RoleAnnotations, ScopedTypeVariables,
             Trustworthy #-}

{-# OPTIONS_GHC -fno-warn-inline-rule-shadowing #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Syntax
-- Copyright   :  (c) The University of Glasgow 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Abstract syntax definitions for Template Haskell.
--
-----------------------------------------------------------------------------

module Language.Haskell.TH.Syntax
    ( module Language.Haskell.TH.Syntax
      -- * Language extensions
    , module Language.Haskell.TH.LanguageExtensions
    , ForeignSrcLang(..)
    ) where

import Data.Data hiding (Fixity(..))
import Data.IORef
import System.IO.Unsafe ( unsafePerformIO )
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO (..))
import System.IO        ( hPutStrLn, stderr )
import Data.Char        ( isAlpha, isAlphaNum, isUpper )
import Data.Int
import Data.Word
import Data.Ratio
import GHC.Generics     ( Generic )
import GHC.Lexeme       ( startsVarSym, startsVarId )
import GHC.ForeignSrcLang.Type
import Language.Haskell.TH.LanguageExtensions
import Numeric.Natural

import qualified Control.Monad.Fail as Fail

-----------------------------------------------------
--
--              The Quasi class
--
-----------------------------------------------------

class (MonadIO m, Fail.MonadFail m) => Quasi m where
  qNewName :: String -> m Name
        -- ^ Fresh names

        -- Error reporting and recovery
  qReport  :: Bool -> String -> m ()    -- ^ Report an error (True) or warning (False)
                                        -- ...but carry on; use 'fail' to stop
  qRecover :: m a -- ^ the error handler
           -> m a -- ^ action which may fail
           -> m a               -- ^ Recover from the monadic 'fail'

        -- Inspect the type-checker's environment
  qLookupName :: Bool -> String -> m (Maybe Name)
       -- True <=> type namespace, False <=> value namespace
  qReify          :: Name -> m Info
  qReifyFixity    :: Name -> m (Maybe Fixity)
  qReifyInstances :: Name -> [Type] -> m [Dec]
       -- Is (n tys) an instance?
       -- Returns list of matching instance Decs
       --    (with empty sub-Decs)
       -- Works for classes and type functions
  qReifyRoles         :: Name -> m [Role]
  qReifyAnnotations   :: Data a => AnnLookup -> m [a]
  qReifyModule        :: Module -> m ModuleInfo
  qReifyConStrictness :: Name -> m [DecidedStrictness]

  qLocation :: m Loc

  qRunIO :: IO a -> m a
  qRunIO = liftIO
  -- ^ Input/output (dangerous)

  qAddDependentFile :: FilePath -> m ()

  qAddTopDecls :: [Dec] -> m ()

  qAddForeignFile :: ForeignSrcLang -> String -> m ()

  qAddModFinalizer :: Q () -> m ()

  qAddCorePlugin :: String -> m ()

  qGetQ :: Typeable a => m (Maybe a)

  qPutQ :: Typeable a => a -> m ()

  qIsExtEnabled :: Extension -> m Bool
  qExtsEnabled :: m [Extension]

-----------------------------------------------------
--      The IO instance of Quasi
--
--  This instance is used only when running a Q
--  computation in the IO monad, usually just to
--  print the result.  There is no interesting
--  type environment, so reification isn't going to
--  work.
--
-----------------------------------------------------

instance Quasi IO where
  qNewName s = do { n <- atomicModifyIORef' counter (\x -> (x + 1, x))
                  ; pure (mkNameU s n) }

  qReport True  msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)
  qReport False msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)

  qLookupName _ _       = badIO "lookupName"
  qReify _              = badIO "reify"
  qReifyFixity _        = badIO "reifyFixity"
  qReifyInstances _ _   = badIO "reifyInstances"
  qReifyRoles _         = badIO "reifyRoles"
  qReifyAnnotations _   = badIO "reifyAnnotations"
  qReifyModule _        = badIO "reifyModule"
  qReifyConStrictness _ = badIO "reifyConStrictness"
  qLocation             = badIO "currentLocation"
  qRecover _ _          = badIO "recover" -- Maybe we could fix this?
  qAddDependentFile _   = badIO "addDependentFile"
  qAddTopDecls _        = badIO "addTopDecls"
  qAddForeignFile _ _   = badIO "addForeignFile"
  qAddModFinalizer _    = badIO "addModFinalizer"
  qAddCorePlugin _      = badIO "addCorePlugin"
  qGetQ                 = badIO "getQ"
  qPutQ _               = badIO "putQ"
  qIsExtEnabled _       = badIO "isExtEnabled"
  qExtsEnabled          = badIO "extsEnabled"

badIO :: String -> IO a
badIO op = do   { qReport True ("Can't do `" ++ op ++ "' in the IO monad")
                ; fail "Template Haskell failure" }

-- Global variable to generate unique symbols
counter :: IORef Int
{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0)


-----------------------------------------------------
--
--              The Q monad
--
-----------------------------------------------------

newtype Q a = Q { unQ :: forall m. Quasi m => m a }

-- \"Runs\" the 'Q' monad. Normal users of Template Haskell
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
  fail       = Fail.fail

instance Fail.MonadFail Q where
  fail s     = report True s >> Q (Fail.fail "Q monad failure")

instance Functor Q where
  fmap f (Q x) = Q (fmap f x)

instance Applicative Q where
  pure x = Q (pure x)
  Q f <*> Q x = Q (f <*> x)
  Q m *> Q n = Q (m *> n)

-----------------------------------------------------
--
--              The TExp type
--
-----------------------------------------------------

type role TExp nominal   -- See Note [Role of TExp]
newtype TExp a = TExp { unType :: Exp }

unTypeQ :: Q (TExp a) -> Q Exp
unTypeQ m = do { TExp e <- m
               ; return e }

unsafeTExpCoerce :: Q Exp -> Q (TExp a)
unsafeTExpCoerce m = do { e <- m
                        ; return (TExp e) }

{- Note [Role of TExp]
~~~~~~~~~~~~~~~~~~~~~~
TExp's argument must have a nominal role, not phantom as would
be inferred (Trac #8459).  Consider

  e :: TExp Age
  e = MkAge 3

  foo = $(coerce e) + 4::Int

The splice will evaluate to (MkAge 3) and you can't add that to
4::Int. So you can't coerce a (TExp Age) to a (TExp Int). -}

----------------------------------------------------
-- Packaged versions for the programmer, hiding the Quasi-ness

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
newName :: String -> Q Name
newName s = Q (qNewName s)

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


{- | 'reify' looks up information about the 'Name'.

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

{- | @reifyInstances nm tys@ returns a list of visible instances of @nm tys@. That is,
if @nm@ is the name of a type class, then all instances of this class at the types @tys@
are returned. Alternatively, if @nm@ is the name of a data family or type family,
all instances of this family at the types @tys@ are returned.
-}
reifyInstances :: Name -> [Type] -> Q [InstanceDec]
reifyInstances cls tys = Q (qReifyInstances cls tys)

{- | @reifyRoles nm@ returns the list of roles associated with the parameters of
the tycon @nm@. Fails if @nm@ cannot be found or is not a tycon.
The returned list should never contain 'InferR'.
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
-- value of @thisModule@.
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
-- To get better errors, it is reccomended to use #line pragmas when
-- emitting C files, e.g.
--
-- > {-# LANGUAGE CPP #-}
-- > ...
-- > addForeignFile LangC $ unlines
-- >   [ "#line " ++ show (__LINE__ + 1) ++ " " ++ show __FILE__
-- >   , ...
-- >   ]
addForeignFile :: ForeignSrcLang -> String -> Q ()
addForeignFile lang str = Q (qAddForeignFile lang str)

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

-- | Get state from the 'Q' monad. Note that the state is local to the
-- Haskell module in which the Template Haskell expression is executed.
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

instance MonadIO Q where
  liftIO = runIO

instance Quasi Q where
  qNewName            = newName
  qReport             = report
  qRecover            = recover
  qReify              = reify
  qReifyFixity        = reifyFixity
  qReifyInstances     = reifyInstances
  qReifyRoles         = reifyRoles
  qReifyAnnotations   = reifyAnnotations
  qReifyModule        = reifyModule
  qReifyConStrictness = reifyConStrictness
  qLookupName         = lookupName
  qLocation           = location
  qAddDependentFile   = addDependentFile
  qAddTopDecls        = addTopDecls
  qAddForeignFile     = addForeignFile
  qAddModFinalizer    = addModFinalizer
  qAddCorePlugin      = addCorePlugin
  qGetQ               = getQ
  qPutQ               = putQ
  qIsExtEnabled       = isExtEnabled
  qExtsEnabled        = extsEnabled


----------------------------------------------------
-- The following operations are used solely in DsMeta when desugaring brackets
-- They are not necessary for the user, who can use ordinary return and (>>=) etc

returnQ :: a -> Q a
returnQ = return

bindQ :: Q a -> (a -> Q b) -> Q b
bindQ = (>>=)

sequenceQ :: [Q a] -> Q [a]
sequenceQ = sequence


-----------------------------------------------------
--
--              The Lift class
--
-----------------------------------------------------

-- | A 'Lift' instance can have any of its values turned into a Template
-- Haskell expression. This is needed when a value used within a Template
-- Haskell quotation is bound outside the Oxford brackets (@[| ... |]@) but not
-- at the top level. As an example:
--
-- > add1 :: Int -> Q Exp
-- > add1 x = [| x + 1 |]
--
-- Template Haskell has no way of knowing what value @x@ will take on at
-- splice-time, so it requires the type of @x@ to be an instance of 'Lift'.
--
-- A 'Lift' instance must satisfy @$(lift x) ≡ x@ for all @x@, where @$(...)@
-- is a Template Haskell splice.
--
-- 'Lift' instances can be derived automatically by use of the @-XDeriveLift@
-- GHC language extension:
--
-- > {-# LANGUAGE DeriveLift #-}
-- > module Foo where
-- >
-- > import Language.Haskell.TH.Syntax
-- >
-- > data Bar a = Bar1 a (Bar a) | Bar2 String
-- >   deriving Lift
class Lift t where
  -- | Turn a value into a Template Haskell expression, suitable for use in
  -- a splice.
  lift :: t -> Q Exp
  default lift :: Data t => t -> Q Exp
  lift = liftData

-- If you add any instances here, consider updating test th/TH_Lift
instance Lift Integer where
  lift x = return (LitE (IntegerL x))

instance Lift Int where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int8 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int16 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int32 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int64 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word8 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word16 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word32 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word64 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Natural where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Integral a => Lift (Ratio a) where
  lift x = return (LitE (RationalL (toRational x)))

instance Lift Float where
  lift x = return (LitE (RationalL (toRational x)))

instance Lift Double where
  lift x = return (LitE (RationalL (toRational x)))

instance Lift Char where
  lift x = return (LitE (CharL x))

instance Lift Bool where
  lift True  = return (ConE trueName)
  lift False = return (ConE falseName)

instance Lift a => Lift (Maybe a) where
  lift Nothing  = return (ConE nothingName)
  lift (Just x) = liftM (ConE justName `AppE`) (lift x)

instance (Lift a, Lift b) => Lift (Either a b) where
  lift (Left x)  = liftM (ConE leftName  `AppE`) (lift x)
  lift (Right y) = liftM (ConE rightName `AppE`) (lift y)

instance Lift a => Lift [a] where
  lift xs = do { xs' <- mapM lift xs; return (ListE xs') }

liftString :: String -> Q Exp
-- Used in TcExpr to short-circuit the lifting for strings
liftString s = return (LitE (StringL s))

instance Lift () where
  lift () = return (ConE (tupleDataName 0))

instance (Lift a, Lift b) => Lift (a, b) where
  lift (a, b)
    = liftM TupE $ sequence [lift a, lift b]

instance (Lift a, Lift b, Lift c) => Lift (a, b, c) where
  lift (a, b, c)
    = liftM TupE $ sequence [lift a, lift b, lift c]

instance (Lift a, Lift b, Lift c, Lift d) => Lift (a, b, c, d) where
  lift (a, b, c, d)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d]

instance (Lift a, Lift b, Lift c, Lift d, Lift e)
      => Lift (a, b, c, d, e) where
  lift (a, b, c, d, e)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e]

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f)
      => Lift (a, b, c, d, e, f) where
  lift (a, b, c, d, e, f)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e, lift f]

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g)
      => Lift (a, b, c, d, e, f, g) where
  lift (a, b, c, d, e, f, g)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e, lift f, lift g]

-- TH has a special form for literal strings,
-- which we should take advantage of.
-- NB: the lhs of the rule has no args, so that
--     the rule will apply to a 'lift' all on its own
--     which happens to be the way the type checker
--     creates it.
{-# RULES "TH:liftString" lift = \s -> return (LitE (StringL s)) #-}


trueName, falseName :: Name
trueName  = mkNameG DataName "ghc-prim" "GHC.Types" "True"
falseName = mkNameG DataName "ghc-prim" "GHC.Types" "False"

nothingName, justName :: Name
nothingName = mkNameG DataName "base" "GHC.Base" "Nothing"
justName    = mkNameG DataName "base" "GHC.Base" "Just"

leftName, rightName :: Name
leftName  = mkNameG DataName "base" "Data.Either" "Left"
rightName = mkNameG DataName "base" "Data.Either" "Right"

-----------------------------------------------------
--
--              Generic Lift implementations
--
-----------------------------------------------------

-- | 'dataToQa' is an internal utility function for constructing generic
-- conversion functions from types with 'Data' instances to various
-- quasi-quoting representations.  See the source of 'dataToExpQ' and
-- 'dataToPatQ' for two example usages: @mkCon@, @mkLit@
-- and @appQ@ are overloadable to account for different syntax for
-- expressions and patterns; @antiQ@ allows you to override type-specific
-- cases, a common usage is just @const Nothing@, which results in
-- no overloading.
dataToQa  ::  forall a k q. Data a
          =>  (Name -> k)
          ->  (Lit -> Q q)
          ->  (k -> [Q q] -> Q q)
          ->  (forall b . Data b => b -> Maybe (Q q))
          ->  a
          ->  Q q
dataToQa mkCon mkLit appCon antiQ t =
    case antiQ t of
      Nothing ->
          case constrRep constr of
            AlgConstr _ ->
                appCon (mkCon funOrConName) conArgs
              where
                funOrConName :: Name
                funOrConName =
                    case showConstr constr of
                      "(:)"       -> Name (mkOccName ":")
                                          (NameG DataName
                                                (mkPkgName "ghc-prim")
                                                (mkModName "GHC.Types"))
                      con@"[]"    -> Name (mkOccName con)
                                          (NameG DataName
                                                (mkPkgName "ghc-prim")
                                                (mkModName "GHC.Types"))
                      con@('(':_) -> Name (mkOccName con)
                                          (NameG DataName
                                                (mkPkgName "ghc-prim")
                                                (mkModName "GHC.Tuple"))

                      -- Tricky case: see Note [Data for non-algebraic types]
                      fun@(x:_)   | startsVarSym x || startsVarId x
                                  -> mkNameG_v tyconPkg tyconMod fun
                      con         -> mkNameG_d tyconPkg tyconMod con

                  where
                    tycon :: TyCon
                    tycon = (typeRepTyCon . typeOf) t

                    tyconPkg, tyconMod :: String
                    tyconPkg = tyConPackage tycon
                    tyconMod = tyConModule  tycon

                conArgs :: [Q q]
                conArgs = gmapQ (dataToQa mkCon mkLit appCon antiQ) t
            IntConstr n ->
                mkLit $ IntegerL n
            FloatConstr n ->
                mkLit $ RationalL n
            CharConstr c ->
                mkLit $ CharL c
        where
          constr :: Constr
          constr = toConstr t

      Just y -> y


{- Note [Data for non-algebraic types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Class Data was originally intended for algebraic data types.  But
it is possible to use it for abstract types too.  For example, in
package `text` we find

  instance Data Text where
    ...
    toConstr _ = packConstr

  packConstr :: Constr
  packConstr = mkConstr textDataType "pack" [] Prefix

Here `packConstr` isn't a real data constructor, it's an ordinary
function.  Two complications

* In such a case, we must take care to build the Name using
  mkNameG_v (for values), not mkNameG_d (for data constructors).
  See Trac #10796.

* The pseudo-constructor is named only by its string, here "pack".
  But 'dataToQa' needs the TyCon of its defining module, and has
  to assume it's defined in the same module as the TyCon itself.
  But nothing enforces that; Trac #12596 shows what goes wrong if
  "pack" is defined in a different module than the data type "Text".
  -}

-- | 'dataToExpQ' converts a value to a 'Q Exp' representation of the
-- same value, in the SYB style. It is generalized to take a function
-- override type-specific cases; see 'liftData' for a more commonly
-- used variant.
dataToExpQ  ::  Data a
            =>  (forall b . Data b => b -> Maybe (Q Exp))
            ->  a
            ->  Q Exp
dataToExpQ = dataToQa varOrConE litE (foldl appE)
    where
          -- Make sure that VarE is used if the Constr value relies on a
          -- function underneath the surface (instead of a constructor).
          -- See Trac #10796.
          varOrConE s =
            case nameSpace s of
                 Just VarName  -> return (VarE s)
                 Just DataName -> return (ConE s)
                 _ -> fail $ "Can't construct an expression from name "
                          ++ showName s
          appE x y = do { a <- x; b <- y; return (AppE a b)}
          litE c = return (LitE c)

-- | 'liftData' is a variant of 'lift' in the 'Lift' type class which
-- works for any type with a 'Data' instance.
liftData :: Data a => a -> Q Exp
liftData = dataToExpQ (const Nothing)

-- | 'dataToPatQ' converts a value to a 'Q Pat' representation of the same
-- value, in the SYB style. It takes a function to handle type-specific cases,
-- alternatively, pass @const Nothing@ to get default behavior.
dataToPatQ  ::  Data a
            =>  (forall b . Data b => b -> Maybe (Q Pat))
            ->  a
            ->  Q Pat
dataToPatQ = dataToQa id litP conP
    where litP l = return (LitP l)
          conP n ps =
            case nameSpace n of
                Just DataName -> do
                    ps' <- sequence ps
                    return (ConP n ps')
                _ -> fail $ "Can't construct a pattern from name "
                         ++ showName n

-----------------------------------------------------
--              Names and uniques
-----------------------------------------------------

newtype ModName = ModName String        -- Module name
 deriving (Show,Eq,Ord,Data,Generic)

newtype PkgName = PkgName String        -- package name
 deriving (Show,Eq,Ord,Data,Generic)

-- | Obtained from 'reifyModule' and 'thisModule'.
data Module = Module PkgName ModName -- package qualified module name
 deriving (Show,Eq,Ord,Data,Generic)

newtype OccName = OccName String
 deriving (Show,Eq,Ord,Data,Generic)

mkModName :: String -> ModName
mkModName s = ModName s

modString :: ModName -> String
modString (ModName m) = m


mkPkgName :: String -> PkgName
mkPkgName s = PkgName s

pkgString :: PkgName -> String
pkgString (PkgName m) = m


-----------------------------------------------------
--              OccName
-----------------------------------------------------

mkOccName :: String -> OccName
mkOccName s = OccName s

occString :: OccName -> String
occString (OccName occ) = occ


-----------------------------------------------------
--               Names
-----------------------------------------------------
--
-- For "global" names ('NameG') we need a totally unique name,
-- so we must include the name-space of the thing
--
-- For unique-numbered things ('NameU'), we've got a unique reference
-- anyway, so no need for name space
--
-- For dynamically bound thing ('NameS') we probably want them to
-- in a context-dependent way, so again we don't want the name
-- space.  For example:
--
-- > let v = mkName "T" in [| data $v = $v |]
--
-- Here we use the same Name for both type constructor and data constructor
--
--
-- NameL and NameG are bound *outside* the TH syntax tree
-- either globally (NameG) or locally (NameL). Ex:
--
-- > f x = $(h [| (map, x) |])
--
-- The 'map' will be a NameG, and 'x' wil be a NameL
--
-- These Names should never appear in a binding position in a TH syntax tree

{- $namecapture #namecapture#
Much of 'Name' API is concerned with the problem of /name capture/, which
can be seen in the following example.

> f expr = [| let x = 0 in $expr |]
> ...
> g x = $( f [| x |] )
> h y = $( f [| y |] )

A naive desugaring of this would yield:

> g x = let x = 0 in x
> h y = let x = 0 in y

All of a sudden, @g@ and @h@ have different meanings! In this case,
we say that the @x@ in the RHS of @g@ has been /captured/
by the binding of @x@ in @f@.

What we actually want is for the @x@ in @f@ to be distinct from the
@x@ in @g@, so we get the following desugaring:

> g x = let x' = 0 in x
> h y = let x' = 0 in y

which avoids name capture as desired.

In the general case, we say that a @Name@ can be captured if
the thing it refers to can be changed by adding new declarations.
-}

{- |
An abstract type representing names in the syntax tree.

'Name's can be constructed in several ways, which come with different
name-capture guarantees (see "Language.Haskell.TH.Syntax#namecapture" for
an explanation of name capture):

  * the built-in syntax @'f@ and @''T@ can be used to construct names,
    The expression @'f@ gives a @Name@ which refers to the value @f@
    currently in scope, and @''T@ gives a @Name@ which refers to the
    type @T@ currently in scope. These names can never be captured.

  * 'lookupValueName' and 'lookupTypeName' are similar to @'f@ and
     @''T@ respectively, but the @Name@s are looked up at the point
     where the current splice is being run. These names can never be
     captured.

  * 'newName' monadically generates a new name, which can never
     be captured.

  * 'mkName' generates a capturable name.

Names constructed using @newName@ and @mkName@ may be used in bindings
(such as @let x = ...@ or @\x -> ...@), but names constructed using
@lookupValueName@, @lookupTypeName@, @'f@, @''T@ may not.
-}
data Name = Name OccName NameFlavour deriving (Data, Eq, Generic)

instance Ord Name where
    -- check if unique is different before looking at strings
  (Name o1 f1) `compare` (Name o2 f2) = (f1 `compare` f2)   `thenCmp`
                                        (o1 `compare` o2)

data NameFlavour
  = NameS           -- ^ An unqualified name; dynamically bound
  | NameQ ModName   -- ^ A qualified name; dynamically bound
  | NameU !Int      -- ^ A unique local name
  | NameL !Int      -- ^ Local name bound outside of the TH AST
  | NameG NameSpace PkgName ModName -- ^ Global name bound outside of the TH AST:
                -- An original name (occurrences only, not binders)
                -- Need the namespace too to be sure which
                -- thing we are naming
  deriving ( Data, Eq, Ord, Show, Generic )

data NameSpace = VarName        -- ^ Variables
               | DataName       -- ^ Data constructors
               | TcClsName      -- ^ Type constructors and classes; Haskell has them
                                -- in the same name space for now.
               deriving( Eq, Ord, Show, Data, Generic )

type Uniq = Int

-- | The name without its module prefix.
--
-- ==== __Examples__
--
-- >>> nameBase ''Data.Either.Either
-- "Either"
-- >>> nameBase (mkName "foo")
-- "foo"
-- >>> nameBase (mkName "Module.foo")
-- "foo"
nameBase :: Name -> String
nameBase (Name occ _) = occString occ

-- | Module prefix of a name, if it exists.
--
-- ==== __Examples__
--
-- >>> nameModule ''Data.Either.Either
-- Just "Data.Either"
-- >>> nameModule (mkName "foo")
-- Nothing
-- >>> nameModule (mkName "Module.foo")
-- Just "Module"
nameModule :: Name -> Maybe String
nameModule (Name _ (NameQ m))     = Just (modString m)
nameModule (Name _ (NameG _ _ m)) = Just (modString m)
nameModule _                      = Nothing

-- | A name's package, if it exists.
--
-- ==== __Examples__
--
-- >>> namePackage ''Data.Either.Either
-- Just "base"
-- >>> namePackage (mkName "foo")
-- Nothing
-- >>> namePackage (mkName "Module.foo")
-- Nothing
namePackage :: Name -> Maybe String
namePackage (Name _ (NameG _ p _)) = Just (pkgString p)
namePackage _                      = Nothing

-- | Returns whether a name represents an occurrence of a top-level variable
-- ('VarName'), data constructor ('DataName'), type constructor, or type class
-- ('TcClsName'). If we can't be sure, it returns 'Nothing'.
--
-- ==== __Examples__
--
-- >>> nameSpace 'Prelude.id
-- Just VarName
-- >>> nameSpace (mkName "id")
-- Nothing -- only works for top-level variable names
-- >>> nameSpace 'Data.Maybe.Just
-- Just DataName
-- >>> nameSpace ''Data.Maybe.Maybe
-- Just TcClsName
-- >>> nameSpace ''Data.Ord.Ord
-- Just TcClsName
nameSpace :: Name -> Maybe NameSpace
nameSpace (Name _ (NameG ns _ _)) = Just ns
nameSpace _                       = Nothing

{- |
Generate a capturable name. Occurrences of such names will be
resolved according to the Haskell scoping rules at the occurrence
site.

For example:

> f = [| pi + $(varE (mkName "pi")) |]
> ...
> g = let pi = 3 in $f

In this case, @g@ is desugared to

> g = Prelude.pi + 3

Note that @mkName@ may be used with qualified names:

> mkName "Prelude.pi"

See also 'Language.Haskell.TH.Lib.dyn' for a useful combinator. The above example could
be rewritten using 'dyn' as

> f = [| pi + $(dyn "pi") |]
-}
mkName :: String -> Name
-- The string can have a '.', thus "Foo.baz",
-- giving a dynamically-bound qualified name,
-- in which case we want to generate a NameQ
--
-- Parse the string to see if it has a "." in it
-- so we know whether to generate a qualified or unqualified name
-- It's a bit tricky because we need to parse
--
-- > Foo.Baz.x   as    Qual Foo.Baz x
--
-- So we parse it from back to front
mkName str
  = split [] (reverse str)
  where
    split occ []        = Name (mkOccName occ) NameS
    split occ ('.':rev) | not (null occ)
                        , is_rev_mod_name rev
                        = Name (mkOccName occ) (NameQ (mkModName (reverse rev)))
        -- The 'not (null occ)' guard ensures that
        --      mkName "&." = Name "&." NameS
        -- The 'is_rev_mod' guards ensure that
        --      mkName ".&" = Name ".&" NameS
        --      mkName "^.." = Name "^.." NameS      -- Trac #8633
        --      mkName "Data.Bits..&" = Name ".&" (NameQ "Data.Bits")
        -- This rather bizarre case actually happened; (.&.) is in Data.Bits
    split occ (c:rev)   = split (c:occ) rev

    -- Recognises a reversed module name xA.yB.C,
    -- with at least one component,
    -- and each component looks like a module name
    --   (i.e. non-empty, starts with capital, all alpha)
    is_rev_mod_name rev_mod_str
      | (compt, rest) <- break (== '.') rev_mod_str
      , not (null compt), isUpper (last compt), all is_mod_char compt
      = case rest of
          []             -> True
          (_dot : rest') -> is_rev_mod_name rest'
      | otherwise
      = False

    is_mod_char c = isAlphaNum c || c == '_' || c == '\''

-- | Only used internally
mkNameU :: String -> Uniq -> Name
mkNameU s u = Name (mkOccName s) (NameU u)

-- | Only used internally
mkNameL :: String -> Uniq -> Name
mkNameL s u = Name (mkOccName s) (NameL u)

-- | Used for 'x etc, but not available to the programmer
mkNameG :: NameSpace -> String -> String -> String -> Name
mkNameG ns pkg modu occ
  = Name (mkOccName occ) (NameG ns (mkPkgName pkg) (mkModName modu))

mkNameS :: String -> Name
mkNameS n = Name (mkOccName n) NameS

mkNameG_v, mkNameG_tc, mkNameG_d :: String -> String -> String -> Name
mkNameG_v  = mkNameG VarName
mkNameG_tc = mkNameG TcClsName
mkNameG_d  = mkNameG DataName

data NameIs = Alone | Applied | Infix

showName :: Name -> String
showName = showName' Alone

showName' :: NameIs -> Name -> String
showName' ni nm
 = case ni of
       Alone        -> nms
       Applied
        | pnam      -> nms
        | otherwise -> "(" ++ nms ++ ")"
       Infix
        | pnam      -> "`" ++ nms ++ "`"
        | otherwise -> nms
    where
        -- For now, we make the NameQ and NameG print the same, even though
        -- NameQ is a qualified name (so what it means depends on what the
        -- current scope is), and NameG is an original name (so its meaning
        -- should be independent of what's in scope.
        -- We may well want to distinguish them in the end.
        -- Ditto NameU and NameL
        nms = case nm of
                    Name occ NameS         -> occString occ
                    Name occ (NameQ m)     -> modString m ++ "." ++ occString occ
                    Name occ (NameG _ _ m) -> modString m ++ "." ++ occString occ
                    Name occ (NameU u)     -> occString occ ++ "_" ++ show u
                    Name occ (NameL u)     -> occString occ ++ "_" ++ show u

        pnam = classify nms

        -- True if we are function style, e.g. f, [], (,)
        -- False if we are operator style, e.g. +, :+
        classify "" = False -- shouldn't happen; . operator is handled below
        classify (x:xs) | isAlpha x || (x `elem` "_[]()") =
                            case dropWhile (/='.') xs of
                                  (_:xs') -> classify xs'
                                  []      -> True
                        | otherwise = False

instance Show Name where
  show = showName

-- Tuple data and type constructors
-- | Tuple data constructor
tupleDataName :: Int -> Name
-- | Tuple type constructor
tupleTypeName :: Int -> Name

tupleDataName 0 = mk_tup_name 0 DataName
tupleDataName 1 = error "tupleDataName 1"
tupleDataName n = mk_tup_name (n-1) DataName

tupleTypeName 0 = mk_tup_name 0 TcClsName
tupleTypeName 1 = error "tupleTypeName 1"
tupleTypeName n = mk_tup_name (n-1) TcClsName

mk_tup_name :: Int -> NameSpace -> Name
mk_tup_name n_commas space
  = Name occ (NameG space (mkPkgName "ghc-prim") tup_mod)
  where
    occ = mkOccName ('(' : replicate n_commas ',' ++ ")")
    tup_mod = mkModName "GHC.Tuple"

-- Unboxed tuple data and type constructors
-- | Unboxed tuple data constructor
unboxedTupleDataName :: Int -> Name
-- | Unboxed tuple type constructor
unboxedTupleTypeName :: Int -> Name

unboxedTupleDataName n = mk_unboxed_tup_name n DataName
unboxedTupleTypeName n = mk_unboxed_tup_name n TcClsName

mk_unboxed_tup_name :: Int -> NameSpace -> Name
mk_unboxed_tup_name n space
  = Name (mkOccName tup_occ) (NameG space (mkPkgName "ghc-prim") tup_mod)
  where
    tup_occ | n == 1    = "Unit#" -- See Note [One-tuples] in TysWiredIn
            | otherwise = "(#" ++ replicate n_commas ',' ++ "#)"
    n_commas = n - 1
    tup_mod  = mkModName "GHC.Tuple"

-- Unboxed sum data and type constructors
-- | Unboxed sum data constructor
unboxedSumDataName :: SumAlt -> SumArity -> Name
-- | Unboxed sum type constructor
unboxedSumTypeName :: SumArity -> Name

unboxedSumDataName alt arity
  | alt > arity
  = error $ prefix ++ "Index out of bounds." ++ debug_info

  | alt <= 0
  = error $ prefix ++ "Alt must be > 0." ++ debug_info

  | arity < 2
  = error $ prefix ++ "Arity must be >= 2." ++ debug_info

  | otherwise
  = Name (mkOccName sum_occ)
         (NameG DataName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))

  where
    prefix     = "unboxedSumDataName: "
    debug_info = " (alt: " ++ show alt ++ ", arity: " ++ show arity ++ ")"

    -- Synced with the definition of mkSumDataConOcc in TysWiredIn
    sum_occ = '(' : '#' : bars nbars_before ++ '_' : bars nbars_after ++ "#)"
    bars i = replicate i '|'
    nbars_before = alt - 1
    nbars_after  = arity - alt

unboxedSumTypeName arity
  | arity < 2
  = error $ "unboxedSumTypeName: Arity must be >= 2."
         ++ " (arity: " ++ show arity ++ ")"

  | otherwise
  = Name (mkOccName sum_occ)
         (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))

  where
    -- Synced with the definition of mkSumTyConOcc in TysWiredIn
    sum_occ = '(' : '#' : replicate (arity - 1) '|' ++ "#)"

-----------------------------------------------------
--              Locations
-----------------------------------------------------

data Loc
  = Loc { loc_filename :: String
        , loc_package  :: String
        , loc_module   :: String
        , loc_start    :: CharPos
        , loc_end      :: CharPos }
   deriving( Show, Eq, Ord, Data, Generic )

type CharPos = (Int, Int)       -- ^ Line and character position


-----------------------------------------------------
--
--      The Info returned by reification
--
-----------------------------------------------------

-- | Obtained from 'reify' in the 'Q' Monad.
data Info
  =
  -- | A class, with a list of its visible instances
  ClassI
      Dec
      [InstanceDec]

  -- | A class method
  | ClassOpI
       Name
       Type
       ParentName

  -- | A \"plain\" type constructor. \"Fancier\" type constructors are returned using 'PrimTyConI' or 'FamilyI' as appropriate
  | TyConI
        Dec

  -- | A type or data family, with a list of its visible instances. A closed
  -- type family is returned with 0 instances.
  | FamilyI
        Dec
        [InstanceDec]

  -- | A \"primitive\" type constructor, which can't be expressed with a 'Dec'. Examples: @(->)@, @Int#@.
  | PrimTyConI
       Name
       Arity
       Unlifted

  -- | A data constructor
  | DataConI
       Name
       Type
       ParentName

  -- | A pattern synonym.
  | PatSynI
       Name
       PatSynType

  {- |
  A \"value\" variable (as opposed to a type variable, see 'TyVarI').

  The @Maybe Dec@ field contains @Just@ the declaration which
  defined the variable -- including the RHS of the declaration --
  or else @Nothing@, in the case where the RHS is unavailable to
  the compiler. At present, this value is _always_ @Nothing@:
  returning the RHS has not yet been implemented because of
  lack of interest.
  -}
  | VarI
       Name
       Type
       (Maybe Dec)

  {- |
  A type variable.

  The @Type@ field contains the type which underlies the variable.
  At present, this is always @'VarT' theName@, but future changes
  may permit refinement of this.
  -}
  | TyVarI      -- Scoped type variable
        Name
        Type    -- What it is bound to
  deriving( Show, Eq, Ord, Data, Generic )

-- | Obtained from 'reifyModule' in the 'Q' Monad.
data ModuleInfo =
  -- | Contains the import list of the module.
  ModuleInfo [Module]
  deriving( Show, Eq, Ord, Data, Generic )

{- |
In 'ClassOpI' and 'DataConI', name of the parent class or type
-}
type ParentName = Name

-- | In 'UnboxedSumE' and 'UnboxedSumP', the number associated with a
-- particular data constructor. 'SumAlt's are one-indexed and should never
-- exceed the value of its corresponding 'SumArity'. For example:
--
-- * @(\#_|\#)@ has 'SumAlt' 1 (out of a total 'SumArity' of 2)
--
-- * @(\#|_\#)@ has 'SumAlt' 2 (out of a total 'SumArity' of 2)
type SumAlt = Int

-- | In 'UnboxedSumE', 'UnboxedSumT', and 'UnboxedSumP', the total number of
-- 'SumAlt's. For example, @(\#|\#)@ has a 'SumArity' of 2.
type SumArity = Int

-- | In 'PrimTyConI', arity of the type constructor
type Arity = Int

-- | In 'PrimTyConI', is the type constructor unlifted?
type Unlifted = Bool

-- | 'InstanceDec' desribes a single instance of a class or type function.
-- It is just a 'Dec', but guaranteed to be one of the following:
--
--   * 'InstanceD' (with empty @['Dec']@)
--
--   * 'DataInstD' or 'NewtypeInstD' (with empty derived @['Name']@)
--
--   * 'TySynInstD'
type InstanceDec = Dec

data Fixity          = Fixity Int FixityDirection
    deriving( Eq, Ord, Show, Data, Generic )
data FixityDirection = InfixL | InfixR | InfixN
    deriving( Eq, Ord, Show, Data, Generic )

-- | Highest allowed operator precedence for 'Fixity' constructor (answer: 9)
maxPrecedence :: Int
maxPrecedence = (9::Int)

-- | Default fixity: @infixl 9@
defaultFixity :: Fixity
defaultFixity = Fixity maxPrecedence InfixL


{-
Note [Unresolved infix]
~~~~~~~~~~~~~~~~~~~~~~~
-}
{- $infix #infix#
When implementing antiquotation for quasiquoters, one often wants
to parse strings into expressions:

> parse :: String -> Maybe Exp

But how should we parse @a + b * c@? If we don't know the fixities of
@+@ and @*@, we don't know whether to parse it as @a + (b * c)@ or @(a
+ b) * c@.

In cases like this, use 'UInfixE', 'UInfixP', or 'UInfixT', which stand for
\"unresolved infix expression/pattern/type\", respectively. When the compiler
is given a splice containing a tree of @UInfixE@ applications such as

> UInfixE
>   (UInfixE e1 op1 e2)
>   op2
>   (UInfixE e3 op3 e4)

it will look up and the fixities of the relevant operators and
reassociate the tree as necessary.

  * trees will not be reassociated across 'ParensE', 'ParensP', or 'ParensT',
    which are of use for parsing expressions like

    > (a + b * c) + d * e

  * 'InfixE', 'InfixP', and 'InfixT' expressions are never reassociated.

  * The 'UInfixE' constructor doesn't support sections. Sections
    such as @(a *)@ have no ambiguity, so 'InfixE' suffices. For longer
    sections such as @(a + b * c -)@, use an 'InfixE' constructor for the
    outer-most section, and use 'UInfixE' constructors for all
    other operators:

    > InfixE
    >   Just (UInfixE ...a + b * c...)
    >   op
    >   Nothing

    Sections such as @(a + b +)@ and @((a + b) +)@ should be rendered
    into 'Exp's differently:

    > (+ a + b)   ---> InfixE Nothing + (Just $ UInfixE a + b)
    >                    -- will result in a fixity error if (+) is left-infix
    > (+ (a + b)) ---> InfixE Nothing + (Just $ ParensE $ UInfixE a + b)
    >                    -- no fixity errors

  * Quoted expressions such as

    > [| a * b + c |] :: Q Exp
    > [p| a : b : c |] :: Q Pat
    > [t| T + T |] :: Q Type

    will never contain 'UInfixE', 'UInfixP', 'UInfixT', 'InfixT', 'ParensE',
    'ParensP', or 'ParensT' constructors.

-}

-----------------------------------------------------
--
--      The main syntax data types
--
-----------------------------------------------------

data Lit = CharL Char
         | StringL String
         | IntegerL Integer     -- ^ Used for overloaded and non-overloaded
                                -- literals. We don't have a good way to
                                -- represent non-overloaded literals at
                                -- the moment. Maybe that doesn't matter?
         | RationalL Rational   -- Ditto
         | IntPrimL Integer
         | WordPrimL Integer
         | FloatPrimL Rational
         | DoublePrimL Rational
         | StringPrimL [Word8]  -- ^ A primitive C-style string, type Addr#
         | CharPrimL Char
    deriving( Show, Eq, Ord, Data, Generic )

    -- We could add Int, Float, Double etc, as we do in HsLit,
    -- but that could complicate the
    -- supposedly-simple TH.Syntax literal type

-- | Pattern in Haskell given in @{}@
data Pat
  = LitP Lit                        -- ^ @{ 5 or \'c\' }@
  | VarP Name                       -- ^ @{ x }@
  | TupP [Pat]                      -- ^ @{ (p1,p2) }@
  | UnboxedTupP [Pat]               -- ^ @{ (\# p1,p2 \#) }@
  | UnboxedSumP Pat SumAlt SumArity -- ^ @{ (\#|p|\#) }@
  | ConP Name [Pat]                 -- ^ @data T1 = C1 t1 t2; {C1 p1 p1} = e@
  | InfixP Pat Name Pat             -- ^ @foo ({x :+ y}) = e@
  | UInfixP Pat Name Pat            -- ^ @foo ({x :+ y}) = e@
                                    --
                                    -- See "Language.Haskell.TH.Syntax#infix"
  | ParensP Pat                     -- ^ @{(p)}@
                                    --
                                    -- See "Language.Haskell.TH.Syntax#infix"
  | TildeP Pat                      -- ^ @{ ~p }@
  | BangP Pat                       -- ^ @{ !p }@
  | AsP Name Pat                    -- ^ @{ x \@ p }@
  | WildP                           -- ^ @{ _ }@
  | RecP Name [FieldPat]            -- ^ @f (Pt { pointx = x }) = g x@
  | ListP [ Pat ]                   -- ^ @{ [1,2,3] }@
  | SigP Pat Type                   -- ^ @{ p :: t }@
  | ViewP Exp Pat                   -- ^ @{ e -> p }@
  deriving( Show, Eq, Ord, Data, Generic )

type FieldPat = (Name,Pat)

data Match = Match Pat Body [Dec] -- ^ @case e of { pat -> body where decs }@
    deriving( Show, Eq, Ord, Data, Generic )
data Clause = Clause [Pat] Body [Dec]
                                  -- ^ @f { p1 p2 = body where decs }@
    deriving( Show, Eq, Ord, Data, Generic )

data Exp
  = VarE Name                          -- ^ @{ x }@
  | ConE Name                          -- ^ @data T1 = C1 t1 t2; p = {C1} e1 e2  @
  | LitE Lit                           -- ^ @{ 5 or \'c\'}@
  | AppE Exp Exp                       -- ^ @{ f x }@
  | AppTypeE Exp Type                  -- ^ @{ f \@Int }@

  | InfixE (Maybe Exp) Exp (Maybe Exp) -- ^ @{x + y} or {(x+)} or {(+ x)} or {(+)}@

    -- It's a bit gruesome to use an Exp as the
    -- operator, but how else can we distinguish
    -- constructors from non-constructors?
    -- Maybe there should be a var-or-con type?
    -- Or maybe we should leave it to the String itself?

  | UInfixE Exp Exp Exp                -- ^ @{x + y}@
                                       --
                                       -- See "Language.Haskell.TH.Syntax#infix"
  | ParensE Exp                        -- ^ @{ (e) }@
                                       --
                                       -- See "Language.Haskell.TH.Syntax#infix"
  | LamE [Pat] Exp                     -- ^ @{ \\ p1 p2 -> e }@
  | LamCaseE [Match]                   -- ^ @{ \\case m1; m2 }@
  | TupE [Exp]                         -- ^ @{ (e1,e2) }  @
  | UnboxedTupE [Exp]                  -- ^ @{ (\# e1,e2 \#) }  @
  | UnboxedSumE Exp SumAlt SumArity    -- ^ @{ (\#|e|\#) }@
  | CondE Exp Exp Exp                  -- ^ @{ if e1 then e2 else e3 }@
  | MultiIfE [(Guard, Exp)]            -- ^ @{ if | g1 -> e1 | g2 -> e2 }@
  | LetE [Dec] Exp                     -- ^ @{ let x=e1;   y=e2 in e3 }@
  | CaseE Exp [Match]                  -- ^ @{ case e of m1; m2 }@
  | DoE [Stmt]                         -- ^ @{ do { p <- e1; e2 }  }@
  | CompE [Stmt]                       -- ^ @{ [ (x,y) | x <- xs, y <- ys ] }@
      --
      -- The result expression of the comprehension is
      -- the /last/ of the @'Stmt'@s, and should be a 'NoBindS'.
      --
      -- E.g. translation:
      --
      -- > [ f x | x <- xs ]
      --
      -- > CompE [BindS (VarP x) (VarE xs), NoBindS (AppE (VarE f) (VarE x))]

  | ArithSeqE Range                    -- ^ @{ [ 1 ,2 .. 10 ] }@
  | ListE [ Exp ]                      -- ^ @{ [1,2,3] }@
  | SigE Exp Type                      -- ^ @{ e :: t }@
  | RecConE Name [FieldExp]            -- ^ @{ T { x = y, z = w } }@
  | RecUpdE Exp [FieldExp]             -- ^ @{ (f x) { z = w } }@
  | StaticE Exp                        -- ^ @{ static e }@
  | UnboundVarE Name                   -- ^ @{ _x }@ (hole)
  | LabelE String                      -- ^ @{ #x }@ ( Overloaded label )
  deriving( Show, Eq, Ord, Data, Generic )

type FieldExp = (Name,Exp)

-- Omitted: implicit parameters

data Body
  = GuardedB [(Guard,Exp)]   -- ^ @f p { | e1 = e2
                                 --      | e3 = e4 }
                                 -- where ds@
  | NormalB Exp              -- ^ @f p { = e } where ds@
  deriving( Show, Eq, Ord, Data, Generic )

data Guard
  = NormalG Exp -- ^ @f x { | odd x } = x@
  | PatG [Stmt] -- ^ @f x { | Just y <- x, Just z <- y } = z@
  deriving( Show, Eq, Ord, Data, Generic )

data Stmt
  = BindS Pat Exp
  | LetS [ Dec ]
  | NoBindS Exp
  | ParS [[Stmt]]
  deriving( Show, Eq, Ord, Data, Generic )

data Range = FromR Exp | FromThenR Exp Exp
           | FromToR Exp Exp | FromThenToR Exp Exp Exp
          deriving( Show, Eq, Ord, Data, Generic )

data Dec
  = FunD Name [Clause]            -- ^ @{ f p1 p2 = b where decs }@
  | ValD Pat Body [Dec]           -- ^ @{ p = b where decs }@
  | DataD Cxt Name [TyVarBndr]
          (Maybe Kind)            -- Kind signature (allowed only for GADTs)
          [Con] [DerivClause]
                                  -- ^ @{ data Cxt x => T x = A x | B (T x)
                                  --       deriving (Z,W)
                                  --       deriving stock Eq }@
  | NewtypeD Cxt Name [TyVarBndr]
             (Maybe Kind)         -- Kind signature
             Con [DerivClause]    -- ^ @{ newtype Cxt x => T x = A (B x)
                                  --       deriving (Z,W Q)
                                  --       deriving stock Eq }@
  | TySynD Name [TyVarBndr] Type  -- ^ @{ type T x = (x,x) }@
  | ClassD Cxt Name [TyVarBndr]
         [FunDep] [Dec]           -- ^ @{ class Eq a => Ord a where ds }@
  | InstanceD (Maybe Overlap) Cxt Type [Dec]
                                  -- ^ @{ instance {\-\# OVERLAPS \#-\}
                                  --        Show w => Show [w] where ds }@
  | SigD Name Type                -- ^ @{ length :: [a] -> Int }@
  | ForeignD Foreign              -- ^ @{ foreign import ... }
                                  --{ foreign export ... }@

  | InfixD Fixity Name            -- ^ @{ infix 3 foo }@

  -- | pragmas
  | PragmaD Pragma                -- ^ @{ {\-\# INLINE [1] foo \#-\} }@

  -- | data families (may also appear in [Dec] of 'ClassD' and 'InstanceD')
  | DataFamilyD Name [TyVarBndr]
               (Maybe Kind)
         -- ^ @{ data family T a b c :: * }@

  | DataInstD Cxt Name [Type]
             (Maybe Kind)         -- Kind signature
             [Con] [DerivClause]  -- ^ @{ data instance Cxt x => T [x]
                                  --       = A x | B (T x)
                                  --       deriving (Z,W)
                                  --       deriving stock Eq }@

  | NewtypeInstD Cxt Name [Type]
                 (Maybe Kind)      -- Kind signature
                 Con [DerivClause] -- ^ @{ newtype instance Cxt x => T [x]
                                   --        = A (B x)
                                   --        deriving (Z,W)
                                   --        deriving stock Eq }@
  | TySynInstD Name TySynEqn       -- ^ @{ type instance ... }@

  -- | open type families (may also appear in [Dec] of 'ClassD' and 'InstanceD')
  | OpenTypeFamilyD TypeFamilyHead
         -- ^ @{ type family T a b c = (r :: *) | r -> a b }@

  | ClosedTypeFamilyD TypeFamilyHead [TySynEqn]
       -- ^ @{ type family F a b = (r :: *) | r -> a where ... }@

  | RoleAnnotD Name [Role]     -- ^ @{ type role T nominal representational }@
  | StandaloneDerivD (Maybe DerivStrategy) Cxt Type
       -- ^ @{ deriving stock instance Ord a => Ord (Foo a) }@
  | DefaultSigD Name Type      -- ^ @{ default size :: Data a => a -> Int }@

  -- | Pattern Synonyms
  | PatSynD Name PatSynArgs PatSynDir Pat
      -- ^ @{ pattern P v1 v2 .. vn <- p }@  unidirectional           or
      --   @{ pattern P v1 v2 .. vn = p  }@  implicit bidirectional   or
      --   @{ pattern P v1 v2 .. vn <- p
      --        where P v1 v2 .. vn = e  }@  explicit bidirectional
      --
      -- also, besides prefix pattern synonyms, both infix and record
      -- pattern synonyms are supported. See 'PatSynArgs' for details

  | PatSynSigD Name PatSynType  -- ^ A pattern synonym's type signature.
  deriving( Show, Eq, Ord, Data, Generic )

-- | Varieties of allowed instance overlap.
data Overlap = Overlappable   -- ^ May be overlapped by more specific instances
             | Overlapping    -- ^ May overlap a more general instance
             | Overlaps       -- ^ Both 'Overlapping' and 'Overlappable'
             | Incoherent     -- ^ Both 'Overlappable' and 'Overlappable', and
                              -- pick an arbitrary one if multiple choices are
                              -- available.
  deriving( Show, Eq, Ord, Data, Generic )

-- | A single @deriving@ clause at the end of a datatype.
data DerivClause = DerivClause (Maybe DerivStrategy) Cxt
    -- ^ @{ deriving stock (Eq, Ord) }@
  deriving( Show, Eq, Ord, Data, Generic )

-- | What the user explicitly requests when deriving an instance.
data DerivStrategy = StockStrategy    -- ^ A \"standard\" derived instance
                   | AnyclassStrategy -- ^ @-XDeriveAnyClass@
                   | NewtypeStrategy  -- ^ @-XGeneralizedNewtypeDeriving@
  deriving( Show, Eq, Ord, Data, Generic )

-- | A Pattern synonym's type. Note that a pattern synonym's *fully*
-- specified type has a peculiar shape coming with two forall
-- quantifiers and two constraint contexts. For example, consider the
-- pattern synonym
--
--   pattern P x1 x2 ... xn = <some-pattern>
--
-- P's complete type is of the following form
--
--   forall universals. required constraints
--     => forall existentials. provided constraints
--     => t1 -> t2 -> ... -> tn -> t
--
-- consisting of four parts:
--
--   1) the (possibly empty lists of) universally quantified type
--      variables and required constraints on them.
--   2) the (possibly empty lists of) existentially quantified
--      type variables and the provided constraints on them.
--   3) the types t1, t2, .., tn of x1, x2, .., xn, respectively
--   4) the type t of <some-pattern>, mentioning only universals.
--
-- Pattern synonym types interact with TH when (a) reifying a pattern
-- synonym, (b) pretty printing, or (c) specifying a pattern synonym's
-- type signature explicitly:
--
-- (a) Reification always returns a pattern synonym's *fully* specified
--     type in abstract syntax.
--
-- (b) Pretty printing via 'pprPatSynType' abbreviates a pattern
--     synonym's type unambiguously in concrete syntax: The rule of
--     thumb is to print initial empty universals and the required
--     context as `() =>`, if existentials and a provided context
--     follow. If only universals and their required context, but no
--     existentials are specified, only the universals and their
--     required context are printed. If both or none are specified, so
--     both (or none) are printed.
--
-- (c) When specifying a pattern synonym's type explicitly with
--     'PatSynSigD' either one of the universals, the existentials, or
--     their contexts may be left empty.
--
-- See the GHC user's guide for more information on pattern synonyms
-- and their types: https://downloads.haskell.org/~ghc/latest/docs/html/
-- users_guide/syntax-extns.html#pattern-synonyms.
type PatSynType = Type

-- | Common elements of 'OpenTypeFamilyD' and 'ClosedTypeFamilyD'. By
-- analogy with "head" for type classes and type class instances as
-- defined in /Type classes: an exploration of the design space/, the
-- @TypeFamilyHead@ is defined to be the elements of the declaration
-- between @type family@ and @where@.
data TypeFamilyHead =
  TypeFamilyHead Name [TyVarBndr] FamilyResultSig (Maybe InjectivityAnn)
  deriving( Show, Eq, Ord, Data, Generic )

-- | One equation of a type family instance or closed type family. The
-- arguments are the left-hand-side type patterns and the right-hand-side
-- result.
data TySynEqn = TySynEqn [Type] Type
  deriving( Show, Eq, Ord, Data, Generic )

data FunDep = FunDep [Name] [Name]
  deriving( Show, Eq, Ord, Data, Generic )

data Foreign = ImportF Callconv Safety String Name Type
             | ExportF Callconv        String Name Type
         deriving( Show, Eq, Ord, Data, Generic )

-- keep Callconv in sync with module ForeignCall in ghc/compiler/prelude/ForeignCall.hs
data Callconv = CCall | StdCall | CApi | Prim | JavaScript
          deriving( Show, Eq, Ord, Data, Generic )

data Safety = Unsafe | Safe | Interruptible
        deriving( Show, Eq, Ord, Data, Generic )

data Pragma = InlineP         Name Inline RuleMatch Phases
            | SpecialiseP     Name Type (Maybe Inline) Phases
            | SpecialiseInstP Type
            | RuleP           String [RuleBndr] Exp Exp Phases
            | AnnP            AnnTarget Exp
            | LineP           Int String
            | CompleteP       [Name] (Maybe Name)
                -- ^ @{ {\-\# COMPLETE C_1, ..., C_i [ :: T ] \#-} }@
        deriving( Show, Eq, Ord, Data, Generic )

data Inline = NoInline
            | Inline
            | Inlinable
            deriving (Show, Eq, Ord, Data, Generic)

data RuleMatch = ConLike
               | FunLike
               deriving (Show, Eq, Ord, Data, Generic)

data Phases = AllPhases
            | FromPhase Int
            | BeforePhase Int
            deriving (Show, Eq, Ord, Data, Generic)

data RuleBndr = RuleVar Name
              | TypedRuleVar Name Type
              deriving (Show, Eq, Ord, Data, Generic)

data AnnTarget = ModuleAnnotation
               | TypeAnnotation Name
               | ValueAnnotation Name
              deriving (Show, Eq, Ord, Data, Generic)

type Cxt = [Pred]                 -- ^ @(Eq a, Ord b)@

-- | Since the advent of @ConstraintKinds@, constraints are really just types.
-- Equality constraints use the 'EqualityT' constructor. Constraints may also
-- be tuples of other constraints.
type Pred = Type

data SourceUnpackedness
  = NoSourceUnpackedness -- ^ @C a@
  | SourceNoUnpack       -- ^ @C { {\-\# NOUNPACK \#-\} } a@
  | SourceUnpack         -- ^ @C { {\-\# UNPACK \#-\} } a@
        deriving (Show, Eq, Ord, Data, Generic)

data SourceStrictness = NoSourceStrictness    -- ^ @C a@
                      | SourceLazy            -- ^ @C {~}a@
                      | SourceStrict          -- ^ @C {!}a@
        deriving (Show, Eq, Ord, Data, Generic)

-- | Unlike 'SourceStrictness' and 'SourceUnpackedness', 'DecidedStrictness'
-- refers to the strictness that the compiler chooses for a data constructor
-- field, which may be different from what is written in source code. See
-- 'reifyConStrictness' for more information.
data DecidedStrictness = DecidedLazy
                       | DecidedStrict
                       | DecidedUnpack
        deriving (Show, Eq, Ord, Data, Generic)

-- | A single data constructor.
--
-- The constructors for 'Con' can roughly be divided up into two categories:
-- those for constructors with \"vanilla\" syntax ('NormalC', 'RecC', and
-- 'InfixC'), and those for constructors with GADT syntax ('GadtC' and
-- 'RecGadtC'). The 'ForallC' constructor, which quantifies additional type
-- variables and class contexts, can surround either variety of constructor.
-- However, the type variables that it quantifies are different depending
-- on what constructor syntax is used:
--
-- * If a 'ForallC' surrounds a constructor with vanilla syntax, then the
--   'ForallC' will only quantify /existential/ type variables. For example:
--
--   @
--   data Foo a = forall b. MkFoo a b
--   @
--
--   In @MkFoo@, 'ForallC' will quantify @b@, but not @a@.
--
-- * If a 'ForallC' surrounds a constructor with GADT syntax, then the
--   'ForallC' will quantify /all/ type variables used in the constructor.
--   For example:
--
--   @
--   data Bar a b where
--     MkBar :: (a ~ b) => c -> MkBar a b
--   @
--
--   In @MkBar@, 'ForallC' will quantify @a@, @b@, and @c@.
data Con = NormalC Name [BangType]       -- ^ @C Int a@
         | RecC Name [VarBangType]       -- ^ @C { v :: Int, w :: a }@
         | InfixC BangType Name BangType -- ^ @Int :+ a@
         | ForallC [TyVarBndr] Cxt Con   -- ^ @forall a. Eq a => C [a]@
         | GadtC [Name] [BangType]
                 Type                    -- See Note [GADT return type]
                                         -- ^ @C :: a -> b -> T b Int@
         | RecGadtC [Name] [VarBangType]
                    Type                 -- See Note [GADT return type]
                                         -- ^ @C :: { v :: Int } -> T b Int@
        deriving (Show, Eq, Ord, Data, Generic)

-- Note [GADT return type]
-- ~~~~~~~~~~~~~~~~~~~~~~~
--
-- The return type of a GADT constructor does not necessarily match the name of
-- the data type:
--
-- type S = T
--
-- data T a where
--     MkT :: S Int
--
--
-- type S a = T
--
-- data T a where
--     MkT :: S Char Int
--
--
-- type Id a = a
-- type S a = T
--
-- data T a where
--     MkT :: Id (S Char Int)
--
--
-- That is why we allow the return type stored by a constructor to be an
-- arbitrary type. See also #11341

data Bang = Bang SourceUnpackedness SourceStrictness
         -- ^ @C { {\-\# UNPACK \#-\} !}a@
        deriving (Show, Eq, Ord, Data, Generic)

type BangType    = (Bang, Type)
type VarBangType = (Name, Bang, Type)

-- | As of @template-haskell-2.11.0.0@, 'Strict' has been replaced by 'Bang'.
type Strict      = Bang

-- | As of @template-haskell-2.11.0.0@, 'StrictType' has been replaced by
-- 'BangType'.
type StrictType    = BangType

-- | As of @template-haskell-2.11.0.0@, 'VarStrictType' has been replaced by
-- 'VarBangType'.
type VarStrictType = VarBangType

-- | A pattern synonym's directionality.
data PatSynDir
  = Unidir             -- ^ @pattern P x {<-} p@
  | ImplBidir          -- ^ @pattern P x {=} p@
  | ExplBidir [Clause] -- ^ @pattern P x {<-} p where P x = e@
  deriving( Show, Eq, Ord, Data, Generic )

-- | A pattern synonym's argument type.
data PatSynArgs
  = PrefixPatSyn [Name]        -- ^ @pattern P {x y z} = p@
  | InfixPatSyn Name Name      -- ^ @pattern {x P y} = p@
  | RecordPatSyn [Name]        -- ^ @pattern P { {x,y,z} } = p@
  deriving( Show, Eq, Ord, Data, Generic )

data Type = ForallT [TyVarBndr] Cxt Type  -- ^ @forall \<vars\>. \<ctxt\> => \<type\>@
          | AppT Type Type                -- ^ @T a b@
          | SigT Type Kind                -- ^ @t :: k@
          | VarT Name                     -- ^ @a@
          | ConT Name                     -- ^ @T@
          | PromotedT Name                -- ^ @'T@
          | InfixT Type Name Type         -- ^ @T + T@
          | UInfixT Type Name Type        -- ^ @T + T@
                                          --
                                          -- See "Language.Haskell.TH.Syntax#infix"
          | ParensT Type                  -- ^ @(T)@

          -- See Note [Representing concrete syntax in types]
          | TupleT Int                    -- ^ @(,), (,,), etc.@
          | UnboxedTupleT Int             -- ^ @(\#,\#), (\#,,\#), etc.@
          | UnboxedSumT SumArity          -- ^ @(\#|\#), (\#||\#), etc.@
          | ArrowT                        -- ^ @->@
          | EqualityT                     -- ^ @~@
          | ListT                         -- ^ @[]@
          | PromotedTupleT Int            -- ^ @'(), '(,), '(,,), etc.@
          | PromotedNilT                  -- ^ @'[]@
          | PromotedConsT                 -- ^ @(':)@
          | StarT                         -- ^ @*@
          | ConstraintT                   -- ^ @Constraint@
          | LitT TyLit                    -- ^ @0,1,2, etc.@
          | WildCardT                     -- ^ @_@
      deriving( Show, Eq, Ord, Data, Generic )

data TyVarBndr = PlainTV  Name            -- ^ @a@
               | KindedTV Name Kind       -- ^ @(a :: k)@
      deriving( Show, Eq, Ord, Data, Generic )

-- | Type family result signature
data FamilyResultSig = NoSig              -- ^ no signature
                     | KindSig  Kind      -- ^ @k@
                     | TyVarSig TyVarBndr -- ^ @= r, = (r :: k)@
      deriving( Show, Eq, Ord, Data, Generic )

-- | Injectivity annotation
data InjectivityAnn = InjectivityAnn Name [Name]
  deriving ( Show, Eq, Ord, Data, Generic )

data TyLit = NumTyLit Integer             -- ^ @2@
           | StrTyLit String              -- ^ @\"Hello\"@
  deriving ( Show, Eq, Ord, Data, Generic )

-- | Role annotations
data Role = NominalR            -- ^ @nominal@
          | RepresentationalR   -- ^ @representational@
          | PhantomR            -- ^ @phantom@
          | InferR              -- ^ @_@
  deriving( Show, Eq, Ord, Data, Generic )

-- | Annotation target for reifyAnnotations
data AnnLookup = AnnLookupModule Module
               | AnnLookupName Name
               deriving( Show, Eq, Ord, Data, Generic )

-- | To avoid duplication between kinds and types, they
-- are defined to be the same. Naturally, you would never
-- have a type be 'StarT' and you would never have a kind
-- be 'SigT', but many of the other constructors are shared.
-- Note that the kind @Bool@ is denoted with 'ConT', not
-- 'PromotedT'. Similarly, tuple kinds are made with 'TupleT',
-- not 'PromotedTupleT'.

type Kind = Type

{- Note [Representing concrete syntax in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Haskell has a rich concrete syntax for types, including
  t1 -> t2, (t1,t2), [t], and so on
In TH we represent all of this using AppT, with a distinguished
type constructor at the head.  So,
  Type              TH representation
  -----------------------------------------------
  t1 -> t2          ArrowT `AppT` t2 `AppT` t2
  [t]               ListT `AppT` t
  (t1,t2)           TupleT 2 `AppT` t1 `AppT` t2
  '(t1,t2)          PromotedTupleT 2 `AppT` t1 `AppT` t2

But if the original HsSyn used prefix application, we won't use
these special TH constructors.  For example
  [] t              ConT "[]" `AppT` t
  (->) t            ConT "->" `AppT` t
In this way we can faithfully represent in TH whether the original
HsType used concrete syntax or not.

The one case that doesn't fit this pattern is that of promoted lists
  '[ Maybe, IO ]    PromotedListT 2 `AppT` t1 `AppT` t2
but it's very smelly because there really is no type constructor
corresponding to PromotedListT. So we encode HsExplicitListTy with
PromotedConsT and PromotedNilT (which *do* have underlying type
constructors):
  '[ Maybe, IO ]    PromotedConsT `AppT` Maybe `AppT`
                    (PromotedConsT  `AppT` IO `AppT` PromotedNilT)
-}

-----------------------------------------------------
--              Internal helper functions
-----------------------------------------------------

cmpEq :: Ordering -> Bool
cmpEq EQ = True
cmpEq _  = False

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o2 = o2
thenCmp o1 _  = o1
