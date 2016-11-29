-- | Our extended FCode monad.

-- We add a mapping from names to CmmExpr, to support local variable names in
-- the concrete C-- code.  The unique supply of the underlying FCode monad
-- is used to grab a new unique for each local variable.

-- In C--, a local variable can be declared anywhere within a proc,
-- and it scopes from the beginning of the proc to the end.  Hence, we have
-- to collect declarations as we parse the proc, and feed the environment
-- back in circularly (to avoid a two-pass algorithm).

module StgCmmExtCode (
        CmmParse, unEC,
        Named(..), Env,

        loopDecls,
        getEnv,

        withName,
        getName,

        newLocal,
        newLabel,
        newBlockId,
        newFunctionName,
        newImport,
        lookupLabel,
        lookupName,

        code,
        emit, emitLabel, emitAssign, emitStore,
        getCode, getCodeR, getCodeScoped,
        emitOutOfLine,
        withUpdFrameOff, getUpdFrameOff
)

where

import qualified StgCmmMonad as F
import StgCmmMonad (FCode, newUnique)

import Cmm
import CLabel
import MkGraph

import BlockId
import DynFlags
import FastString
import Module
import UniqFM
import Unique
import UniqSupply

import Control.Monad (liftM, ap)

-- | The environment contains variable definitions or blockids.
data Named
        = VarN CmmExpr          -- ^ Holds CmmLit(CmmLabel ..) which gives the label type,
                                --      eg, RtsLabel, ForeignLabel, CmmLabel etc.

        | FunN   UnitId      -- ^ A function name from this package
        | LabelN BlockId                -- ^ A blockid of some code or data.

-- | An environment of named things.
type Env        = UniqFM Named

-- | Local declarations that are in scope during code generation.
type Decls      = [(FastString,Named)]

-- | Does a computation in the FCode monad, with a current environment
--      and a list of local declarations. Returns the resulting list of declarations.
newtype CmmParse a
        = EC { unEC :: String -> Env -> Decls -> FCode (Decls, a) }

type ExtCode = CmmParse ()

returnExtFC :: a -> CmmParse a
returnExtFC a   = EC $ \_ _ s -> return (s, a)

thenExtFC :: CmmParse a -> (a -> CmmParse b) -> CmmParse b
thenExtFC (EC m) k = EC $ \c e s -> do (s',r) <- m c e s; unEC (k r) c e s'

instance Functor CmmParse where
      fmap = liftM

instance Applicative CmmParse where
      pure = returnExtFC
      (<*>) = ap

instance Monad CmmParse where
  (>>=) = thenExtFC

instance MonadUnique CmmParse where
  getUniqueSupplyM = code getUniqueSupplyM
  getUniqueM = EC $ \_ _ decls -> do
    u <- getUniqueM
    return (decls, u)

instance HasDynFlags CmmParse where
    getDynFlags = EC (\_ _ d -> do dflags <- getDynFlags
                                   return (d, dflags))


-- | Takes the variable decarations and imports from the monad
--      and makes an environment, which is looped back into the computation.
--      In this way, we can have embedded declarations that scope over the whole
--      procedure, and imports that scope over the entire module.
--      Discards the local declaration contained within decl'
--
loopDecls :: CmmParse a -> CmmParse a
loopDecls (EC fcode) =
      EC $ \c e globalDecls -> do
        (_, a) <- F.fixC $ \ ~(decls, _) ->
          fcode c (addListToUFM e decls) globalDecls
        return (globalDecls, a)


-- | Get the current environment from the monad.
getEnv :: CmmParse Env
getEnv  = EC $ \_ e s -> return (s, e)

-- | Get the current context name from the monad
getName :: CmmParse String
getName  = EC $ \c _ s -> return (s, c)

-- | Set context name for a sub-parse
withName :: String -> CmmParse a -> CmmParse a
withName c' (EC fcode) = EC $ \_ e s -> fcode c' e s

addDecl :: FastString -> Named -> ExtCode
addDecl name named = EC $ \_ _ s -> return ((name, named) : s, ())


-- | Add a new variable to the list of local declarations.
--      The CmmExpr says where the value is stored.
addVarDecl :: FastString -> CmmExpr -> ExtCode
addVarDecl var expr = addDecl var (VarN expr)

-- | Add a new label to the list of local declarations.
addLabel :: FastString -> BlockId -> ExtCode
addLabel name block_id = addDecl name (LabelN block_id)


-- | Create a fresh local variable of a given type.
newLocal
        :: CmmType              -- ^ data type
        -> FastString           -- ^ name of variable
        -> CmmParse LocalReg    -- ^ register holding the value

newLocal ty name = do
   u <- code newUnique
   let reg = LocalReg u ty
   addVarDecl name (CmmReg (CmmLocal reg))
   return reg


-- | Allocate a fresh label.
newLabel :: FastString -> CmmParse BlockId
newLabel name = do
   u <- code newUnique
   addLabel name (mkBlockId u)
   return (mkBlockId u)

-- | Add add a local function to the environment.
newFunctionName
        :: FastString   -- ^ name of the function
        -> UnitId    -- ^ package of the current module
        -> ExtCode

newFunctionName name pkg = addDecl name (FunN pkg)


-- | Add an imported foreign label to the list of local declarations.
--      If this is done at the start of the module the declaration will scope
--      over the whole module.
newImport
        :: (FastString, CLabel)
        -> CmmParse ()

newImport (name, cmmLabel)
   = addVarDecl name (CmmLit (CmmLabel cmmLabel))


-- | Lookup the BlockId bound to the label with this name.
--      If one hasn't been bound yet, create a fresh one based on the
--      Unique of the name.
lookupLabel :: FastString -> CmmParse BlockId
lookupLabel name = do
  env <- getEnv
  return $
     case lookupUFM env name of
        Just (LabelN l) -> l
        _other          -> mkBlockId (newTagUnique (getUnique name) 'L')


-- | Lookup the location of a named variable.
--      Unknown names are treated as if they had been 'import'ed from the runtime system.
--      This saves us a lot of bother in the RTS sources, at the expense of
--      deferring some errors to link time.
lookupName :: FastString -> CmmParse CmmExpr
lookupName name = do
  env    <- getEnv
  return $
     case lookupUFM env name of
        Just (VarN e)   -> e
        Just (FunN pkg) -> CmmLit (CmmLabel (mkCmmCodeLabel pkg          name))
        _other          -> CmmLit (CmmLabel (mkCmmCodeLabel rtsUnitId name))


-- | Lift an FCode computation into the CmmParse monad
code :: FCode a -> CmmParse a
code fc = EC $ \_ _ s -> do
                r <- fc
                return (s, r)

emit :: CmmAGraph -> CmmParse ()
emit = code . F.emit

emitLabel :: BlockId -> CmmParse ()
emitLabel = code . F.emitLabel

emitAssign :: CmmReg  -> CmmExpr -> CmmParse ()
emitAssign l r = code (F.emitAssign l r)

emitStore :: CmmExpr  -> CmmExpr -> CmmParse ()
emitStore l r = code (F.emitStore l r)

getCode :: CmmParse a -> CmmParse CmmAGraph
getCode (EC ec) = EC $ \c e s -> do
  ((s',_), gr) <- F.getCodeR (ec c e s)
  return (s', gr)

getCodeR :: CmmParse a -> CmmParse (a, CmmAGraph)
getCodeR (EC ec) = EC $ \c e s -> do
  ((s', r), gr) <- F.getCodeR (ec c e s)
  return (s', (r,gr))

getCodeScoped :: CmmParse a -> CmmParse (a, CmmAGraphScoped)
getCodeScoped (EC ec) = EC $ \c e s -> do
  ((s', r), gr) <- F.getCodeScoped (ec c e s)
  return (s', (r,gr))

emitOutOfLine :: BlockId -> CmmAGraphScoped -> CmmParse ()
emitOutOfLine l g = code (F.emitOutOfLine l g)

withUpdFrameOff :: UpdFrameOffset -> CmmParse () -> CmmParse ()
withUpdFrameOff size inner
  = EC $ \c e s -> F.withUpdFrameOff size $ (unEC inner) c e s

getUpdFrameOff :: CmmParse UpdFrameOffset
getUpdFrameOff = code $ F.getUpdFrameOff
