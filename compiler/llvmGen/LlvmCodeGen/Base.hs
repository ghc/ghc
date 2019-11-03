{-# LANGUAGE CPP #-}

-- ----------------------------------------------------------------------------
-- | Base LLVM Code Generation module
--
-- Contains functions useful through out the code generator.
--

module LlvmCodeGen.Base (

        LlvmCmmDecl, LlvmBasicBlock,
        LiveGlobalRegs,
        LlvmUnresData, LlvmData, UnresLabel, UnresStatic,

        LlvmVersion, supportedLlvmVersion, llvmVersionSupported, parseLlvmVersion,
        llvmVersionStr, llvmVersionList,

        LlvmM,
        runLlvm, liftStream, withClearVars, varLookup, varInsert,
        markStackReg, checkStackReg,
        funLookup, funInsert, getLlvmVer, getDynFlags, getDynFlag, getLlvmPlatform,
        dumpIfSetLlvm, renderLlvm, markUsedVar, getUsedVars,
        ghcInternalFunctions,

        getMetaUniqueId,
        setUniqMeta, getUniqMeta,

        cmmToLlvmType, widthToLlvmFloat, widthToLlvmInt, llvmFunTy,
        llvmFunSig, llvmFunArgs, llvmStdFunAttrs, llvmFunAlign, llvmInfAlign,
        llvmPtrBits, tysToParams, llvmFunSection,

        strCLabel_llvm, strDisplayName_llvm, strProcedureName_llvm,
        getGlobalPtr, generateExternDecls,

        aliasify,
    ) where

#include "HsVersions.h"
#include "ghcautoconf.h"

import GhcPrelude

import Llvm
import LlvmCodeGen.Regs

import CLabel
import CodeGen.Platform ( activeStgRegs )
import DynFlags
import FastString
import Cmm              hiding ( succ )
import Outputable as Outp
import Platform
import UniqFM
import Unique
import BufWrite   ( BufHandle )
import UniqSet
import UniqSupply
import ErrUtils
import qualified Stream

import Control.Monad (ap)
import Data.Char (isDigit)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE

-- ----------------------------------------------------------------------------
-- * Some Data Types
--

type LlvmCmmDecl = GenCmmDecl [LlvmData] (Maybe CmmStatics) (ListGraph LlvmStatement)
type LlvmBasicBlock = GenBasicBlock LlvmStatement

-- | Global registers live on proc entry
type LiveGlobalRegs = [GlobalReg]

-- | Unresolved code.
-- Of the form: (data label, data type, unresolved data)
type LlvmUnresData = (CLabel, Section, LlvmType, [UnresStatic])

-- | Top level LLVM Data (globals and type aliases)
type LlvmData = ([LMGlobal], [LlvmType])

-- | An unresolved Label.
--
-- Labels are unresolved when we haven't yet determined if they are defined in
-- the module we are currently compiling, or an external one.
type UnresLabel  = CmmLit
type UnresStatic = Either UnresLabel LlvmStatic

-- ----------------------------------------------------------------------------
-- * Type translations
--

-- | Translate a basic CmmType to an LlvmType.
cmmToLlvmType :: CmmType -> LlvmType
cmmToLlvmType ty | isVecType ty   = LMVector (vecLength ty) (cmmToLlvmType (vecElemType ty))
                 | isFloatType ty = widthToLlvmFloat $ typeWidth ty
                 | otherwise      = widthToLlvmInt   $ typeWidth ty

-- | Translate a Cmm Float Width to a LlvmType.
widthToLlvmFloat :: Width -> LlvmType
widthToLlvmFloat W32  = LMFloat
widthToLlvmFloat W64  = LMDouble
widthToLlvmFloat W80  = LMFloat80
widthToLlvmFloat W128 = LMFloat128
widthToLlvmFloat w    = panic $ "widthToLlvmFloat: Bad float size: " ++ show w

-- | Translate a Cmm Bit Width to a LlvmType.
widthToLlvmInt :: Width -> LlvmType
widthToLlvmInt w = LMInt $ widthInBits w

-- | GHC Call Convention for LLVM
llvmGhcCC :: DynFlags -> LlvmCallConvention
llvmGhcCC dflags
 | platformUnregisterised (targetPlatform dflags) = CC_Ccc
 | otherwise                                      = CC_Ghc

-- | Llvm Function type for Cmm function
llvmFunTy :: LiveGlobalRegs -> LlvmM LlvmType
llvmFunTy live = return . LMFunction =<< llvmFunSig' live (fsLit "a") ExternallyVisible

-- | Llvm Function signature
llvmFunSig :: LiveGlobalRegs ->  CLabel -> LlvmLinkageType -> LlvmM LlvmFunctionDecl
llvmFunSig live lbl link = do
  lbl' <- strCLabel_llvm lbl
  llvmFunSig' live lbl' link

llvmFunSig' :: LiveGlobalRegs -> LMString -> LlvmLinkageType -> LlvmM LlvmFunctionDecl
llvmFunSig' live lbl link
  = do let toParams x | isPointer x = (x, [NoAlias, NoCapture])
                      | otherwise   = (x, [])
       dflags <- getDynFlags
       return $ LlvmFunctionDecl lbl link (llvmGhcCC dflags) LMVoid FixedArgs
                                 (map (toParams . getVarType) (llvmFunArgs dflags live))
                                 (llvmFunAlign dflags)

-- | Alignment to use for functions
llvmFunAlign :: DynFlags -> LMAlign
llvmFunAlign dflags = Just (wORD_SIZE dflags)

-- | Alignment to use for into tables
llvmInfAlign :: DynFlags -> LMAlign
llvmInfAlign dflags = Just (wORD_SIZE dflags)

-- | Section to use for a function
llvmFunSection :: DynFlags -> LMString -> LMSection
llvmFunSection dflags lbl
    | gopt Opt_SplitSections dflags = Just (concatFS [fsLit ".text.", lbl])
    | otherwise                     = Nothing

-- | A Function's arguments
llvmFunArgs :: DynFlags -> LiveGlobalRegs -> [LlvmVar]
llvmFunArgs dflags live =
    map (lmGlobalRegArg dflags) (filter isPassed (activeStgRegs platform))
    where platform = targetPlatform dflags
          isLive r = not (isSSE r) || r `elem` alwaysLive || r `elem` live
          isPassed r = not (isSSE r) || isLive r
          isSSE (FloatReg _)  = True
          isSSE (DoubleReg _) = True
          isSSE (XmmReg _)    = True
          isSSE (YmmReg _)    = True
          isSSE (ZmmReg _)    = True
          isSSE _             = False

-- | Llvm standard fun attributes
llvmStdFunAttrs :: [LlvmFuncAttr]
llvmStdFunAttrs = [NoUnwind]

-- | Convert a list of types to a list of function parameters
-- (each with no parameter attributes)
tysToParams :: [LlvmType] -> [LlvmParameter]
tysToParams = map (\ty -> (ty, []))

-- | Pointer width
llvmPtrBits :: DynFlags -> Int
llvmPtrBits dflags = widthInBits $ typeWidth $ gcWord dflags

-- ----------------------------------------------------------------------------
-- * Llvm Version
--

-- Newtype to avoid using the Eq instance!
newtype LlvmVersion = LlvmVersion { llvmVersionNE :: NE.NonEmpty Int }

parseLlvmVersion :: String -> Maybe LlvmVersion
parseLlvmVersion =
    fmap LlvmVersion . NE.nonEmpty . go [] . dropWhile (not . isDigit)
  where
    go vs s
      | null ver_str
      = reverse vs
      | '.' : rest' <- rest
      = go (read ver_str : vs) rest'
      | otherwise
      = reverse (read ver_str : vs)
      where
        (ver_str, rest) = span isDigit s

-- | The LLVM Version that is currently supported.
supportedLlvmVersion :: LlvmVersion
supportedLlvmVersion = LlvmVersion (sUPPORTED_LLVM_VERSION NE.:| [])

llvmVersionSupported :: LlvmVersion -> Bool
llvmVersionSupported (LlvmVersion v) = NE.head v == sUPPORTED_LLVM_VERSION

llvmVersionStr :: LlvmVersion -> String
llvmVersionStr = intercalate "." . map show . llvmVersionList

llvmVersionList :: LlvmVersion -> [Int]
llvmVersionList = NE.toList . llvmVersionNE

-- ----------------------------------------------------------------------------
-- * Environment Handling
--

data LlvmEnv = LlvmEnv
  { envVersion :: LlvmVersion      -- ^ LLVM version
  , envDynFlags :: DynFlags        -- ^ Dynamic flags
  , envOutput :: BufHandle         -- ^ Output buffer
  , envUniq :: UniqSupply          -- ^ Supply of unique values
  , envFreshMeta :: MetaId         -- ^ Supply of fresh metadata IDs
  , envUniqMeta :: UniqFM MetaId   -- ^ Global metadata nodes
  , envFunMap :: LlvmEnvMap        -- ^ Global functions so far, with type
  , envAliases :: UniqSet LMString -- ^ Globals that we had to alias, see [Llvm Forward References]
  , envUsedVars :: [LlvmVar]       -- ^ Pointers to be added to llvm.used (see @cmmUsedLlvmGens@)

    -- the following get cleared for every function (see @withClearVars@)
  , envVarMap :: LlvmEnvMap        -- ^ Local variables so far, with type
  , envStackRegs :: [GlobalReg]    -- ^ Non-constant registers (alloca'd in the function prelude)
  }

type LlvmEnvMap = UniqFM LlvmType

-- | The Llvm monad. Wraps @LlvmEnv@ state as well as the @IO@ monad
newtype LlvmM a = LlvmM { runLlvmM :: LlvmEnv -> IO (a, LlvmEnv) }

instance Functor LlvmM where
    fmap f m = LlvmM $ \env -> do (x, env') <- runLlvmM m env
                                  return (f x, env')

instance Applicative LlvmM where
    pure x = LlvmM $ \env -> return (x, env)
    (<*>) = ap

instance Monad LlvmM where
    m >>= f  = LlvmM $ \env -> do (x, env') <- runLlvmM m env
                                  runLlvmM (f x) env'

instance HasDynFlags LlvmM where
    getDynFlags = LlvmM $ \env -> return (envDynFlags env, env)

instance MonadUnique LlvmM where
    getUniqueSupplyM = do
        us <- getEnv envUniq
        let (us1, us2) = splitUniqSupply us
        modifyEnv (\s -> s { envUniq = us2 })
        return us1

    getUniqueM = do
        us <- getEnv envUniq
        let (u,us') = takeUniqFromSupply us
        modifyEnv (\s -> s { envUniq = us' })
        return u

-- | Lifting of IO actions. Not exported, as we want to encapsulate IO.
liftIO :: IO a -> LlvmM a
liftIO m = LlvmM $ \env -> do x <- m
                              return (x, env)

-- | Get initial Llvm environment.
runLlvm :: DynFlags -> LlvmVersion -> BufHandle -> UniqSupply -> LlvmM () -> IO ()
runLlvm dflags ver out us m = do
    _ <- runLlvmM m env
    return ()
  where env = LlvmEnv { envFunMap = emptyUFM
                      , envVarMap = emptyUFM
                      , envStackRegs = []
                      , envUsedVars = []
                      , envAliases = emptyUniqSet
                      , envVersion = ver
                      , envDynFlags = dflags
                      , envOutput = out
                      , envUniq = us
                      , envFreshMeta = MetaId 0
                      , envUniqMeta = emptyUFM
                      }

-- | Get environment (internal)
getEnv :: (LlvmEnv -> a) -> LlvmM a
getEnv f = LlvmM (\env -> return (f env, env))

-- | Modify environment (internal)
modifyEnv :: (LlvmEnv -> LlvmEnv) -> LlvmM ()
modifyEnv f = LlvmM (\env -> return ((), f env))

-- | Lift a stream into the LlvmM monad
liftStream :: Stream.Stream IO a x -> Stream.Stream LlvmM a x
liftStream s = Stream.Stream $ do
  r <- liftIO $ Stream.runStream s
  case r of
    Left b        -> return (Left b)
    Right (a, r2) -> return (Right (a, liftStream r2))

-- | Clear variables from the environment for a subcomputation
withClearVars :: LlvmM a -> LlvmM a
withClearVars m = LlvmM $ \env -> do
    (x, env') <- runLlvmM m env { envVarMap = emptyUFM, envStackRegs = [] }
    return (x, env' { envVarMap = emptyUFM, envStackRegs = [] })

-- | Insert variables or functions into the environment.
varInsert, funInsert :: Uniquable key => key -> LlvmType -> LlvmM ()
varInsert s t = modifyEnv $ \env -> env { envVarMap = addToUFM (envVarMap env) s t }
funInsert s t = modifyEnv $ \env -> env { envFunMap = addToUFM (envFunMap env) s t }

-- | Lookup variables or functions in the environment.
varLookup, funLookup :: Uniquable key => key -> LlvmM (Maybe LlvmType)
varLookup s = getEnv (flip lookupUFM s . envVarMap)
funLookup s = getEnv (flip lookupUFM s . envFunMap)

-- | Set a register as allocated on the stack
markStackReg :: GlobalReg -> LlvmM ()
markStackReg r = modifyEnv $ \env -> env { envStackRegs = r : envStackRegs env }

-- | Check whether a register is allocated on the stack
checkStackReg :: GlobalReg -> LlvmM Bool
checkStackReg r = getEnv ((elem r) . envStackRegs)

-- | Allocate a new global unnamed metadata identifier
getMetaUniqueId :: LlvmM MetaId
getMetaUniqueId = LlvmM $ \env ->
    return (envFreshMeta env, env { envFreshMeta = succ $ envFreshMeta env })

-- | Get the LLVM version we are generating code for
getLlvmVer :: LlvmM LlvmVersion
getLlvmVer = getEnv envVersion

-- | Get the platform we are generating code for
getDynFlag :: (DynFlags -> a) -> LlvmM a
getDynFlag f = getEnv (f . envDynFlags)

-- | Get the platform we are generating code for
getLlvmPlatform :: LlvmM Platform
getLlvmPlatform = getDynFlag targetPlatform

-- | Dumps the document if the corresponding flag has been set by the user
dumpIfSetLlvm :: DumpFlag -> String -> Outp.SDoc -> LlvmM ()
dumpIfSetLlvm flag hdr doc = do
  dflags <- getDynFlags
  liftIO $ dumpIfSet_dyn dflags flag hdr doc

-- | Prints the given contents to the output handle
renderLlvm :: Outp.SDoc -> LlvmM ()
renderLlvm sdoc = do

    -- Write to output
    dflags <- getDynFlags
    out <- getEnv envOutput
    liftIO $ Outp.bufLeftRenderSDoc dflags out
               (Outp.mkCodeStyle Outp.CStyle) sdoc

    -- Dump, if requested
    dumpIfSetLlvm Opt_D_dump_llvm "LLVM Code" sdoc
    return ()

-- | Marks a variable as "used"
markUsedVar :: LlvmVar -> LlvmM ()
markUsedVar v = modifyEnv $ \env -> env { envUsedVars = v : envUsedVars env }

-- | Return all variables marked as "used" so far
getUsedVars :: LlvmM [LlvmVar]
getUsedVars = getEnv envUsedVars

-- | Saves that at some point we didn't know the type of the label and
-- generated a reference to a type variable instead
saveAlias :: LMString -> LlvmM ()
saveAlias lbl = modifyEnv $ \env -> env { envAliases = addOneToUniqSet (envAliases env) lbl }

-- | Sets metadata node for a given unique
setUniqMeta :: Unique -> MetaId -> LlvmM ()
setUniqMeta f m = modifyEnv $ \env -> env { envUniqMeta = addToUFM (envUniqMeta env) f m }

-- | Gets metadata node for given unique
getUniqMeta :: Unique -> LlvmM (Maybe MetaId)
getUniqMeta s = getEnv (flip lookupUFM s . envUniqMeta)

-- ----------------------------------------------------------------------------
-- * Internal functions
--

-- | Here we pre-initialise some functions that are used internally by GHC
-- so as to make sure they have the most general type in the case that
-- user code also uses these functions but with a different type than GHC
-- internally. (Main offender is treating return type as 'void' instead of
-- 'void *'). Fixes trac #5486.
ghcInternalFunctions :: LlvmM ()
ghcInternalFunctions = do
    dflags <- getDynFlags
    mk "memcpy" i8Ptr [i8Ptr, i8Ptr, llvmWord dflags]
    mk "memmove" i8Ptr [i8Ptr, i8Ptr, llvmWord dflags]
    mk "memset" i8Ptr [i8Ptr, llvmWord dflags, llvmWord dflags]
    mk "newSpark" (llvmWord dflags) [i8Ptr, i8Ptr]
  where
    mk n ret args = do
      let n' = fsLit n `appendFS` fsLit "$def"
          decl = LlvmFunctionDecl n' ExternallyVisible CC_Ccc ret
                                 FixedArgs (tysToParams args) Nothing
      renderLlvm $ ppLlvmFunctionDecl decl
      funInsert n' (LMFunction decl)

-- ----------------------------------------------------------------------------
-- * Label handling
--

-- | Pretty print a 'CLabel'.
strCLabel_llvm :: CLabel -> LlvmM LMString
strCLabel_llvm lbl = do
    platform <- getLlvmPlatform
    dflags <- getDynFlags
    let sdoc = pprCLabel platform lbl
        str = Outp.renderWithStyle dflags sdoc (Outp.mkCodeStyle Outp.CStyle)
    return (fsLit str)

strDisplayName_llvm :: CLabel -> LlvmM LMString
strDisplayName_llvm lbl = do
    platform <- getLlvmPlatform
    dflags <- getDynFlags
    let sdoc = pprCLabel platform lbl
        depth = Outp.PartWay 1
        style = Outp.mkUserStyle dflags Outp.reallyAlwaysQualify depth
        str = Outp.renderWithStyle dflags sdoc style
    return (fsLit (dropInfoSuffix str))

dropInfoSuffix :: String -> String
dropInfoSuffix = go
  where go "_info"        = []
        go "_static_info" = []
        go "_con_info"    = []
        go (x:xs)         = x:go xs
        go []             = []

strProcedureName_llvm :: CLabel -> LlvmM LMString
strProcedureName_llvm lbl = do
    platform <- getLlvmPlatform
    dflags <- getDynFlags
    let sdoc = pprCLabel platform lbl
        depth = Outp.PartWay 1
        style = Outp.mkUserStyle dflags Outp.neverQualify depth
        str = Outp.renderWithStyle dflags sdoc style
    return (fsLit str)

-- ----------------------------------------------------------------------------
-- * Global variables / forward references
--

-- | Create/get a pointer to a global value. Might return an alias if
-- the value in question hasn't been defined yet. We especially make
-- no guarantees on the type of the returned pointer.
getGlobalPtr :: LMString -> LlvmM LlvmVar
getGlobalPtr llvmLbl = do
  m_ty <- funLookup llvmLbl
  let mkGlbVar lbl ty = LMGlobalVar lbl (LMPointer ty) Private Nothing Nothing
  case m_ty of
    -- Directly reference if we have seen it already
    Just ty -> return $ mkGlbVar (llvmLbl `appendFS` fsLit "$def") ty Global
    -- Otherwise use a forward alias of it
    Nothing -> do
      saveAlias llvmLbl
      return $ mkGlbVar llvmLbl i8 Alias

-- | Generate definitions for aliases forward-referenced by @getGlobalPtr@.
--
-- Must be called at a point where we are sure that no new global definitions
-- will be generated anymore!
generateExternDecls :: LlvmM ([LMGlobal], [LlvmType])
generateExternDecls = do
  delayed <- fmap nonDetEltsUniqSet $ getEnv envAliases
  -- This is non-deterministic but we do not
  -- currently support deterministic code-generation.
  -- See Note [Unique Determinism and code generation]
  defss <- flip mapM delayed $ \lbl -> do
    m_ty <- funLookup lbl
    case m_ty of
      -- If we have a definition we've already emitted the proper aliases
      -- when the symbol itself was emitted by @aliasify@
      Just _ -> return []

      -- If we don't have a definition this is an external symbol and we
      -- need to emit a declaration
      Nothing ->
        let var = LMGlobalVar lbl i8Ptr External Nothing Nothing Global
        in return [LMGlobal var Nothing]

  -- Reset forward list
  modifyEnv $ \env -> env { envAliases = emptyUniqSet }
  return (concat defss, [])

-- | Here we take a global variable definition, rename it with a
-- @$def@ suffix, and generate the appropriate alias.
aliasify :: LMGlobal -> LlvmM [LMGlobal]
aliasify (LMGlobal var val) = do
    let LMGlobalVar lbl ty link sect align const = var

        defLbl = lbl `appendFS` fsLit "$def"
        defVar = LMGlobalVar defLbl ty Internal sect align const

        defPtrVar = LMGlobalVar defLbl (LMPointer ty) link Nothing Nothing const
        aliasVar = LMGlobalVar lbl i8Ptr link Nothing Nothing Alias
        aliasVal = LMBitc (LMStaticPointer defPtrVar) i8Ptr

    -- we need to mark the $def symbols as used so LLVM doesn't forget which
    -- section they need to go in. This will vanish once we switch away from
    -- mangling sections for TNTC.
    markUsedVar defVar

    return [ LMGlobal defVar val
           , LMGlobal aliasVar (Just aliasVal)
           ]

-- Note [Llvm Forward References]
--
-- The issue here is that LLVM insists on being strongly typed at
-- every corner, so the first time we mention something, we have to
-- settle what type we assign to it. That makes things awkward, as Cmm
-- will often reference things before their definition, and we have no
-- idea what (LLVM) type it is going to be before that point.
--
-- Our work-around is to define "aliases" of a standard type (i8 *) in
-- these kind of situations, which we later tell LLVM to be either
-- references to their actual local definitions (involving a cast) or
-- an external reference. This obviously only works for pointers.
--
-- In particular when we encounter a reference to a symbol in a chunk of
-- C-- there are three possible scenarios,
--
--   1. We have already seen a definition for the referenced symbol. This
--      means we already know its type.
--
--   2. We have not yet seen a definition but we will find one later in this
--      compilation unit. Since we want to be a good consumer of the
--      C-- streamed to us from upstream, we don't know the type of the
--      symbol at the time when we must emit the reference.
--
--   3. We have not yet seen a definition nor will we find one in this
--      compilation unit. In this case the reference refers to an
--      external symbol for which we do not know the type.
--
-- Let's consider case (2) for a moment: say we see a reference to
-- the symbol @fooBar@ for which we have not seen a definition. As we
-- do not know the symbol's type, we assume it is of type @i8*@ and emit
-- the appropriate casts in @getSymbolPtr@. Later on, when we
-- encounter the definition of @fooBar@ we emit it but with a modified
-- name, @fooBar$def@ (which we'll call the definition symbol), to
-- since we have already had to assume that the symbol @fooBar@
-- is of type @i8*@. We then emit @fooBar@ itself as an alias
-- of @fooBar$def@ with appropriate casts. This all happens in
-- @aliasify@.
--
-- Case (3) is quite similar to (2): References are emitted assuming
-- the referenced symbol is of type @i8*@. When we arrive at the end of
-- the compilation unit and realize that the symbol is external, we emit
-- an LLVM @external global@ declaration for the symbol @fooBar@
-- (handled in @generateExternDecls@). This takes advantage of the
-- fact that the aliases produced by @aliasify@ for exported symbols
-- have external linkage and can therefore be used as normal symbols.
--
-- Historical note: As of release 3.5 LLVM does not allow aliases to
-- refer to declarations. This the reason why aliases are produced at the
-- point of definition instead of the point of usage, as was previously
-- done. See #9142 for details.
--
-- Finally, case (1) is trival. As we already have a definition for
-- and therefore know the type of the referenced symbol, we can do
-- away with casting the alias to the desired type in @getSymbolPtr@
-- and instead just emit a reference to the definition symbol directly.
-- This is the @Just@ case in @getSymbolPtr@.
