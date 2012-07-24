-- | Our extended FCode monad.

-- We add a mapping from names to CmmExpr, to support local variable names in
-- the concrete C-- code.  The unique supply of the underlying FCode monad
-- is used to grab a new unique for each local variable.

-- In C--, a local variable can be declared anywhere within a proc,
-- and it scopes from the beginning of the proc to the end.  Hence, we have
-- to collect declarations as we parse the proc, and feed the environment
-- back in circularly (to avoid a two-pass algorithm).

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module CgExtCode (
	ExtFCode(..),
	ExtCode,
	Named(..), Env,
	
	loopDecls,
	getEnv,

	newLocal,
	newLabel,
	newFunctionName,
	newImport,
	lookupLabel,
	lookupName,

	code,
	code2,
	nopEC,
	stmtEC,
	stmtsEC,
	getCgStmtsEC,
	getCgStmtsEC',
	forkLabelledCodeEC
)

where

import CgMonad

import CLabel
import OldCmm hiding( ClosureTypeInfo(..) )

-- import BasicTypes
import BlockId
import DynFlags
import FastString
import Module
import UniqFM
import Unique


-- | The environment contains variable definitions or blockids.
data Named 	
	= VarN CmmExpr		-- ^ Holds CmmLit(CmmLabel ..) which gives the label type,
				--	eg, RtsLabel, ForeignLabel, CmmLabel etc. 

	| FunN	 PackageId	-- ^ A function name from this package
	| LabelN BlockId		-- ^ A blockid of some code or data.
	
-- | An environment of named things.
type Env   	= UniqFM Named

-- | Local declarations that are in scope during code generation.
type Decls 	= [(FastString,Named)]

-- | Does a computation in the FCode monad, with a current environment
--	and a list of local declarations. Returns the resulting list of declarations.
newtype ExtFCode a 	
	= EC { unEC :: Env -> Decls -> FCode (Decls, a) }

type ExtCode = ExtFCode ()

returnExtFC :: a -> ExtFCode a
returnExtFC a 	= EC $ \_ s -> return (s, a)

thenExtFC :: ExtFCode a -> (a -> ExtFCode b) -> ExtFCode b
thenExtFC (EC m) k = EC $ \e s -> do (s',r) <- m e s; unEC (k r) e s'

instance Monad ExtFCode where
  (>>=) = thenExtFC
  return = returnExtFC

instance HasDynFlags ExtFCode where
    getDynFlags = EC (\_ d -> do dflags <- getDynFlags
                                 return (d, dflags))


-- | Takes the variable decarations and imports from the monad
-- 	and makes an environment, which is looped back into the computation.  
--	In this way, we can have embedded declarations that scope over the whole
-- 	procedure, and imports that scope over the entire module.
--	Discards the local declaration contained within decl'
--
loopDecls :: ExtFCode a -> ExtFCode a
loopDecls (EC fcode) =
      EC $ \e globalDecls -> do
	(_, a) <- fixC (\ ~(decls, _) -> fcode (addListToUFM e (decls ++ globalDecls)) globalDecls)
	return (globalDecls, a)


-- | Get the current environment from the monad.
getEnv :: ExtFCode Env
getEnv 	= EC $ \e s -> return (s, e)


-- | Add a new variable to the list of local declarations. 
--	The CmmExpr says where the value is stored. 
addVarDecl :: FastString -> CmmExpr -> ExtCode
addVarDecl var expr 
	= EC $ \_ s -> return ((var, VarN expr):s, ())

-- | Add a new label to the list of local declarations.
addLabel :: FastString -> BlockId -> ExtCode
addLabel name block_id 
	= EC $ \_ s -> return ((name, LabelN block_id):s, ())


-- | Create a fresh local variable of a given type.
newLocal 
	:: CmmType 		-- ^ data type
	-> FastString 		-- ^ name of variable
	-> ExtFCode LocalReg	-- ^ register holding the value
	
newLocal ty name = do
   u <- code newUnique
   let reg = LocalReg u ty
   addVarDecl name (CmmReg (CmmLocal reg))
   return reg


-- | Allocate a fresh label.
newLabel :: FastString -> ExtFCode BlockId
newLabel name = do
   u <- code newUnique
   addLabel name (mkBlockId u)
   return (mkBlockId u)


-- | Add add a local function to the environment.
newFunctionName 
	:: FastString	-- ^ name of the function 
	-> PackageId 	-- ^ package of the current module
	-> ExtCode
	
newFunctionName name pkg
	= EC $ \_ s -> return ((name, FunN pkg):s, ())
	
	
-- | Add an imported foreign label to the list of local declarations.
--	If this is done at the start of the module the declaration will scope
--	over the whole module.
newImport 
	:: (FastString, CLabel) 
	-> ExtFCode ()

newImport (name, cmmLabel) 
   = addVarDecl name (CmmLit (CmmLabel cmmLabel))


-- | Lookup the BlockId bound to the label with this name.
--	If one hasn't been bound yet, create a fresh one based on the 
--	Unique of the name.
lookupLabel :: FastString -> ExtFCode BlockId
lookupLabel name = do
  env <- getEnv
  return $ 
     case lookupUFM env name of
	Just (LabelN l) -> l
	_other 		-> mkBlockId (newTagUnique (getUnique name) 'L')


-- | Lookup the location of a named variable.
--	Unknown names are treated as if they had been 'import'ed from the runtime system.
-- 	This saves us a lot of bother in the RTS sources, at the expense of
-- 	deferring some errors to link time.
lookupName :: FastString -> ExtFCode CmmExpr
lookupName name = do
  env    <- getEnv
  return $ 
     case lookupUFM env name of
	Just (VarN e) 	-> e
	Just (FunN pkg)	-> CmmLit (CmmLabel (mkCmmCodeLabel pkg          name))
	_other 		-> CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId name))


-- | Lift an FCode computation into the ExtFCode monad
code :: FCode a -> ExtFCode a
code fc = EC $ \_ s -> do 
		r <- fc
 		return (s, r)


code2 :: (FCode (Decls,b) -> FCode ((Decls,b),c)) -> ExtFCode b -> ExtFCode c
code2 f (EC ec) 
	= EC $ \e s -> do 
		((s', _),c) <- f (ec e s)
		return (s',c)


-- | Do nothing in the ExtFCode monad.
nopEC :: ExtFCode ()
nopEC = code nopC


-- | Accumulate a CmmStmt into the monad state.
stmtEC :: CmmStmt -> ExtFCode () 
stmtEC stmt = code (stmtC stmt)


-- | Accumulate some CmmStmts into the monad state.
stmtsEC :: [CmmStmt] -> ExtFCode ()
stmtsEC stmts = code (stmtsC stmts)


-- | Get the generated statements out of the monad state.
getCgStmtsEC :: ExtFCode a -> ExtFCode CgStmts
getCgStmtsEC = code2 getCgStmts'


-- | Get the generated statements, and the return value out of the monad state.
getCgStmtsEC' :: ExtFCode a -> ExtFCode (a, CgStmts)
getCgStmtsEC' = code2 (\m -> getCgStmts' m >>= f)
  where f ((decl, b), c) = return ((decl, b), (b, c))


-- | Emit a chunk of code outside the instruction stream, 
--	and return its block id. 
forkLabelledCodeEC :: ExtFCode a -> ExtFCode BlockId
forkLabelledCodeEC ec = do
  stmts <- getCgStmtsEC ec
  code (forkCgStmts stmts)


