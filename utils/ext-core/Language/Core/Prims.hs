{-# OPTIONS -Wall #-}

{- This module contains a few primitive types that need to be wired in.
   Most are defined in PrimEnv, which is automatically generated from
   GHC's primops.txt. -}

module Language.Core.Prims(initialEnv, primEnv, primId, bv,
             tIntzh, tInt64zh, tCharzh, tFloatzh, tAddrzh, tDoublezh, tcStatezh,
             tWordzh, tWord64zh, tByteArrayzh,
             tcStablePtrzh, tcIO, mkInitialEnv, mkTypeEnv, tRWS, tBool, tcBool,
             ioBaseMname) where

import Control.Monad

import Language.Core.Core
import Language.Core.Encoding
import Language.Core.Env
import Language.Core.Check
import Language.Core.PrimCoercions
import Language.Core.PrimEnv

initialEnv :: Menv
initialEnv = efromlist [(primMname,primEnv),
		     (errMname,errorEnv),
                     (boolMname,boolEnv)]

primEnv :: Envs
-- Tediously, we add defs for ByteArray# etc. because these are
-- declared as ByteArr# (etc.) in primops.txt, and GHC has
-- ByteArray# etc. wired-in.
-- At least this is better than when all primops were wired-in here.
primEnv = Envs {tcenv_=efromlist $ map (\ (t,k) -> (t,Kind k)) $ 
                  [(snd tcByteArrayzh,ktByteArrayzh), 
                   (snd tcMutableArrayzh, ktMutableArrayzh),
                   (snd tcMutableByteArrayzh, ktMutableByteArrayzh)] ++
                 ([(snd $ tcUtuple n, ktUtuple n) | n <- [1..maxUtuple]] 
                   ++ ((snd tcArrow,ktArrow):primTcs)),
		cenv_=efromlist primDcs,
		venv_=efromlist (opsState ++ primVals)}

errorEnv :: Envs
errorEnv = Envs {tcenv_=eempty,
		 cenv_=eempty,
		 venv_=efromlist errorVals}

-- Unpleasantly, we wire in the Bool type because some people
-- (i.e. me) need to depend on it being primitive. This shouldn't
-- hurt anything, since if someone pulls in the GHC.Bool module,
-- it will override this definition.
boolEnv :: Envs
boolEnv = Envs {tcenv_=efromlist boolTcs,
                cenv_=efromlist boolDcs,
                venv_=eempty}

boolTcs :: [(Tcon, KindOrCoercion)]
boolTcs = [(snd tcBool, Kind Klifted)]
            
boolDcs :: [(Dcon, Ty)]
boolDcs = [(dcTrue, tBool),
           (dcFalse, tBool)]

primDcs :: [(Dcon,Ty)]
primDcs = map (\ ((_,c),t) -> (c,t))
	      [(dcUtuple n, dcUtupleTy n) | n <- [1..maxUtuple]]

tRWS :: Ty
tRWS = tStatezh tRealWorld

opsState :: [(Var, Ty)]
opsState = [
  ("realWorldzh", tRWS)]

{- Arrays -}

tcByteArrayzh, tcMutableArrayzh, tcMutableByteArrayzh :: Qual Tcon
ktByteArrayzh, ktMutableArrayzh, ktMutableByteArrayzh :: Kind

tcByteArrayzh = pvz "ByteArray"
ktByteArrayzh = Kunlifted

tcMutableArrayzh = pvz "MutableArray"
ktMutableArrayzh = Karrow Klifted (Karrow Klifted Kunlifted)

tcMutableByteArrayzh = pvz "MutableByteArray"
ktMutableByteArrayzh = Karrow Klifted Kunlifted

{- Real world and state. -}

-- tjc: why isn't this one unboxed?
tcRealWorld :: Qual Tcon
tcRealWorld = pv "RealWorld"
tRealWorld :: Ty
tRealWorld = Tcon tcRealWorld

tcStatezh :: Qual Tcon
tcStatezh = pvz "State"
tStatezh :: Ty -> Ty
tStatezh t = Tapp (Tcon tcStatezh) t

{- Properly defined in PrelError, but needed in many modules before that. -}
errorVals :: [(Var, Ty)]
errorVals = []
{-
 [
 ("error", Tforall ("a",Kopen) (tArrow tString (Tvar "a"))),
 ("irrefutPatError", str2A),
 ("patError", str2A),
 ("divZZeroError", forallAA),
 ("overflowError", forallAA)]
-}

{- Non-primitive, but mentioned in the types of primitives. -}

bv :: a -> Qual a
bv = qual baseMname

str2A, forallAA :: Ty  
str2A = Tforall ("a",Kopen) (tArrow tAddrzh (Tvar "a"))
forallAA = Tforall ("a",Kopen) (Tvar "a")

tBool :: Ty
tBool = Tcon tcBool
tcBool :: Qual Tcon
tcBool = (Just boolMname, "Bool")
tcChar :: Qual Tcon
tcChar = bv "Char"
tChar :: Ty
tChar = Tcon tcChar
tcList :: Qual Tcon
tcList = bv "ZMZN"
tList :: Ty -> Ty
tList t = Tapp (Tcon tcList) t
tString :: Ty
tString = tList tChar
tIntzh, tInt64zh, tWordzh, tWord64zh, tCharzh, tFloatzh, tDoublezh, {-tIOUnit,-} 
  tByteArrayzh :: Ty
tIntzh = Tcon (primId "Int#")
tInt64zh = Tcon (primId "Int64#")
tWordzh = Tcon (primId "Word#")
tWord64zh = Tcon (primId "Word64#")
tByteArrayzh = Tcon (primId "ByteArray#")
tCharzh = Tcon (primId "Char#")
tFloatzh = Tcon (primId "Float#")
tDoublezh = Tcon (primId "Double#")
tcStablePtrzh, tcIO :: Qual Tcon
tcStablePtrzh = pvz "StablePtr"
tcIO = (Just (mkBaseMname "IOBase"), "IO")

primId :: String -> Qual Id
primId = pv . zEncodeString

--- doesn't really belong here... sigh

mkInitialEnv :: [Module] -> IO Menv
mkInitialEnv libs = foldM mkTypeEnv initialEnv libs
                    
mkTypeEnv :: Menv -> Module -> IO Menv
mkTypeEnv globalEnv m@(Module mn _ _) = 
    catch (return (envsModule globalEnv m)) handler
        where handler e = do
                putStrLn ("WARNING: mkTypeEnv caught an exception " ++ show e 
                                    ++ " while processing " ++ show mn)
                return globalEnv

----- move this 
ioBaseMname :: AnMname
ioBaseMname = mkBaseMname "IOBase"
