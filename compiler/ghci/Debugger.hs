-----------------------------------------------------------------------------
--
-- GHCi Interactive debugging commands 
--
-- Pepe Iborra (supported by Google SoC) 2006
--
-- ToDo: lots of violation of layering here.  This module should
-- decide whether it is above the GHC API (import GHC and nothing
-- else) or below it.
-- 
-----------------------------------------------------------------------------

{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module Debugger (pprintClosureCommand, showTerm) where

import Linker
import RtClosureInspect

import HscTypes
import IdInfo
--import Id
import Name
import Var hiding ( varName )
import VarSet
import Name 
import UniqSupply
import TcType
import GHC
import InteractiveEval
import Outputable
import Pretty                    ( Mode(..), showDocWith )
import SrcLoc

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Data.IORef

import System.IO
import GHC.Exts

#include "HsVersions.h"
-------------------------------------
-- | The :print & friends commands
-------------------------------------
pprintClosureCommand :: Session -> Bool -> Bool -> String -> IO ()
pprintClosureCommand session bindThings force str = do
  tythings <- (catMaybes . concat) `liftM`
                 mapM (\w -> GHC.parseName session w >>=
                                mapM (GHC.lookupName session))
                      (words str)
  let ids = [id | AnId id <- tythings]

  -- Obtain the terms and the recovered type information
  (terms, substs) <- unzip `liftM` mapM (go session) ids
  
  -- Apply the substitutions obtained after recovering the types
  modifySession session $ \hsc_env ->
         hsc_env{hsc_IC = foldr (flip substInteractiveContext)
                                (hsc_IC hsc_env)
                                (map skolemiseSubst substs)}
  -- Finally, print the Terms
  unqual  <- GHC.getPrintUnqual session
  let showSDocForUserOneLine unqual doc =
               showDocWith LeftMode (doc (mkErrStyle unqual))
  docterms <- mapM (showTerm session) terms
  (putStrLn . showSDocForUserOneLine unqual . vcat)
        (zipWith (\id docterm -> ppr id <+> char '=' <+> docterm)
                 ids
                 docterms)
 where

   -- Do the obtainTerm--bindSuspensions-computeSubstitution dance
   go :: Session -> Id -> IO (Term, TvSubst)
   go cms id = do
       term_    <- GHC.obtainTerm cms force id
       term     <- tidyTermTyVars cms term_
       term'    <- if not bindThings then return term
                     else bindSuspensions cms term                       
     -- Before leaving, we compare the type obtained to see if it's more specific
     --  Then, we extract a substitution,
     --  mapping the old tyvars to the reconstructed types.
       let Just reconstructed_type = termType term
           Just subst = computeRTTIsubst (idType id) (reconstructed_type)
       return (term',subst)

   tidyTermTyVars :: Session -> Term -> IO Term
   tidyTermTyVars (Session ref) t = do
     hsc_env <- readIORef ref
     let env_tvs      = ic_tyvars (hsc_IC hsc_env)
         my_tvs       = termTyVars t
         tvs          = env_tvs `minusVarSet` my_tvs
         tyvarOccName = nameOccName . tyVarName
         tidyEnv      = (initTidyOccEnv (map tyvarOccName (varSetElems tvs))
                        , env_tvs `intersectVarSet` my_tvs)
     return$ mapTermType (snd . tidyOpenType tidyEnv) t

-- | Give names, and bind in the interactive environment, to all the suspensions
--   included (inductively) in a term
bindSuspensions :: Session -> Term -> IO Term
bindSuspensions cms@(Session ref) t = do
      hsc_env <- readIORef ref
      inScope <- GHC.getBindings cms
      let ictxt        = hsc_IC hsc_env
          prefix       = "_t"
          alreadyUsedNames = map (occNameString . nameOccName . getName) inScope
          availNames   = map ((prefix++) . show) [1..] \\ alreadyUsedNames
      availNames_var  <- newIORef availNames
      (t', stuff)     <- foldTerm (nameSuspensionsAndGetInfos availNames_var) t
      let (names, tys, hvals) = unzip3 stuff
      let tys' = map (fst.skolemiseTy) tys
      let ids = [ mkGlobalId VanillaGlobal name ty vanillaIdInfo
                | (name,ty) <- zip names tys']
          new_tyvars   = tyVarsOfTypes tys'
          new_ic       = extendInteractiveContext ictxt ids new_tyvars
      extendLinkEnv (zip names hvals)
      writeIORef ref (hsc_env {hsc_IC = new_ic })
      return t'
     where

--    Processing suspensions. Give names and recopilate info
        nameSuspensionsAndGetInfos :: IORef [String] ->
                                       TermFold (IO (Term, [(Name,Type,HValue)]))
        nameSuspensionsAndGetInfos freeNames = TermFold
                      {
                        fSuspension = doSuspension freeNames
                      , fTerm = \ty dc v tt -> do
                                    tt' <- sequence tt
                                    let (terms,names) = unzip tt'
                                    return (Term ty dc v terms, concat names)
                      , fPrim    = \ty n ->return (Prim ty n,[])
                      }
        doSuspension freeNames ct mb_ty hval Nothing = do
          name <- atomicModifyIORef freeNames (\x->(tail x, head x))
          n <- newGrimName cms name
          let ty' = fromMaybe (error "unexpected") mb_ty
          return (Suspension ct mb_ty hval (Just n), [(n,ty',hval)])


--  A custom Term printer to enable the use of Show instances
showTerm cms@(Session ref) = cPprTerm cPpr
 where
  cPpr = \p-> cPprShowable : cPprTermBase p
  cPprShowable prec t@Term{ty=ty, dc=dc, val=val} = 
    if not (isFullyEvaluatedTerm t)
     then return Nothing
     else do
        hsc_env <- readIORef ref
        dflags  <- GHC.getSessionDynFlags cms
        do
           (new_env, bname) <- bindToFreshName hsc_env ty "showme"
           writeIORef ref (new_env)
           let noop_log _ _ _ _ = return ()
               expr = "show " ++ showSDoc (ppr bname)
           GHC.setSessionDynFlags cms dflags{log_action=noop_log}
           mb_txt <- withExtendedLinkEnv [(bname, val)]
                                         (GHC.compileExpr cms expr)
           let myprec = 10 -- application precedence. TODO Infix constructors
           case mb_txt of
             Just txt_ | txt <- unsafeCoerce# txt_, not (null txt)
                       -> return $ Just$ cparen (prec >= myprec &&
                                                      needsParens txt)
                                                (text txt)
             _  -> return Nothing
         `finally` do
           writeIORef ref hsc_env
           GHC.setSessionDynFlags cms dflags
  needsParens ('"':txt) = False -- some simple heuristics to see whether parens
                                -- are redundant in an arbitrary Show output
  needsParens ('(':txt) = False
  needsParens txt = ' ' `elem` txt


  bindToFreshName hsc_env ty userName = do
    name <- newGrimName cms userName
    let ictxt    = hsc_IC hsc_env
        tmp_ids  = ic_tmp_ids ictxt
        id       = mkGlobalId VanillaGlobal name (sigmaType ty) vanillaIdInfo
        new_ic   = ictxt { ic_tmp_ids = id : tmp_ids }
    return (hsc_env {hsc_IC = new_ic }, name)

--    Create new uniques and give them sequentially numbered names
--    newGrimName :: Session -> String -> IO Name
newGrimName cms userName  = do
    us <- mkSplitUniqSupply 'b'
    let unique  = uniqFromSupply us
        occname = mkOccName varName userName
        name    = mkInternalName unique occname noSrcSpan
    return name
