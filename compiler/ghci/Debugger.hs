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

module Debugger (pprintClosureCommand, showTerm, pprTypeAndContents) where

import Linker
import RtClosureInspect

import HscTypes
import IdInfo
import Id
import Name
import Var hiding ( varName )
import VarSet
import Name 
import UniqSupply
import TcType
import GHC
import DynFlags
import InteractiveEval
import Outputable
import SrcLoc
import PprTyThing

import Exception
import Control.Monad
import Data.List
import Data.Maybe
import Data.IORef

import System.IO
import GHC.Exts

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
  docterms <- mapM (showTerm session) terms
  (printForUser stdout unqual . vcat)
        (zipWith (\id docterm -> ppr id <+> char '=' <+> docterm)
                 ids
                 docterms)
 where

   -- Do the obtainTerm--bindSuspensions-computeSubstitution dance
   go :: Session -> Id -> IO (Term, TvSubst)
   go cms id = do
       term_    <- GHC.obtainTerm cms force id
       term     <- tidyTermTyVars cms term_
       term'    <- if bindThings && 
                      False == isUnliftedTypeKind (termType term)
                     then bindSuspensions cms term
                     else return term
     -- Before leaving, we compare the type obtained to see if it's more specific
     --  Then, we extract a substitution,
     --  mapping the old tyvars to the reconstructed types.
       let reconstructed_type = termType term
       mb_subst <- withSession cms $ \hsc_env ->
                      improveRTTIType hsc_env (idType id) (reconstructed_type)
       return (term', fromMaybe emptyTvSubst mb_subst)

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
          availNames   = map ((prefix++) . show) [(1::Int)..] \\ alreadyUsedNames
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
                      , fNewtypeWrap  = 
                                \ty dc t -> do 
                                    (term, names) <- t
                                    return (NewtypeWrap ty dc term, names)
                      , fRefWrap = \ty t -> do
                                    (term, names) <- t 
                                    return (RefWrap ty term, names)
                      }
        doSuspension freeNames ct ty hval _name = do
          name <- atomicModifyIORef freeNames (\x->(tail x, head x))
          n <- newGrimName name
          return (Suspension ct ty hval (Just n), [(n,ty,hval)])


--  A custom Term printer to enable the use of Show instances
showTerm :: Session -> Term -> IO SDoc
showTerm cms@(Session ref) term = do
    dflags       <- GHC.getSessionDynFlags cms
    if dopt Opt_PrintEvldWithShow dflags
       then cPprTerm (liftM2 (++) (\_y->[cPprShowable]) cPprTermBase) term
       else cPprTerm cPprTermBase term
 where
  cPprShowable prec t@Term{ty=ty, val=val} =
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
  cPprShowable prec NewtypeWrap{ty=new_ty,wrapped_term=t} = 
      cPprShowable prec t{ty=new_ty}
  cPprShowable _ _ = return Nothing

  needsParens ('"':_) = False   -- some simple heuristics to see whether parens
                                -- are redundant in an arbitrary Show output
  needsParens ('(':_) = False
  needsParens txt = ' ' `elem` txt


  bindToFreshName hsc_env ty userName = do
    name <- newGrimName userName
    let ictxt    = hsc_IC hsc_env
        tmp_ids  = ic_tmp_ids ictxt
        id       = mkGlobalId VanillaGlobal name (sigmaType ty) vanillaIdInfo
        new_ic   = ictxt { ic_tmp_ids = id : tmp_ids }
    return (hsc_env {hsc_IC = new_ic }, name)

--    Create new uniques and give them sequentially numbered names
newGrimName :: String -> IO Name
newGrimName userName  = do
    us <- mkSplitUniqSupply 'b'
    let unique  = uniqFromSupply us
        occname = mkOccName varName userName
        name    = mkInternalName unique occname noSrcSpan
    return name

pprTypeAndContents :: Session -> [Id] -> IO SDoc
pprTypeAndContents session ids = do
  dflags  <- GHC.getSessionDynFlags session
  let pefas     = dopt Opt_PrintExplicitForalls dflags
      pcontents = dopt Opt_PrintBindContents dflags
  if pcontents 
    then do
      let depthBound = 100
      terms      <- mapM (GHC.obtainTermB session depthBound False) ids
      docs_terms <- mapM (showTerm session) terms
      return $ vcat $ zipWith (\ty cts -> ty <+> equals <+> cts)
                             (map (pprTyThing pefas . AnId) ids)
                             docs_terms
    else return $  vcat $ map (pprTyThing pefas . AnId) ids
