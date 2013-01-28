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

import GhcMonad
import HscTypes
import Id
import Name
import Var hiding ( varName )
import VarSet
import UniqSupply
import Type
import Kind
import GHC
import Outputable
import PprTyThing
import MonadUtils
import DynFlags
import Exception

import Control.Monad
import Data.List
import Data.Maybe
import Data.IORef

import GHC.Exts

-------------------------------------
-- | The :print & friends commands
-------------------------------------
pprintClosureCommand :: GhcMonad m => Bool -> Bool -> String -> m ()
pprintClosureCommand bindThings force str = do
  tythings <- (catMaybes . concat) `liftM`
                 mapM (\w -> GHC.parseName w >>=
                                mapM GHC.lookupName)
                      (words str)
  let ids = [id | AnId id <- tythings]

  -- Obtain the terms and the recovered type information
  (subst, terms) <- mapAccumLM go emptyTvSubst ids

  -- Apply the substitutions obtained after recovering the types
  modifySession $ \hsc_env ->
    hsc_env{hsc_IC = substInteractiveContext (hsc_IC hsc_env) subst}

  -- Finally, print the Terms
  unqual  <- GHC.getPrintUnqual
  docterms <- mapM showTerm terms
  dflags <- getDynFlags
  liftIO $ (printOutputForUser dflags unqual . vcat)
           (zipWith (\id docterm -> ppr id <+> char '=' <+> docterm)
                    ids
                    docterms)
 where
   -- Do the obtainTerm--bindSuspensions-computeSubstitution dance
   go :: GhcMonad m => TvSubst -> Id -> m (TvSubst, Term)
   go subst id = do
       let id' = id `setIdType` substTy subst (idType id) 
       term_    <- GHC.obtainTermFromId maxBound force id'
       term     <- tidyTermTyVars term_
       term'    <- if bindThings &&
                      False == isUnliftedTypeKind (termType term)
                     then bindSuspensions term
                     else return term
     -- Before leaving, we compare the type obtained to see if it's more specific
     --  Then, we extract a substitution,
     --  mapping the old tyvars to the reconstructed types.
       let reconstructed_type = termType term
       hsc_env <- getSession
       case (improveRTTIType hsc_env (idType id) (reconstructed_type)) of
         Nothing     -> return (subst, term')
         Just subst' -> do { traceOptIf Opt_D_dump_rtti
                               (fsep $ [text "RTTI Improvement for", ppr id,
                                text "is the substitution:" , ppr subst'])
                           ; return (subst `unionTvSubst` subst', term')}

   tidyTermTyVars :: GhcMonad m => Term -> m Term
   tidyTermTyVars t =
     withSession $ \hsc_env -> do
     let env_tvs      = tyThingsTyVars $ ic_tythings $ hsc_IC hsc_env
         my_tvs       = termTyVars t
         tvs          = env_tvs `minusVarSet` my_tvs
         tyvarOccName = nameOccName . tyVarName
         tidyEnv      = (initTidyOccEnv (map tyvarOccName (varSetElems tvs))
                        , env_tvs `intersectVarSet` my_tvs)
     return$ mapTermType (snd . tidyOpenType tidyEnv) t

-- | Give names, and bind in the interactive environment, to all the suspensions
--   included (inductively) in a term
bindSuspensions :: GhcMonad m => Term -> m Term
bindSuspensions t = do
      hsc_env <- getSession
      inScope <- GHC.getBindings
      let ictxt        = hsc_IC hsc_env
          prefix       = "_t"
          alreadyUsedNames = map (occNameString . nameOccName . getName) inScope
          availNames   = map ((prefix++) . show) [(1::Int)..] \\ alreadyUsedNames
      availNames_var  <- liftIO $ newIORef availNames
      (t', stuff)     <- liftIO $ foldTerm (nameSuspensionsAndGetInfos availNames_var) t
      let (names, tys, hvals) = unzip3 stuff
      let ids = [ mkVanillaGlobal name ty 
                | (name,ty) <- zip names tys]
          new_ic = extendInteractiveContext ictxt (map AnId ids)
      liftIO $ extendLinkEnv (zip names hvals)
      modifySession $ \_ -> hsc_env {hsc_IC = new_ic }
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
showTerm :: GhcMonad m => Term -> m SDoc
showTerm term = do
    dflags       <- GHC.getSessionDynFlags
    if gopt Opt_PrintEvldWithShow dflags
       then cPprTerm (liftM2 (++) (\_y->[cPprShowable]) cPprTermBase) term
       else cPprTerm cPprTermBase term
 where
  cPprShowable prec t@Term{ty=ty, val=val} =
    if not (isFullyEvaluatedTerm t)
     then return Nothing
     else do
        hsc_env <- getSession
        dflags  <- GHC.getSessionDynFlags
        do
           (new_env, bname) <- bindToFreshName hsc_env ty "showme"
           setSession new_env
                      -- XXX: this tries to disable logging of errors
                      -- does this still do what it is intended to do
                      -- with the changed error handling and logging?
           let noop_log _ _ _ _ _ = return ()
               expr = "show " ++ showPpr dflags bname
           _ <- GHC.setSessionDynFlags dflags{log_action=noop_log}
           txt_ <- withExtendedLinkEnv [(bname, val)]
                                       (GHC.compileExpr expr)
           let myprec = 10 -- application precedence. TODO Infix constructors
           let txt = unsafeCoerce# txt_
           if not (null txt) then
             return $ Just $ cparen (prec >= myprec && needsParens txt)
                                    (text txt)
            else return Nothing
         `gfinally` do
           setSession hsc_env
           GHC.setSessionDynFlags dflags
  cPprShowable prec NewtypeWrap{ty=new_ty,wrapped_term=t} = 
      cPprShowable prec t{ty=new_ty}
  cPprShowable _ _ = return Nothing

  needsParens ('"':_) = False   -- some simple heuristics to see whether parens
                                -- are redundant in an arbitrary Show output
  needsParens ('(':_) = False
  needsParens txt = ' ' `elem` txt


  bindToFreshName hsc_env ty userName = do
    name <- newGrimName userName
    let id       = AnId $ mkVanillaGlobal name ty 
        new_ic   = extendInteractiveContext (hsc_IC hsc_env) [id]
    return (hsc_env {hsc_IC = new_ic }, name)

--    Create new uniques and give them sequentially numbered names
newGrimName :: MonadIO m => String -> m Name
newGrimName userName  = do
    us <- liftIO $ mkSplitUniqSupply 'b'
    let unique  = uniqFromSupply us
        occname = mkOccName varName userName
        name    = mkInternalName unique occname noSrcSpan
    return name

pprTypeAndContents :: GhcMonad m => Id -> m SDoc
pprTypeAndContents id = do
  dflags  <- GHC.getSessionDynFlags
  let pefas     = gopt Opt_PrintExplicitForalls dflags
      pcontents = gopt Opt_PrintBindContents dflags
      pprdId    = (PprTyThing.pprTyThing pefas . AnId) id
  if pcontents 
    then do
      let depthBound = 100
      -- If the value is an exception, make sure we catch it and
      -- show the exception, rather than propagating the exception out.
      e_term <- gtry $ GHC.obtainTermFromId depthBound False id
      docs_term <- case e_term of
                      Right term -> showTerm term
                      Left  exn  -> return (text "*** Exception:" <+>
                                            text (show (exn :: SomeException)))
      return $ pprdId <+> equals <+> docs_term
    else return pprdId

--------------------------------------------------------------
-- Utils 

traceOptIf :: GhcMonad m => DumpFlag -> SDoc -> m ()
traceOptIf flag doc = do
  dflags <- GHC.getSessionDynFlags
  when (dopt flag dflags) $ liftIO $ printInfoForUser dflags alwaysQualify doc
