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

module GHC.Runtime.Debugger (pprintClosureCommand, showTerm, pprTypeAndContents) where

import GHC.Prelude

import GHC

import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Driver.Monad
import GHC.Driver.Env

import GHC.Linker.Loader

import GHC.Runtime.Heap.Inspect
import GHC.Runtime.Interpreter
import GHC.Runtime.Context

import GHC.Iface.Syntax ( showToHeader )
import GHC.Iface.Env    ( newInteractiveBinder )
import GHC.Core.Type

import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Utils.Monad
import GHC.Utils.Logger

import GHC.Types.Id
import GHC.Types.Id.Make (ghcPrimIds)
import GHC.Types.Name
import GHC.Types.Var hiding ( varName )
import GHC.Types.Var.Set
import GHC.Types.Unique.Set
import GHC.Types.TyThing.Ppr
import GHC.Types.TyThing

import Control.Monad
import Control.Monad.Catch as MC
import Data.List ( (\\), partition )
import Data.Maybe
import Data.IORef

-------------------------------------
-- | The :print & friends commands
-------------------------------------
pprintClosureCommand :: GhcMonad m => Bool -> Bool -> String -> m ()
pprintClosureCommand bindThings force str = do
  tythings <- (catMaybes . concat) `liftM`
                 mapM (\w -> GHC.parseName w >>=
                                mapM GHC.lookupName)
                      (words str)

  -- Sort out good and bad tythings for :print and friends
  let (pprintables, unpprintables) = partition can_pprint tythings

  -- Obtain the terms and the recovered type information
  let ids = [id | AnId id <- pprintables]
  (subst, terms) <- mapAccumLM go emptyTCvSubst ids

  -- Apply the substitutions obtained after recovering the types
  modifySession $ \hsc_env ->
    hsc_env{hsc_IC = substInteractiveContext (hsc_IC hsc_env) subst}

  -- Finally, print the Results
  docterms <- mapM showTerm terms
  let sdocTerms = zipWith (\id docterm -> ppr id <+> char '=' <+> docterm)
                          ids
                          docterms
  printSDocs $ (no_pprint <$> unpprintables) ++ sdocTerms
 where
   -- Check whether a TyThing can be processed by :print and friends.
   -- Take only Ids, exclude pseudoops, they don't have any HValues.
   can_pprint :: TyThing -> Bool                              -- #19394
   can_pprint (AnId x)
       | x `notElem` ghcPrimIds = True
       | otherwise              = False
   can_pprint _                 = False

   -- Create a short message for a TyThing, that cannot processed by :print
   no_pprint :: TyThing -> SDoc
   no_pprint tything = ppr tything <+>
          text "is not eligible for the :print, :sprint or :force commands."

   -- Helper to print out the results of :print and friends
   printSDocs :: GhcMonad m => [SDoc] -> m ()
   printSDocs sdocs = do
      logger <- getLogger
      unqual <- GHC.getPrintUnqual
      liftIO $ printOutputForUser logger unqual $ vcat sdocs

   -- Do the obtainTerm--bindSuspensions-computeSubstitution dance
   go :: GhcMonad m => TCvSubst -> Id -> m (TCvSubst, Term)
   go subst id = do
       let id' = updateIdTypeAndMult (substTy subst) id
           id_ty' = idType id'
       term_    <- GHC.obtainTermFromId maxBound force id'
       term     <- tidyTermTyVars term_
       term'    <- if bindThings
                     then bindSuspensions term
                     else return term
     -- Before leaving, we compare the type obtained to see if it's more specific
     --  Then, we extract a substitution,
     --  mapping the old tyvars to the reconstructed types.
       let reconstructed_type = termType term
       hsc_env <- getSession
       case (improveRTTIType hsc_env id_ty' reconstructed_type) of
         Nothing     -> return (subst, term')
         Just subst' -> do { logger <- getLogger
                           ; liftIO $
                               putDumpFileMaybe logger Opt_D_dump_rtti "RTTI"
                                 FormatText
                                 (fsep $ [text "RTTI Improvement for", ppr id,
                                  text "old substitution:" , ppr subst,
                                  text "new substitution:" , ppr subst'])
                           ; return (subst `unionTCvSubst` subst', term')}

   tidyTermTyVars :: GhcMonad m => Term -> m Term
   tidyTermTyVars t =
     withSession $ \hsc_env -> do
     let env_tvs      = tyThingsTyCoVars $ ic_tythings $ hsc_IC hsc_env
         my_tvs       = termTyCoVars t
         tvs          = env_tvs `minusVarSet` my_tvs
         tyvarOccName = nameOccName . tyVarName
         tidyEnv      = (initTidyOccEnv (map tyvarOccName (nonDetEltsUniqSet tvs))
           -- It's OK to use nonDetEltsUniqSet here because initTidyOccEnv
           -- forgets the ordering immediately by creating an env
                        , getUniqSet $ env_tvs `intersectVarSet` my_tvs)
     return $ mapTermType (snd . tidyOpenType tidyEnv) t

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
      (t', stuff)     <- liftIO $ foldTerm (nameSuspensionsAndGetInfos hsc_env availNames_var) t
      let (names, tys, fhvs) = unzip3 stuff
      let ids = [ mkVanillaGlobal name ty
                | (name,ty) <- zip names tys]
          new_ic = extendInteractiveContextWithIds ictxt ids
          interp = hscInterp hsc_env
      liftIO $ extendLoadedEnv interp (zip names fhvs)
      setSession hsc_env {hsc_IC = new_ic }
      return t'
     where

--    Processing suspensions. Give names and recopilate info
        nameSuspensionsAndGetInfos :: HscEnv -> IORef [String]
                                   -> TermFold (IO (Term, [(Name,Type,ForeignHValue)]))
        nameSuspensionsAndGetInfos hsc_env freeNames = TermFold
                      {
                        fSuspension = doSuspension hsc_env freeNames
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
        doSuspension hsc_env freeNames ct ty hval _name = do
          name <- atomicModifyIORef' freeNames (\x->(tail x, head x))
          n <- newGrimName hsc_env name
          return (Suspension ct ty hval (Just n), [(n,ty,hval)])


--  A custom Term printer to enable the use of Show instances
showTerm :: GhcMonad m => Term -> m SDoc
showTerm term = do
    dflags       <- GHC.getSessionDynFlags
    if gopt Opt_PrintEvldWithShow dflags
       then cPprTerm (liftM2 (++) (\_y->[cPprShowable]) cPprTermBase) term
       else cPprTerm cPprTermBase term
 where
  cPprShowable prec t@Term{ty=ty, val=fhv} =
    if not (isFullyEvaluatedTerm t)
     then return Nothing
     else do
        let set_session = do
                hsc_env <- getSession
                (new_env, bname) <- bindToFreshName hsc_env ty "showme"
                setSession new_env

                -- this disables logging of errors
                let noop_log _ _ _ _ = return ()
                pushLogHookM (const noop_log)

                return (hsc_env, bname)

            reset_session (old_env,_) = setSession old_env

        MC.bracket set_session reset_session $ \(_,bname) -> do
           hsc_env <- getSession
           dflags  <- GHC.getSessionDynFlags
           let expr = "Prelude.return (Prelude.show " ++
                         showPpr dflags bname ++
                      ") :: Prelude.IO Prelude.String"
               interp = hscInterp hsc_env
           txt_ <- withExtendedLoadedEnv interp
                                       [(bname, fhv)]
                                       (GHC.compileExprRemote expr)
           let myprec = 10 -- application precedence. TODO Infix constructors
           txt <- liftIO $ evalString interp txt_
           if not (null txt) then
             return $ Just $ cparen (prec >= myprec && needsParens txt)
                                    (text txt)
            else return Nothing

  cPprShowable prec NewtypeWrap{ty=new_ty,wrapped_term=t} =
      cPprShowable prec t{ty=new_ty}
  cPprShowable _ _ = return Nothing

  needsParens ('"':_) = False   -- some simple heuristics to see whether parens
                                -- are redundant in an arbitrary Show output
  needsParens ('(':_) = False
  needsParens txt = ' ' `elem` txt


  bindToFreshName hsc_env ty userName = do
    name <- newGrimName hsc_env userName
    let id       = mkVanillaGlobal name ty
        new_ic   = extendInteractiveContextWithIds (hsc_IC hsc_env) [id]
    return (hsc_env {hsc_IC = new_ic }, name)

--    Create new uniques and give them sequentially numbered names
newGrimName :: MonadIO m => HscEnv -> String -> m Name
newGrimName hsc_env userName
  = liftIO (newInteractiveBinder hsc_env occ noSrcSpan)
  where
    occ = mkOccName varName userName

pprTypeAndContents :: GhcMonad m => Id -> m SDoc
pprTypeAndContents id = do
  dflags  <- GHC.getSessionDynFlags
  let pcontents = gopt Opt_PrintBindContents dflags
      pprdId    = (pprTyThing showToHeader . AnId) id
  if pcontents
    then do
      let depthBound = 100
      -- If the value is an exception, make sure we catch it and
      -- show the exception, rather than propagating the exception out.
      e_term <- MC.try $ GHC.obtainTermFromId depthBound False id
      docs_term <- case e_term of
                      Right term -> showTerm term
                      Left  exn  -> return (text "*** Exception:" <+>
                                            text (show (exn :: GHC.Prelude.SomeExceptionWithLocation)))
      return $ pprdId <+> equals <+> docs_term
    else return pprdId
