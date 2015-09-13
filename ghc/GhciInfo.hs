{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Get information on modules, identifiers, etc.

module GhciInfo (collectInfo, getModInfo, showppr) where

import           Control.Exception
import           Control.Monad
import qualified CoreUtils
import           Data.Data
import           Data.List
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as M
import           Data.Maybe
import           Data.Time
import           Desugar
import           GHC
import           GhciTypes
import           GhcMonad
import           NameSet
import           Outputable
import           Prelude           hiding (mod)
import           System.Directory
import           TcHsSyn
import           Var

-- | Collect type info data for the loaded modules.
collectInfo :: (GhcMonad m) => Map ModuleName ModInfo -> [ModuleName]
               -> m (Map ModuleName ModInfo)
collectInfo ms loaded = do
    df <- getSessionDynFlags
    liftIO (filterM cacheInvalid loaded) >>= \case
        [] -> return ms
        invalidated -> do
            liftIO (putStrLn ("Collecting type info for " ++
                              show (length invalidated) ++
                              " module(s) ... "))

            foldM (go df) ms invalidated
  where
    go df m name = do { info <- getModInfo name; return (M.insert name info m) }
                   `gcatch`
                   (\(e :: SomeException) -> do
                         liftIO $ putStrLn
                             ("Error while getting type info from " ++
                              showppr df name ++ ": " ++ show e)
                         return m)

    cacheInvalid name = case M.lookup name ms of
        Nothing -> return True
        Just mi -> do
            let fp = ml_obj_file (ms_location (modinfoSummary mi))
                last' = modinfoLastUpdate mi
            exists <- doesFileExist fp
            if exists
                then (> last') <$> getModificationTime fp
                else return True

-- | Get info about the module: summary, types, etc.
getModInfo :: (GhcMonad m) => ModuleName -> m ModInfo
getModInfo name = do
    m <- getModSummary name
    p <- parseModule m
    typechecked <- typecheckModule p
    allTypes <- processAllTypeCheckedModule typechecked
    let i = tm_checked_module_info typechecked
    now <- liftIO getCurrentTime
    return (ModInfo m allTypes i now)

-- | Get ALL source spans in the module.
processAllTypeCheckedModule :: GhcMonad m => TypecheckedModule -> m [SpanInfo]
processAllTypeCheckedModule tcm = do
    bts <- mapM (getTypeLHsBind tcm) bs
    ets <- mapM (getTypeLHsExpr tcm) es
    pts <- mapM (getTypeLPat tcm) ps
    return (mapMaybe toSpanInfo
            (sortBy cmp(concat bts ++ catMaybes (ets ++ pts))))
  where
    tcs = tm_typechecked_source tcm
    bs = listifyAllSpans tcs :: [LHsBind Id]
    es = listifyAllSpans tcs :: [LHsExpr Id]
    ps = listifyAllSpans tcs :: [LPat Id]

    cmp (_,a,_) (_,b,_)
        | a `isSubspanOf` b = LT
        | b `isSubspanOf` a = GT
        | otherwise         = EQ

getTypeLHsBind :: (GhcMonad m) => TypecheckedModule -> LHsBind Id
                  -> m [(Maybe Id,SrcSpan,Type)]
getTypeLHsBind _ (L _spn FunBind{fun_id = pid,fun_matches = MG _ _ _typ _}) =
    return (return (Just (unLoc pid),getLoc pid,varType (unLoc pid)))
getTypeLHsBind _ _ = return [] -- TODO: are these all cases we need to handle?


getTypeLHsExpr :: (GhcMonad m) => TypecheckedModule -> LHsExpr Id
                  -> m (Maybe (Maybe Id,SrcSpan,Type))
getTypeLHsExpr _ e = do
    hs_env  <- getSession
    (_,mbe) <- liftIO $ deSugarExpr hs_env e
    case mbe of
        Nothing   -> return Nothing
        Just expr -> return $ Just (mid, getLoc e, CoreUtils.exprType expr)
  where
    mid | HsVar i <- unwrapVar (unLoc e) = Just i
        | otherwise                      = Nothing

    unwrapVar (HsWrap _ var) = var
    unwrapVar e' = e'

-- | Get id and type for patterns.
getTypeLPat :: (GhcMonad m) => TypecheckedModule -> LPat Id
               -> m (Maybe (Maybe Id,SrcSpan,Type))
getTypeLPat _ (L spn pat) =
    return (Just (getMaybeId pat,spn,hsPatType pat))
  where
    getMaybeId (VarPat vid) = Just vid
    getMaybeId _            = Nothing


-- | Pretty print the types into a 'SpanInfo'.
toSpanInfo :: (Maybe Id,SrcSpan,Type) -> Maybe SpanInfo
toSpanInfo (n,RealSrcSpan spn,typ) =
    Just (SpanInfo (srcSpanStartLine spn)
                   (srcSpanStartCol spn - 1)
                   (srcSpanEndLine spn)
                   (srcSpanEndCol spn - 1)
                   (Just typ)
                   n)
toSpanInfo _ = Nothing


-- | Pretty print something to string.
showppr :: Outputable a => DynFlags -> a -> String
showppr dflags = showSDocForUser dflags neverQualify . ppr


-- | Get ALL source spans in the source.
listifyAllSpans :: Typeable a => TypecheckedSource -> [Located a]
listifyAllSpans = everythingAllSpans (++) [] ([] `mkQ` (\x -> [x | p x]))
  where
    p (L spn _) = isGoodSrcSpan spn


everythingAllSpans :: (r -> r -> r) -> r -> GenericQ r -> GenericQ r
everythingAllSpans k z f x
  | (False `mkQ` (const True :: NameSet -> Bool)) x = z
  | otherwise = foldl k (f x) (gmapQ (everythingAllSpans k z f) x)

-- aliases from syb
type GenericQ r = forall a. Data a => a -> r

mkQ :: (Typeable a, Typeable b) => r -> (b -> r) -> a -> r
(r `mkQ` br) a = maybe r br (cast a)
