{-# LANGUAGE BangPatterns #-}

-- | Find type/location information.

module GhciFind
  (findType,findLoc,findNameUses)
  where

import           Control.Monad
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe

import           FastString
import           GHC
import           GhcMonad
import           GhciInfo (showppr)
import           GhciTypes
import           Name
import           SrcLoc
import           System.Directory
import           Var

-- | Find any uses of the given identifier in the codebase.
findNameUses :: (GhcMonad m)
             => Map ModuleName ModInfo
             -> FilePath
             -> String
             -> Int
             -> Int
             -> Int
             -> Int
             -> m (Either String [SrcSpan])
findNameUses infos fp string sl sc el ec =
  do mname <- guessModule infos fp
     case mname of
       Nothing ->
         return (Left "Couldn't guess that module name. Does it exist?")
       Just name ->
         case M.lookup name infos of
           Nothing ->
             return (Left ("No module info for the current file! Try loading it?"))
           Just info ->
             do mname' <- findName infos info string sl sc el ec
                case mname' of
                  Left e -> return (Left e)
                  Right name' ->
                    case getSrcSpan name' of
                      UnhelpfulSpan{} ->
                        do d <- getSessionDynFlags
                           return (Left ("Found a name, but no location information. The module is: " ++
                                         maybe "<unknown>"
                                               (showppr d . moduleName)
                                               (nameModule_maybe name')))
                      span' ->
                        return (Right (stripSurrounding
                                         (span' :
                                          map makeSrcSpan
                                              (filter ((== Just name') .
                                                       fmap getName .
                                                       spaninfoVar)
                                                      (modinfoSpans info)))))
  where makeSrcSpan (SpanInfo sl' sc' el' ec' _ _) =
          RealSrcSpan
            (mkRealSrcSpan
               (mkRealSrcLoc (mkFastString fp)
                             sl'
                             (1 + sc'))
               (mkRealSrcLoc (mkFastString fp)
                             el'
                             (1 + ec')))

-- | Strip out spans which surrounding other spans in a parent->child
-- fashion. Those are useless.
stripSurrounding :: [SrcSpan] -> [SrcSpan]
stripSurrounding xs =
  mapMaybe (\x -> if any (\y -> overlaps x y && x /= y) xs
                     then Nothing
                     else Just x)
           xs

-- | Does x overlap y in x `overlaps` y?
overlaps :: SrcSpan -> SrcSpan -> Bool
overlaps y x =
  case (x,y) of
    (RealSrcSpan x',RealSrcSpan y') ->
      realSrcSpanStart y' <= realSrcSpanStart x' &&
      realSrcSpanEnd y' >= realSrcSpanEnd x'
    _ -> False

-- | Try to find the location of the given identifier at the given
-- position in the module.
findLoc :: (GhcMonad m)
        => Map ModuleName ModInfo
        -> FilePath
        -> String
        -> Int
        -> Int
        -> Int
        -> Int
        -> m (Either String SrcSpan)
findLoc infos fp string sl sc el ec =
  do mname <- guessModule infos fp
     case mname of
       Nothing ->
         return (Left "Couldn't guess that module name. Does it exist?")
       Just name ->
         case M.lookup name infos of
           Nothing ->
             return (Left ("No module info for the current file! Try loading it?"))
           Just info ->
             do mname' <- findName infos info string sl sc el ec
                d <- getSessionDynFlags
                case mname' of
                  Left reason ->
                    return (Left reason)
                  Right name' ->
                    case getSrcSpan name' of
                      UnhelpfulSpan{} ->
                        return (Left ("Found a name, but no location information. The module is: " ++
                                      maybe "<unknown>"
                                            (showppr d . moduleName)
                                            (nameModule_maybe name')))
                      span' ->
                        return (Right span')

-- | Try to resolve the name located at the given position, or
-- otherwise resolve based on the current module's scope.
findName :: GhcMonad m
         => Map ModuleName ModInfo
         -> ModInfo
         -> String
         -> Int
         -> Int
         -> Int
         -> Int
         -> m (Either String Name)
findName infos mi string sl sc el ec =
  case resolveName (modinfoSpans mi)
                   sl
                   sc
                   el
                   ec of
    Nothing -> tryExternalModuleResolution
    Just name ->
      case getSrcSpan name of
        UnhelpfulSpan{} -> tryExternalModuleResolution
        _ -> return (Right (getName name))
  where tryExternalModuleResolution =
          case find (matchName string)
                    (fromMaybe [] (modInfoTopLevelScope (modinfoInfo mi))) of
            Nothing ->
              return (Left "Couldn't resolve to any modules.")
            Just imported -> resolveNameFromModule infos imported
        matchName :: String -> Name -> Bool
        matchName str name =
          str ==
          occNameString (getOccName name)

-- | Try to resolve the name from another (loaded) module's exports.
resolveNameFromModule :: GhcMonad m
                      => Map ModuleName ModInfo
                      -> Name
                      -> m (Either String Name)
resolveNameFromModule infos name =
  do d <- getSessionDynFlags
     case nameModule_maybe name of
       Nothing ->
         return (Left ("No module for " ++
                       showppr d name))
       Just modL ->
         do case M.lookup (moduleName modL) infos of
              Nothing ->
                do (return (Left (showppr d (modulePackageKey modL) ++ ":" ++
                                  showppr d modL)))
              Just info ->
                case find (matchName name)
                          (modInfoExports (modinfoInfo info)) of
                  Just name' ->
                    return (Right name')
                  Nothing ->
                    return (Left "No matching export in any local modules.")
  where matchName :: Name -> Name -> Bool
        matchName x y =
          occNameString (getOccName x) ==
          occNameString (getOccName y)

-- | Try to resolve the type display from the given span.
resolveName :: [SpanInfo] -> Int -> Int -> Int -> Int -> Maybe Var
resolveName spans' sl sc el ec =
  listToMaybe (mapMaybe spaninfoVar (filter inside (reverse spans')))
  where inside (SpanInfo sl' sc' el' ec' _ _) =
          ((sl' == sl && sc' >= sc) || (sl' > sl)) &&
          ((el' == el && ec' <= ec) || (el' < el))

-- | Try to find the type of the given span.
findType :: GhcMonad m
         => Map ModuleName ModInfo
         -> FilePath
         -> String
         -> Int
         -> Int
         -> Int
         -> Int
         -> m (Either String (ModInfo, Type))
findType infos fp string sl sc el ec =
  do mname <- guessModule infos fp
     case mname of
       Nothing ->
         return (Left "Couldn't guess that module name. Does it exist?")
       Just name ->
         case M.lookup name infos of
           Nothing ->
             return (Left ("Couldn't guess the module nameIs this module loaded?"))
           Just info ->
             do let !mty =
                      resolveType (modinfoSpans info)
                                  sl
                                  sc
                                  el
                                  ec
                case mty of
                  Just ty -> return (Right (info, ty))
                  Nothing ->
                    fmap (Right . (,) info) (exprType string)

-- | Try to resolve the type display from the given span.
resolveType :: [SpanInfo] -> Int -> Int -> Int -> Int -> Maybe Type
resolveType spans' sl sc el ec =
  join (fmap spaninfoType (find inside (reverse spans')))
  where inside (SpanInfo sl' sc' el' ec' _ _) =
          ((sl' == sl && sc' >= sc) || (sl' > sl)) &&
          ((el' == el && ec' <= ec) || (el' < el))

-- | Guess a module name from a file path.
guessModule :: GhcMonad m
            => Map ModuleName ModInfo -> FilePath -> m (Maybe ModuleName)
guessModule infos fp =
  do target <- guessTarget fp Nothing
     case targetId target of
       TargetModule mn -> return (Just mn)
       TargetFile fp' _ ->
         case find ((Just fp' ==) .
                    ml_hs_file . ms_location . modinfoSummary . snd)
                   (M.toList infos) of
           Just (mn,_) -> return (Just mn)
           Nothing ->
             do fp'' <- liftIO (makeRelativeToCurrentDirectory fp')
                target' <- guessTarget fp'' Nothing
                case targetId target' of
                  TargetModule mn ->
                    return (Just mn)
                  _ ->
                    case find ((Just fp'' ==) .
                               ml_hs_file . ms_location . modinfoSummary . snd)
                              (M.toList infos) of
                      Just (mn,_) ->
                        return (Just mn)
                      Nothing -> return Nothing
