module Main (main) where

import Control.Monad      (when)
import Data.Either        (partitionEithers)
import Data.Foldable      (for_, traverse_)
import Data.Maybe         (listToMaybe)
import Data.String        (fromString)
import Data.Traversable   (for)
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

import qualified Data.Text as T
import qualified Cabal.Index                    as I
import qualified Cabal.Plan                     as P
import qualified Data.Aeson                     as A
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import qualified Distribution.Types.PackageName as C
import qualified Distribution.Types.Version as C
import qualified Topograph                      as TG

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fp] -> main1 fp
        _    -> die "Usage: hadrian-bootstrap-gen plan.json"

main1 :: FilePath -> IO ()
main1 planPath = do
    meta <- I.cachedHackageMetadata
    plan <- P.decodePlanJson planPath
    main2 meta plan

main2 :: Map.Map C.PackageName I.PackageInfo -> P.PlanJson -> IO ()
main2 meta plan = do
    info $ show $ Map.keys $ P.pjUnits plan

    -- find hadrian-install:exe:hadrian unit
    (hadrianUid, hadrianUnit) <- case findHadrianExe plan of
        Just x  -> return x
        Nothing -> die "Cannot find hadrian:exe unit"

    info $ "hadrian:exe unit " ++ show hadrianUid

    -- BFS from hadrian unit, getting all dependencies
    units <- bfs plan hadrianUnit

    info $ "Unit order:"
    for_ units $ \unit -> do
        info $ " - " ++ show (P.uId unit)

    (builtin, deps) <- fmap partitionEithers $ for units $ \unit -> do
        let P.PkgId pkgname@(P.PkgName tpkgname) ver@(P.Ver verdigits) = P.uPId unit

        let uid = P.uId unit

        let cpkgname :: C.PackageName
            cpkgname = C.mkPackageName (T.unpack tpkgname)

        let cversion :: C.Version
            cversion = C.mkVersion verdigits

        case P.uType unit of
            P.UnitTypeBuiltin ->
                return $ Left Builtin
                  { builtinPackageName = pkgname
                  , builtinVersion     = ver
                  }

            _ -> do
                (src, rev, revhash) <- case P.uSha256 unit of
                    Just _  -> do
                        pkgInfo <- maybe (die $ "Cannot find " ++ show uid ++ " package metadata") return $
                            Map.lookup cpkgname meta
                        relInfo <- maybe (die $ "Cannot find " ++ show uid ++ " version metadata") return $
                            Map.lookup cversion $ I.piVersions pkgInfo

                        return
                            ( Hackage
                            , Just $ fromIntegral (I.riRevision relInfo)
                            , P.sha256FromByteString $ I.getSHA256 $ I.riCabal relInfo
                            )

                    Nothing -> case P.uType unit of
                        P.UnitTypeLocal   -> return (Local, Nothing, Nothing)
                        t                 -> die $ "Unit of wrong type " ++ show uid ++ " " ++ show t

                return $ Right Dep
                    { depPackageName = pkgname
                    , depVersion     = ver
                    , depSource      = src
                    , depSrcHash     = P.uSha256 unit
                    , depRevision    = rev
                    , depRevHash     = revhash
                    , depFlags       =
                        [ (if fval then "+" else "-") ++ T.unpack fname
                        | (P.FlagName fname, fval) <- Map.toList (P.uFlags unit)
                        ]
                    }

    LBS.putStr $ A.encode Result
        { resBuiltin      = builtin
        , resDependencies = deps
        }

bfs :: P.PlanJson -> P.Unit -> IO [P.Unit]
bfs plan unit0 = do
    uids <- either (\loop -> die $ "Loop in install-plan " ++ show loop) id $ TG.runG am $ \g -> do
        v <- maybe (die "Cannot find hadrian-install unit in topograph") return $
            TG.gToVertex g $ P.uId unit0

        let t = TG.dfs g v

        return $ map (TG.gFromVertex g) $
            -- nub and sort
            reverse $ Set.toList $ Set.fromList $ concat t

    for uids $ \uid -> do
        unit <- lookupUnit units uid
        case Map.toList (P.uComps unit) of
            [(_, compinfo)] -> checkExeDeps uid (P.pjUnits plan) (P.ciExeDeps compinfo)
            _               -> die $ "Unit with multiple components " ++ show uid
        return unit

  where
    am :: Map.Map P.UnitId (Set.Set P.UnitId)
    am = fmap (foldMap P.ciLibDeps . P.uComps) units

    units = P.pjUnits plan

checkExeDeps :: P.UnitId -> Map.Map P.UnitId P.Unit -> Set.Set P.UnitId -> IO ()
checkExeDeps pkgUid units = traverse_ check . Set.toList where
    check uid = do
        unit <- lookupUnit units uid
        let P.PkgId pkgname _ = P.uPId unit
        when (pkgname /= P.PkgName (fromString "hsc2hs")) $ do
            die $ "unit " ++ show pkgUid ++ " depends on executable " ++ show uid

lookupUnit :: Map.Map P.UnitId P.Unit -> P.UnitId -> IO P.Unit
lookupUnit units uid
    = maybe (die $ "Cannot find unit " ++ show uid) return
    $ Map.lookup uid units

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Result = Result
    { resBuiltin      :: [Builtin]
    , resDependencies :: [Dep]
    }
  deriving (Show)

data Builtin = Builtin
    { builtinPackageName :: P.PkgName
    , builtinVersion     :: P.Ver
    }
  deriving (Show)

data Dep = Dep
    { depPackageName :: P.PkgName
    , depVersion     :: P.Ver
    , depSource      :: SrcType
    , depSrcHash     :: Maybe P.Sha256
    , depRevision    :: Maybe Int
    , depRevHash     :: Maybe P.Sha256
    , depFlags       :: [String]
    }
  deriving (Show)

data SrcType
    = Hackage
    | Local
  deriving (Show)

instance A.ToJSON Result where
    toJSON res = A.object
        [ fromString "builtin"      A..= resBuiltin res
        , fromString "dependencies" A..= resDependencies res
        ]

instance A.ToJSON Builtin where
    toJSON b = A.object
        [ fromString "package"      A..= builtinPackageName b
        , fromString "version"      A..= builtinVersion b
        ]

instance A.ToJSON Dep where
    toJSON dep = A.object
        [ fromString "package"      A..= depPackageName dep
        , fromString "version"      A..= depVersion dep
        , fromString "source"       A..= depSource dep
        , fromString "src_sha256"   A..= depSrcHash dep
        , fromString "revision"     A..= depRevision dep
        , fromString "cabal_sha256" A..= depRevHash dep
        , fromString "flags"        A..= depFlags dep
        ]

instance A.ToJSON SrcType where
    toJSON Hackage     = fromString "hackage"
    toJSON Local       = fromString "local"

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

info :: String -> IO ()
info msg = hPutStrLn stderr $ "INFO: " ++ msg

die :: String -> IO a
die msg = do
    hPutStrLn stderr msg
    exitFailure

-------------------------------------------------------------------------------
-- Pure bits
-------------------------------------------------------------------------------

findHadrianExe :: P.PlanJson -> Maybe (P.UnitId, P.Unit)
findHadrianExe plan = listToMaybe
    [ (uid, unit)
    | (uid, unit) <- Map.toList (P.pjUnits plan)
    , let P.PkgId pkgname _ = P.uPId unit
    , pkgname == P.PkgName (fromString "hadrian")
    , Map.keys (P.uComps unit) == [P.CompNameExe (fromString "hadrian")]
    ]
