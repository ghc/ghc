{-# LANGUAGE RankNTypes #-}

import Data.Data
import Data.List
import GHC
import DynFlags
import Outputable
import ApiAnnotation
import System.Environment( getArgs )
import System.Exit
import qualified Data.Map as Map
import qualified Data.Set as Set

main::IO()
main = do
  args <- getArgs
  case args of
   [libdir,fileName] -> testOneFile libdir fileName
   _ -> putStrLn "invoke with the libdir and a file to parse."

testOneFile :: FilePath -> String -> IO ()
testOneFile libdir fileName = do
       let modByFile m =
             case ml_hs_file $ ms_location m of
               Nothing -> False
               Just fn -> fn == fileName
       ((anns,_cs),p) <- runGhc (Just libdir) $ do
                        dflags <- getSessionDynFlags
                        _ <- setSessionDynFlags dflags
                        addTarget Target { targetId = TargetFile fileName Nothing
                                         , targetAllowObjCode = True
                                         , targetContents = Nothing }
                        _ <- load LoadAllTargets
                        graph <- getModuleGraph
                        let modSum =
                              case filter modByFile (mgModSummaries graph) of
                                [x] -> x
                                xs -> error $ "Can't find module, got:"
                                  ++ show (map (ml_hs_file . ms_location) xs)
                        p <- parseModule modSum
                        return (pm_annotations p,p)

       let sspans = Set.fromList $ getAllSrcSpans (pm_parsed_source p)

           exploded = [((kw,ss),[anchor])
                      | ((anchor,kw),sss) <- Map.toList anns,ss <- sss]

           exploded' = Map.toList $ Map.fromListWith (++) exploded

           problems' = filter (\(_,anchors)
                               -> not (any (\a -> Set.member a sspans) anchors))
                              exploded'

           problems'' = filter (\((a,_),_) -> a /= AnnEofPos) problems'

       putStrLn "---Problems (should be empty list)---"
       putStrLn (intercalate "\n" [pp $ Map.fromList $ map fst problems''])
       putStrLn "---Annotations-----------------------"
       putStrLn "-- SrcSpan the annotation is attached to, AnnKeywordId,"
       putStrLn "--    list of locations the keyword item appears in"
       -- putStrLn (intercalate "\n" [showAnns anns])
       putStrLn (showAnns anns)
       if null problems''
          then exitSuccess
          else exitFailure

    where
      getAllSrcSpans :: (Data t) => t -> [SrcSpan]
      getAllSrcSpans ast = everything (++) ([] `mkQ` getSrcSpan) ast
        where
          getSrcSpan :: SrcSpan -> [SrcSpan]
          getSrcSpan ss = [ss]


showAnns :: Map.Map ApiAnnKey [SrcSpan] -> String
showAnns anns = "[\n" ++ (intercalate ",\n"
   $ map (\((s,k),v)
              -- -> ("(AK " ++ pp s ++ " " ++ show k ++" = " ++ pp v ++ ")\n"))
              -> ("((" ++ pp s ++ "," ++ show k ++"), " ++ pp v ++ ")"))
   $ Map.toList anns)
    ++ "\n]\n"

pp :: (Outputable a) => a -> String
pp a = showPpr unsafeGlobalDynFlags a


-- ---------------------------------------------------------------------

-- Copied from syb for the test


-- | Generic queries of type \"r\",
--   i.e., take any \"a\" and return an \"r\"
--
type GenericQ r = forall a. Data a => a -> r


-- | Make a generic query;
--   start from a type-specific case;
--   return a constant otherwise
--
mkQ :: ( Typeable a
       , Typeable b
       )
    => r
    -> (b -> r)
    -> a
    -> r
(r `mkQ` br) a = case cast a of
                        Just b  -> br b
                        Nothing -> r



-- | Summarise all nodes in top-down, left-to-right order
everything :: (r -> r -> r) -> GenericQ r -> GenericQ r

-- Apply f to x to summarise top-level node;
-- use gmapQ to recurse into immediate subterms;
-- use ordinary foldl to reduce list of intermediate results

everything k f x = foldl k (f x) (gmapQ (everything k f) x)
