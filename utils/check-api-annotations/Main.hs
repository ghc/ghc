{-# LANGUAGE RankNTypes #-}

import Data.Data
import Data.List
import GHC
import GHC.Driver.Ppr
import GHC.Utils.Outputable
import GHC.Types.SrcLoc
import System.Environment( getArgs )
import System.Exit
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe( isJust )

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
       (anns',p) <- runGhc (Just libdir) $ do
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

           ann_items ::  Map.Map ApiAnnKey [RealSrcSpan]
           -- ann_items = apiAnnItems anns'
           ann_items = Map.empty

           exploded = [((kw,ss),[anchor'])
                      | ((anchor',kw),sss) <- Map.toList ann_items,ss <- sss]

           exploded' = Map.toList $ Map.fromListWith (++) exploded

           problems' = filter (\(_,anchors)
                               -> not (any (\a -> Set.member a sspans) anchors))
                              exploded'

           -- Check that every annotation location in 'vs' appears after
           -- the start of the enclosing span 's'
           comesBefore ((s,_),vs) = not $ all ok vs
             where ok v = realSrcSpanStart s <= realSrcSpanStart v

           precedingProblems = filter comesBefore $ Map.toList ann_items

       putStrLn "---Unattached Annotation Problems (should be empty list)---"
       putStrLn (intercalate "\n" [pp $ Map.fromList $ map fst problems'])
       putStrLn "---Ann before enclosing span problem (should be empty list)---"
       putStrLn (showAnnsList precedingProblems)
       putStrLn "---Annotations-----------------------"
       putStrLn "-- SrcSpan the annotation is attached to, AnnKeywordId,"
       putStrLn "--    list of locations the keyword item appears in"
       -- putStrLn (intercalate "\n" [showAnns ann_items])
       putStrLn (showAnns ann_items)
       putStrLn "---Eof Position (should be Just)-----"
       putStrLn (show (apiAnnEofPos anns'))
       if null problems' && null precedingProblems && isJust (apiAnnEofPos anns')
          then exitSuccess
          else exitFailure

    where
      getAllSrcSpans :: (Data t) => t -> [RealSrcSpan]
      getAllSrcSpans ast = everything (++) ([] `mkQ` getSrcSpan) ast
        where
          getSrcSpan :: SrcSpan -> [RealSrcSpan]
          getSrcSpan (RealSrcSpan ss _) = [ss]
          getSrcSpan (UnhelpfulSpan _) = []


showAnns :: Map.Map ApiAnnKey [RealSrcSpan] -> String
showAnns anns' = showAnnsList $ Map.toList anns'

showAnnsList :: [(ApiAnnKey, [RealSrcSpan])] -> String
showAnnsList annsList = "[\n" ++ (intercalate ",\n"
   $ map (\((s,k),v)
              -> ("((" ++ pp s ++ "," ++ show k ++"), " ++ pp v ++ ")"))
   annsList)
    ++ "\n]\n"

pp :: (Outputable a) => a -> String
pp a = showPprUnsafe a


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
