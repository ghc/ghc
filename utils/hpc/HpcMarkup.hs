---------------------------------------------------------
-- The main program for the hpc-markup tool, part of HPC.
-- Andy Gill and Colin Runciman, June 2006
---------------------------------------------------------

module HpcMarkup (markup_plugin) where

import Trace.Hpc.Mix
import Trace.Hpc.Tix
import Trace.Hpc.Util

import HpcFlags
import HpcUtils

import System.Directory
import System.IO (localeEncoding)
import Data.List
import Data.Maybe(fromJust)
import Data.Array
import Data.Monoid
import Control.Monad
import qualified Data.Set as Set

------------------------------------------------------------------------------

markup_options :: FlagOptSeq
markup_options
        = excludeOpt
        . includeOpt
        . srcDirOpt
        . hpcDirOpt
        . resetHpcDirsOpt
        . funTotalsOpt
        . altHighlightOpt
        . destDirOpt

markup_plugin :: Plugin
markup_plugin = Plugin { name = "markup"
                       , usage = "[OPTION] .. <TIX_FILE> [<MODULE> [<MODULE> ..]]"
                       , options = markup_options
                       , summary = "Markup Haskell source with program coverage"
                       , implementation = markup_main
                       , init_flags = default_flags
                       , final_flags = default_final_flags
                       }

------------------------------------------------------------------------------

markup_main :: Flags -> [String] -> IO ()
markup_main flags (prog:modNames) = do
  let hpcflags1 = flags
                { includeMods = Set.fromList modNames
                                   `Set.union`
                                includeMods flags }
  let Flags
       { funTotals = theFunTotals
       , altHighlight = invertOutput
       , destDir = dest_dir
       }  = hpcflags1

  mtix <- readTix (getTixFileName prog)
  Tix tixs <- case mtix of
    Nothing -> hpcError markup_plugin $ "unable to find tix file for: " ++ prog
    Just a -> return a

  mods <-
     sequence [ genHtmlFromMod dest_dir hpcflags1 tix theFunTotals invertOutput
              | tix <- tixs
              , allowModule hpcflags1 (tixModuleName tix)
              ]

  let index_name = "hpc_index"
      index_fun  = "hpc_index_fun"
      index_alt  = "hpc_index_alt"
      index_exp  = "hpc_index_exp"

  let writeSummary filename cmp = do
        let mods' = sortBy cmp mods

        putStrLn $ "Writing: " ++ (filename ++ ".html")

        writeFileUsing (dest_dir ++ "/" ++ filename ++ ".html") $
            "<html>" ++
            "<head>" ++
            charEncodingTag ++
            "<style type=\"text/css\">" ++
            "table.bar { background-color: #f25913; }\n" ++
            "td.bar { background-color: #60de51;  }\n" ++
            "td.invbar { background-color: #f25913;  }\n" ++
            "table.dashboard { border-collapse: collapse  ; border: solid 1px black }\n" ++
            ".dashboard td { border: solid 1px black }\n" ++
            ".dashboard th { border: solid 1px black }\n" ++
            "</style>\n" ++
            "</head>" ++
            "<body>" ++
            "<table class=\"dashboard\" width=\"100%\" border=1>\n" ++
            "<tr>" ++
            "<th rowspan=2><a href=\"" ++ index_name ++ ".html\">module</a></th>" ++
            "<th colspan=3><a href=\"" ++ index_fun ++ ".html\">Top Level Definitions</a></th>" ++
            "<th colspan=3><a href=\"" ++ index_alt ++ ".html\">Alternatives</a></th>" ++
            "<th colspan=3><a href=\"" ++ index_exp ++ ".html\">Expressions</a></th>" ++
            "</tr>" ++
            "<tr>" ++
            "<th>%</th>" ++
            "<th colspan=2>covered / total</th>" ++
            "<th>%</th>" ++
            "<th colspan=2>covered / total</th>" ++
            "<th>%</th>" ++
            "<th colspan=2>covered / total</th>" ++
            "</tr>" ++
            concat [ showModuleSummary (modName,fileName,modSummary)
                   | (modName,fileName,modSummary) <- mods'
                   ] ++
            "<tr></tr>" ++
            showTotalSummary (mconcat
                                 [ modSummary
                                 | (_,_,modSummary) <- mods'
                                 ])
                   ++ "</table></body></html>\n"

  writeSummary index_name  $ \ (n1,_,_) (n2,_,_) -> compare n1 n2

  writeSummary index_fun $ \ (_,_,s1) (_,_,s2) ->
        compare (percent (topFunTicked s2) (topFunTotal s2))
                (percent (topFunTicked s1) (topFunTotal s1))

  writeSummary index_alt $ \ (_,_,s1) (_,_,s2) ->
        compare (percent (altTicked s2) (altTotal s2))
                (percent (altTicked s1) (altTotal s1))

  writeSummary index_exp $ \ (_,_,s1) (_,_,s2) ->
        compare (percent (expTicked s2) (expTotal s2))
                (percent (expTicked s1) (expTotal s1))


markup_main _ []
    = hpcError markup_plugin $ "no .tix file or executable name specified"

charEncodingTag :: String
charEncodingTag =
    "<meta http-equiv=\"Content-Type\" " ++
          "content=\"text/html; " ++ "charset=" ++ show localeEncoding ++ "\">"

genHtmlFromMod
  :: String
  -> Flags
  -> TixModule
  -> Bool
  -> Bool
  -> IO (String, [Char], ModuleSummary)
genHtmlFromMod dest_dir flags tix theFunTotals invertOutput = do
  let theHsPath = srcDirs flags
  let modName0 = tixModuleName tix

  (Mix origFile _ _ tabStop mix') <- readMixWithFlags flags (Right tix)

  let arr_tix :: Array Int Integer
      arr_tix = listArray (0,length (tixModuleTixs tix) - 1)
              $ tixModuleTixs tix

  let tickedWith :: Int -> Integer
      tickedWith n = arr_tix ! n

      isTicked n = tickedWith n /= 0

  let info = [ (pos,theMarkup)
             | (gid,(pos,boxLabel)) <- zip [0 ..] mix'
             , let binBox = case (isTicked gid,isTicked (gid+1)) of
                               (False,False) -> []
                               (True,False)  -> [TickedOnlyTrue]
                               (False,True)  -> [TickedOnlyFalse]
                               (True,True)   -> []
             , let tickBox = if isTicked gid
                             then [IsTicked]
                             else [NotTicked]
             , theMarkup <- case boxLabel of
                                  ExpBox {} -> tickBox
                                  TopLevelBox {}
                                            -> TopLevelDecl theFunTotals (tickedWith gid) : tickBox
                                  LocalBox {}   -> tickBox
                                  BinBox _ True -> binBox
                                  _             -> []
             ]


  let modSummary = foldr (.) id
             [ \ st ->
               case boxLabel of
                 ExpBox False
                        -> st { expTicked = ticked (expTicked st)
                              , expTotal = succ (expTotal st)
                              }
                 ExpBox True
                        -> st { expTicked = ticked (expTicked st)
                              , expTotal = succ (expTotal st)
                              , altTicked = ticked (altTicked st)
                              , altTotal = succ (altTotal st)
                              }
                 TopLevelBox _ ->
                           st { topFunTicked = ticked (topFunTicked st)
                              , topFunTotal = succ (topFunTotal st)
                              }
                 _ -> st
             | (gid,(_pos,boxLabel)) <- zip [0 ..] mix'
             , let ticked = if isTicked gid
                            then succ
                            else id
             ] $ mempty

  -- add prefix to modName argument
  content <- readFileFromPath (hpcError markup_plugin) origFile theHsPath

  let content' = markup tabStop info content
  let show' = reverse . take 5 . (++ "       ") . reverse . show
  let addLine n xs = "<span class=\"lineno\">" ++ show' n ++ " </span>" ++ xs
  let addLines = unlines . map (uncurry addLine) . zip [1 :: Int ..] . lines
  let fileName = modName0 ++ ".hs.html"
  putStrLn $ "Writing: " ++ fileName
  writeFileUsing (dest_dir ++ "/" ++ fileName) $
            unlines ["<html>",
                     "<head>",
                     charEncodingTag,
                     "<style type=\"text/css\">",
                     "span.lineno { color: white; background: #aaaaaa; border-right: solid white 12px }",
                     if invertOutput
                     then "span.nottickedoff { color: #404040; background: white; font-style: oblique }"
                     else "span.nottickedoff { background: " ++ yellow ++ "}",
                     if invertOutput
                     then "span.istickedoff { color: black; background: #d0c0ff; font-style: normal; }"
                     else "span.istickedoff { background: white }",
                     "span.tickonlyfalse { margin: -1px; border: 1px solid " ++ red ++ "; background: " ++ red ++ " }",
                     "span.tickonlytrue  { margin: -1px; border: 1px solid " ++ green ++ "; background: " ++ green ++ " }",
                     "span.funcount { font-size: small; color: orange; z-index: 2; position: absolute; right: 20 }",
                     if invertOutput
                     then "span.decl { font-weight: bold; background: #d0c0ff }"
                     else "span.decl { font-weight: bold }",
                     "span.spaces    { background: white }",
                     "</style>",
                     "</head>",
                     "<body>",
                     "<pre>"] ++ addLines content' ++ "\n</pre>\n</body>\n</html>\n";


  modSummary `seq` return (modName0,fileName,modSummary)

data Loc = Loc !Int !Int
         deriving (Eq,Ord,Show)

data Markup
     = NotTicked
     | TickedOnlyTrue
     | TickedOnlyFalse
     | IsTicked
     | TopLevelDecl
           Bool     -- display entry totals
           Integer
     deriving (Eq,Show)

markup    :: Int                -- ^tabStop
          -> [(HpcPos,Markup)]  -- random list of tick location pairs
          -> String             -- text to mark up
          -> String
markup tabStop mix str = addMarkup tabStop str (Loc 1 1) [] sortedTickLocs
  where
    tickLocs = [ (Loc ln1 c1,Loc ln2 c2,mark)
               | (pos,mark) <- mix
               , let (ln1,c1,ln2,c2) = fromHpcPos pos
               ]
    sortedTickLocs = sortBy (\(locA1,locZ1,_) (locA2,locZ2,_) ->
                              (locA1,locZ2) `compare` (locA2,locZ1)) tickLocs

addMarkup :: Int                -- tabStop
          -> String             -- text to mark up
          -> Loc                -- current location
          -> [(Loc,Markup)]     -- stack of open ticks, with closing location
          -> [(Loc,Loc,Markup)] -- sorted list of tick location pairs
          -> String

-- check the pre-condition.
--addMarkup tabStop cs loc os ticks
--   | not (isSorted (map fst os)) = error $ "addMarkup: bad closing ordering: " ++ show os

--addMarkup tabStop cs loc os@(_:_) ticks
--   | trace (show (loc,os,take 10 ticks)) False = undefined

-- close all open ticks, if we have reached the end
addMarkup _ [] _loc os [] =
  concatMap (const closeTick) os
addMarkup tabStop cs loc ((o,_):os) ticks | loc > o =
  closeTick ++ addMarkup tabStop cs loc os ticks

--addMarkup tabStop cs loc os ((t1,t2,tik@(TopLevelDecl {})):ticks) | loc == t1 =
--   openTick tik ++ closeTick ++ addMarkup tabStop cs loc os ticks

addMarkup tabStop cs loc os ((t1,t2,tik0):ticks) | loc == t1 =
  case os of
  ((_,tik'):_)
    | not (allowNesting tik0 tik')
    -> addMarkup tabStop cs loc os ticks -- already marked or bool within marked bool
  _ -> openTick tik0 ++ addMarkup tabStop cs loc (addTo (t2,tik0) os) ticks
 where

  addTo (t,tik) []             = [(t,tik)]
  addTo (t,tik) ((t',tik'):xs) | t <= t'   = (t,tik):(t',tik'):xs
                               | otherwise = (t',tik):(t',tik'):xs

addMarkup tabStop0 cs loc os ((t1,_t2,_tik):ticks) | loc > t1 =
          -- throw away this tick, because it is from a previous place ??
          addMarkup tabStop0 cs loc os ticks

addMarkup tabStop0 ('\n':cs) loc@(Loc ln col) os@((Loc ln2 col2,_):_) ticks
          | ln == ln2 && col < col2
          = addMarkup tabStop0 (' ':'\n':cs) loc os ticks
addMarkup tabStop0 (c0:cs) loc@(Loc _ p) os ticks =
  if c0=='\n' && os/=[] then
    concatMap (const closeTick) (downToTopLevel os) ++
    c0 : "<span class=\"spaces\">" ++ expand 1 w ++ "</span>" ++
    concatMap (openTick.snd) (reverse (downToTopLevel os)) ++
    addMarkup tabStop0 cs' loc' os ticks
  else if c0=='\t' then
    expand p "\t" ++ addMarkup tabStop0 cs (incBy c0 loc) os ticks
  else
    escape c0 ++ addMarkup tabStop0 cs (incBy c0 loc) os ticks
  where
  (w,cs') = span (`elem` " \t") cs
  loc' = foldl (flip incBy) loc (c0:w)
  escape '>' = "&gt;"
  escape '<' = "&lt;"
  escape '"' = "&quot;"
  escape '&' = "&amp;"
  escape c  = [c]

  expand :: Int -> String -> String
  expand _ ""       = ""
  expand c ('\t':s) = replicate (c' - c) ' ' ++ expand c' s
    where
    c' = tabStopAfter 8 c
  expand c (' ':s)  = ' ' : expand (c+1) s
  expand _ _        = error "bad character in string for expansion"

  incBy :: Char -> Loc -> Loc
  incBy '\n' (Loc ln _c) = Loc (succ ln) 1
  incBy '\t' (Loc ln c) = Loc ln (tabStopAfter tabStop0 c)
  incBy _    (Loc ln c) = Loc ln (succ c)

  tabStopAfter :: Int -> Int -> Int
  tabStopAfter tabStop c = fromJust (find (>c) [1,(tabStop + 1)..])


addMarkup tabStop cs loc os ticks = "ERROR: " ++ show (take 10 cs,tabStop,loc,take 10 os,take 10 ticks)

openTick :: Markup -> String
openTick NotTicked       = "<span class=\"nottickedoff\">"
openTick IsTicked        = "<span class=\"istickedoff\">"
openTick TickedOnlyTrue  = "<span class=\"tickonlytrue\">"
openTick TickedOnlyFalse = "<span class=\"tickonlyfalse\">"
openTick (TopLevelDecl False _) = openTopDecl
openTick (TopLevelDecl True 0)
         = "<span class=\"funcount\">-- never entered</span>" ++
           openTopDecl
openTick (TopLevelDecl True 1)
         = "<span class=\"funcount\">-- entered once</span>" ++
           openTopDecl
openTick (TopLevelDecl True n0)
         = "<span class=\"funcount\">-- entered " ++ showBigNum n0 ++ " times</span>" ++ openTopDecl
  where showBigNum n | n <= 9999 = show n
                     | otherwise = showBigNum' (n `div` 1000) ++ "," ++ showWith (n `mod` 1000)
        showBigNum' n | n <= 999 = show n
                      | otherwise = showBigNum' (n `div` 1000) ++ "," ++ showWith (n `mod` 1000)
        showWith n = take 3 $ reverse $ ("000" ++) $ reverse $ show n

closeTick :: String
closeTick = "</span>"

openTopDecl :: String
openTopDecl = "<span class=\"decl\">"

downToTopLevel :: [(Loc,Markup)] -> [(Loc,Markup)]
downToTopLevel ((_,TopLevelDecl {}):_) = []
downToTopLevel (o : os)               = o : downToTopLevel os
downToTopLevel []                     = []


-- build in logic for nesting bin boxes

allowNesting :: Markup  -- innermost
            -> Markup   -- outermost
            -> Bool
allowNesting n m               | n == m = False -- no need to double nest
allowNesting IsTicked TickedOnlyFalse   = False
allowNesting IsTicked TickedOnlyTrue    = False
allowNesting _ _                        = True

------------------------------------------------------------------------------

data ModuleSummary = ModuleSummary
     { expTicked :: !Int
     , expTotal  :: !Int
     , topFunTicked :: !Int
     , topFunTotal  :: !Int
     , altTicked :: !Int
     , altTotal  :: !Int
     }
     deriving (Show)


showModuleSummary :: (String, String, ModuleSummary) -> String
showModuleSummary (modName,fileName,modSummary) =
  "<tr>\n" ++
  "<td>&nbsp;&nbsp;<tt>module <a href=\"" ++ fileName ++ "\">"
                              ++ modName ++ "</a></tt></td>\n" ++
   showSummary (topFunTicked modSummary) (topFunTotal modSummary) ++
   showSummary (altTicked modSummary) (altTotal modSummary) ++
   showSummary (expTicked modSummary) (expTotal modSummary) ++
  "</tr>\n"

showTotalSummary :: ModuleSummary -> String
showTotalSummary modSummary =
  "<tr style=\"background: #e0e0e0\">\n" ++
  "<th align=left>&nbsp;&nbsp;Program Coverage Total</tt></th>\n" ++
   showSummary (topFunTicked modSummary) (topFunTotal modSummary) ++
   showSummary (altTicked modSummary) (altTotal modSummary) ++
   showSummary (expTicked modSummary) (expTotal modSummary) ++
  "</tr>\n"

showSummary :: (Integral t, Show t) => t -> t -> String
showSummary ticked total =
                "<td align=\"right\">" ++ showP (percent ticked total) ++ "</td>" ++
                "<td>" ++ show ticked ++ "/" ++ show total ++ "</td>" ++
                "<td width=100>" ++
                    (case percent ticked total of
                       Nothing -> "&nbsp;"
                       Just w -> bar w "bar"
                     )  ++ "</td>"
     where
        showP Nothing = "-&nbsp;"
        showP (Just x) = show x ++ "%"
        bar 0 _     = bar 100 "invbar"
        bar w inner = "<table cellpadding=0 cellspacing=0 width=\"100\" class=\"bar\">" ++
                         "<tr><td><table cellpadding=0 cellspacing=0 width=\"" ++ show w ++ "%\">" ++
                              "<tr><td height=12 class=" ++ show inner ++ "></td></tr>" ++
                              "</table></td></tr></table>"

percent :: (Integral a) => a -> a -> Maybe a
percent ticked total = if total == 0 then Nothing else Just (ticked * 100 `div` total)


instance Monoid ModuleSummary where
  mempty = ModuleSummary
                  { expTicked = 0
                  , expTotal  = 0
                  , topFunTicked = 0
                  , topFunTotal  = 0
                  , altTicked = 0
                  , altTotal  = 0
                  }
  mappend (ModuleSummary eTik1 eTot1 tTik1 tTot1 aTik1 aTot1)
          (ModuleSummary eTik2 eTot2 tTik2 tTot2 aTik2 aTot2)
     = ModuleSummary (eTik1 + eTik2) (eTot1 + eTot2) (tTik1 + tTik2) (tTot1 + tTot2) (aTik1 + aTik2) (aTot1 + aTot2)


------------------------------------------------------------------------------

writeFileUsing :: String -> String -> IO ()
writeFileUsing filename text = do
  let dest_dir = reverse . dropWhile (\ x -> x /= '/') . reverse $ filename

-- We need to check for the dest_dir each time, because we use sub-dirs for
-- packages, and a single .tix file might contain information about
-- many package.

  -- create the dest_dir if needed
  when (not (null dest_dir)) $
    createDirectoryIfMissing True dest_dir

  writeFile filename text

------------------------------------------------------------------------------
-- global color pallete

red,green,yellow :: String
red    = "#f20913"
green  = "#60de51"
yellow = "yellow"

