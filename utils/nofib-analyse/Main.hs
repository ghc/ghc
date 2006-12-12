-----------------------------------------------------------------------------
-- $Id: Main.hs,v 1.10 2005/06/07 10:58:31 simonmar Exp $

-- (c) Simon Marlow 1997-2005
-----------------------------------------------------------------------------

module Main where

import GenUtils
import Slurp
import CmdLine

import Text.Printf
import Text.Html hiding ((!))
import qualified Text.Html as Html ((!))
import qualified Data.Map as Map
import Data.Map (Map)
import System.Console.GetOpt
import System.Exit      ( exitWith, ExitCode(..) )

import Numeric          ( showFloat, showFFloat, showSigned )
import Control.Monad
import Data.Maybe       ( isNothing )
import Data.Char
import System.IO
import Data.List

(<!) = (Html.!)

-----------------------------------------------------------------------------
-- Top level stuff

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)

usageHeader = "usage: nofib-analyse [OPTION...] <logfile1> <logfile2> ..."

main = do

 when (not (null cmdline_errors) || OptHelp `elem` flags) $
      die (concat cmdline_errors ++ usageInfo usageHeader argInfo)

 let { html  = OptHTMLOutput  `elem` flags;
       latex = OptLaTeXOutput `elem` flags;
       ascii = OptASCIIOutput `elem` flags
     }

 when (ascii && html)  $ die "Can't produce both ASCII and HTML"
 when (devs && nodevs) $ die "Can't both display and hide deviations"

 results <- parse_logs other_args

 summary_spec <- case [ cols | OptColumns cols <- flags ] of
                        []       -> return (pickSummary results)
                        (cols:_) -> namedColumns (split ',' cols)

 let summary_rows = case [ rows | OptRows rows <- flags ] of
                        [] -> Nothing
                        rows -> Just (split ',' (last rows))

 let column_headings = map (reverse . takeWhile (/= '/') . reverse) other_args

 -- sanity check
 sequence_ [ checkTimes prog res | table <- results,
                                   (prog,res) <- Map.toList table ]

 case () of
   _ | html      ->
        putStr (renderHtml (htmlPage results column_headings))
   _ | latex     ->
        putStr (latexOutput results column_headings summary_spec summary_rows)
   _ | otherwise ->
        putStr (asciiPage results column_headings summary_spec summary_rows)


parse_logs :: [String] -> IO [ResultTable]
parse_logs [] = do
        f <- hGetContents stdin
        return [parse_log f]
parse_logs log_files =
        mapM (\f -> do h <- openFile f ReadMode
                       c <- hGetContents h
                       return (parse_log c)) log_files

-----------------------------------------------------------------------------
-- List of tables we're going to generate

data PerProgTableSpec =
        forall a . Result a =>
           SpecP
                String                  -- Name of the table
                String                  -- Short name (for column heading)
                String                  -- HTML tag for the table
                (Results -> Maybe a)    -- How to get the result
                (Results -> Status)     -- How to get the status of this result
                (a -> Bool)             -- Result within reasonable limits?

data PerModuleTableSpec =
        forall a . Result a =>
           SpecM
                String                  -- Name of the table
                String                  -- HTML tag for the table
                (Results -> Map String a)       -- get the module map
                (a -> Bool)             -- Result within reasonable limits?

-- The various per-program aspects of execution that we can generate results for.
size_spec    = SpecP "Binary Sizes" "Size" "binary-sizes" binary_size compile_status always_ok
alloc_spec   = SpecP "Allocations" "Allocs" "allocations" allocs run_status always_ok
runtime_spec = SpecP "Run Time" "Runtime" "run-times" (mean run_time) run_status time_ok
muttime_spec = SpecP "Mutator Time" "MutTime" "mutator-time" (mean mut_time) run_status time_ok
gctime_spec  = SpecP "GC Time" "GCTime" "gc-time" (mean gc_time) run_status time_ok
gcwork_spec  = SpecP "GC Work" "GCWork" "gc-work" gc_work run_status always_ok
instrs_spec  = SpecP "Instructions" "Instrs" "instrs" instrs run_status always_ok
mreads_spec  = SpecP "Memory Reads" "Reads" "mem-reads" mem_reads run_status always_ok
mwrite_spec  = SpecP "Memory Writes" "Writes" "mem-writes" mem_writes run_status always_ok
cmiss_spec   = SpecP "Cache Misses" "Misses" "cache-misses" cache_misses run_status always_ok

all_specs = [
  size_spec,
  alloc_spec,
  runtime_spec,
  muttime_spec,
  gctime_spec,
  gcwork_spec,
  instrs_spec,
  mreads_spec,
  mwrite_spec,
  cmiss_spec
  ]

namedColumns :: [String] -> IO [PerProgTableSpec]
namedColumns ss = mapM findSpec ss
  where findSpec s =
           case [ spec | spec@(SpecP _ short_name _ _ _ _) <- all_specs,
                         short_name == s ] of
                [] -> die ("unknown column: " ++ s)
                (spec:_) -> return spec

mean :: (Results -> [Float]) -> Results -> Maybe Float
mean f results = go (f results)
  where go [] = Nothing
        go fs = Just (foldl' (+) 0 fs / fromIntegral (length fs))

-- Look for bogus-looking times: On Linux we occasionally get timing results
-- that are bizarrely low, and skew the average.
checkTimes :: String -> Results -> IO ()
checkTimes prog results = do
  check "run time" (run_time results)
  check "mut time" (mut_time results)
  check "GC time" (gc_time results)
  where
        check kind ts
           | any strange ts =
                hPutStrLn stderr ("warning: dubious " ++ kind
                                   ++ " results for " ++ prog
                                   ++ ": " ++ show ts)
           | otherwise = return ()
           where strange t = any (\r -> time_ok r && r / t > 1.4) ts
                        -- looks for times that are >40% smaller than
                        -- any other.


-- These are the per-prog tables we want to generate
per_prog_result_tab =
        [ size_spec, alloc_spec, runtime_spec, muttime_spec, gctime_spec,
          gcwork_spec, instrs_spec, mreads_spec, mwrite_spec, cmiss_spec ]

-- A single summary table, giving comparison figures for a number of
-- aspects, each in its own column.  Only works when comparing two runs.
normal_summary_specs =
        [ size_spec, alloc_spec, runtime_spec ]

cachegrind_summary_specs =
        [ size_spec, alloc_spec, instrs_spec, mreads_spec, mwrite_spec ]

-- Pick an appropriate summary table: if we're cachegrinding, then
-- we're probably not interested in the runtime, but we are interested
-- in instructions, mem reads and mem writes (and vice-versa).
pickSummary :: [ResultTable] -> [PerProgTableSpec]
pickSummary rs
  | isNothing (instrs (head (Map.elems (head rs)))) = normal_summary_specs
  | otherwise = cachegrind_summary_specs

per_module_result_tab =
        [ SpecM "Module Sizes"  "mod-sizes"     module_size  always_ok
        , SpecM "Compile Times" "compile-time"  compile_time time_ok
        ]

always_ok :: a -> Bool
always_ok = const True

time_ok :: Float -> Bool
time_ok t = t > tooquick_threshold

-----------------------------------------------------------------------------
-- HTML page generation

--htmlPage :: Results -> [String] -> Html
htmlPage results args
   =  header << thetitle << reportTitle
          +++ hr
          +++ h1 << reportTitle
          +++ gen_menu
          +++ hr
          +++ body (gen_tables results args)

gen_menu = unordList (map (prog_menu_item) per_prog_result_tab
                      ++ map (module_menu_item) per_module_result_tab)

prog_menu_item (SpecP name _ anc _ _ _) = anchor <! [href ('#':anc)] << name
module_menu_item (SpecM name anc _ _) = anchor <! [href ('#':anc)] << name

gen_tables results args =
  foldr1 (+++) (map (htmlGenProgTable results args) per_prog_result_tab)
  +++ foldr1 (+++) (map (htmlGenModTable results args) per_module_result_tab)

htmlGenProgTable results args (SpecP title _ anc get_result get_status result_ok)
  =   sectHeading title anc
  +++ font <! [size "1"]
        << mkTable (htmlShowResults results args get_result get_status result_ok)
  +++ hr

htmlGenModTable results args (SpecM title anc get_result result_ok)
  =   sectHeading title anc
  +++ font <![size "1"]
        << mkTable (htmlShowMultiResults results args get_result result_ok)
  +++ hr

sectHeading :: String -> String -> Html
sectHeading s nm = h2 << anchor <! [name nm] << s

htmlShowResults
    :: Result a
        => [ResultTable]
        -> [String]
        -> (Results -> Maybe a)
        -> (Results -> Status)
        -> (a -> Bool)
        -> HtmlTable

htmlShowResults (r:rs) ss f stat result_ok
  =   tabHeader ss
  </> aboves (zipWith tableRow [1..] results_per_prog)
  </> aboves ((if nodevs then []
                         else [tableRow (-1) ("-1 s.d.", lows),
                               tableRow (-1) ("+1 s.d.", highs)])
                    ++ [tableRow (-1) ("Average", gms)])
 where
        -- results_per_prog :: [ (String,[BoxValue a]) ]
        results_per_prog = map (calc_result rs f stat result_ok) (Map.toList r)

        results_per_run  = transpose (map snd results_per_prog)
        (lows,gms,highs) = unzip3 (map calc_gmsd results_per_run)

htmlShowMultiResults
    :: Result a
        => [ResultTable]
        -> [String]
        -> (Results -> Map String a)
        -> (a -> Bool)
        -> HtmlTable

htmlShowMultiResults (r:rs) ss f result_ok =
        multiTabHeader ss
         </> aboves (map show_results_for_prog results_per_prog_mod_run)
         </> aboves ((if nodevs then []
                                      else [td << bold << "-1 s.d."
                                            <-> tableRow (-1) ("", lows),
                                            td << bold << "+1 s.d."
                                            <-> tableRow (-1) ("", highs)])
                           ++ [td << bold << "Average"
                               <-> tableRow (-1) ("", gms)])

  where
        base_results = Map.toList r :: [(String,Results)]

        -- results_per_prog_mod_run :: [(String,[(String,[BoxValue a])])]
        results_per_prog_mod_run = map get_results_for_prog base_results

        -- get_results_for_prog :: (String,Results) -> (String,[BoxValue a])
        get_results_for_prog (prog,r) = (prog, map get_results_for_mod (Map.toList (f r)))

           where fms = map get_run_results rs

                 get_run_results fm = case Map.lookup prog fm of
                                        Nothing  -> Map.empty
                                        Just res -> f res

                 get_results_for_mod (id,attr) = calc_result fms Just (const Success)
                                                             result_ok (id,attr)

        show_results_for_prog (prog,mrs) =
            td <! [valign "top"] << bold << prog
            <-> (if null mrs then
                   td << "(no modules compiled)"
                 else
                   toHtml (aboves (map (tableRow 0) mrs)))

        results_per_run  = transpose [xs | (_,mods) <- results_per_prog_mod_run,
                                           (_,xs) <- mods]
        (lows,gms,highs) = unzip3 (map calc_gmsd results_per_run)

tableRow :: Int -> (String, [BoxValue]) -> HtmlTable
tableRow row_no (prog, results)
        =   td <! [bgcolor left_column_color] << prog
        <-> besides (map (\s -> td <! [align "right", clr] << showBox s)
                                results)
  where clr | row_no < 0  = bgcolor average_row_color
            | even row_no = bgcolor even_row_color
            | otherwise   = bgcolor odd_row_color

left_column_color = "#d0d0ff"  -- light blue
odd_row_color     = "#d0d0ff"  -- light blue
even_row_color    = "#f0f0ff"  -- v. light blue
average_row_color = "#ffd0d0"  -- light red

{-
findBest :: Result a => [BoxValue a] -> [(Bool,BoxValue a)]
findBest stuff@(Result base : rest)
  = map (\a -> (a==base, a))
  where
        best = snd (minimumBy (\a b -> fst a < fst b) no_pcnt_stuff

        no_pcnt_stuff = map unPcnt stuff

        unPcnt (r@(Percentage f) : rest) = (base * f/100, r) : unPcnt rest
        unPcnt (r@(Result a) : rest)     = (a, r) : unPcnt rest
        unPcnt (_ : rest)                = unPcnt rest
-}

logHeaders ss
  = besides (map (\s -> (td <! [align "right", width "100"] << bold << s)) ss)

mkTable t = table <! [cellspacing 0, cellpadding 0, border 0] << t

tabHeader ss
  =   (td <! [align "left", width "100"] << bold << "Program")
  <-> logHeaders ss

multiTabHeader ss
  =   (td <! [align "left", width "100"] << bold << "Program")
  <-> (td <! [align "left", width "100"] << bold << "Module")
  <-> logHeaders ss

-- Calculate a color ranging from bright blue for -100% to bright red for +100%.

calcColor :: Int -> String
calcColor p | p >= 0    = "#"     ++ (showHex red 2 "0000")
              | otherwise = "#0000" ++ (showHex blue 2 "")
        where red  = p * 255 `div` 100
              blue = (-p) * 255 `div` 100

showHex 0 f s = if f > 0 then take f (repeat '0') ++ s else s
showHex i f s = showHex (i `div` 16) (f-1) (hexDig (i `mod` 16) : s)

hexDig i | i > 10 = chr (i-10 + ord 'a')
         | otherwise = chr (i + ord '0')

-----------------------------------------------------------------------------
-- LaTeX table generation (just the summary for now)

latexOutput results args summary_spec summary_rows =
   (if (length results == 2)
        then ascii_summary_table True results summary_spec summary_rows
            . str "\n\n"
        else id) ""


-----------------------------------------------------------------------------
-- ASCII page generation

asciiPage results args summary_spec summary_rows =
  ( str reportTitle
  . str "\n\n"
     -- only show the summary table if we're comparing two runs
  . (if (length results == 2)
        then ascii_summary_table False results summary_spec summary_rows . str "\n\n"
        else id)
  . interleave "\n\n" (map (asciiGenProgTable results args) per_prog_result_tab)
  . str "\n"
  . interleave "\n\n" (map (asciiGenModTable results args)  per_module_result_tab)
  ) "\n"

asciiGenProgTable results args (SpecP title _ anc get_result get_status result_ok)
  = str title
  . str "\n"
  . ascii_show_results results args get_result get_status result_ok

asciiGenModTable results args (SpecM title anc get_result result_ok)
  = str title
  . str "\n"
  . ascii_show_multi_results results args get_result result_ok

ascii_header width ss
        = str "\n-------------------------------------------------------------------------------\n"
        . str (rjustify 15 "Program")
        . str (space 5)
        . foldr (.) id (map (str . rjustify width) ss)
        . str "\n-------------------------------------------------------------------------------\n"

ascii_show_results
   :: Result a
        => [ResultTable]
        -> [String]
        -> (Results -> Maybe a)
        -> (Results -> Status)
        -> (a -> Bool)
        -> ShowS

ascii_show_results (r:rs) ss f stat result_ok
        = ascii_header fIELD_WIDTH ss
        . interleave "\n" (map show_per_prog_results results_per_prog)
        . if nodevs then id
                    else   str "\n"
                         . show_per_prog_results ("-1 s.d.",lows)
                         . str "\n"
                         . show_per_prog_results ("+1 s.d.",highs)
        . str "\n"
        . show_per_prog_results ("Average",gms)
 where
        -- results_per_prog :: [ (String,[BoxValue a]) ]
        results_per_prog = map (calc_result rs f stat result_ok) (Map.toList r)

        results_per_run  = transpose (map snd results_per_prog)
        (lows,gms,highs) = unzip3 (map calc_gmsd results_per_run)

-- A summary table, useful only when we are comparing two runs.  This table
-- shows a number of different result categories, one per column.
ascii_summary_table
        :: Bool                         -- generate a LaTeX table?
        -> [ResultTable]
        -> [PerProgTableSpec]
        -> Maybe [String]
        -> ShowS
ascii_summary_table latex (r1:r2:_) specs mb_restrict
  | latex     = makeLatexTable (rows ++ TableLine : av_rows)
  | otherwise =
       makeTable (table_layout (length specs) width)
          (TableLine : TableRow header : TableLine : rows ++ TableLine : av_rows)
  where
        header = BoxString "Program" : map BoxString headings

        (headings, columns, av_cols) = unzip3 (map calc_col specs)
        av_heads = [BoxString "Min", BoxString "Max", BoxString "Geometric Mean"]
        baseline = Map.toList r1
        progs   = map BoxString (Map.keys r1)
        rows0   = map TableRow (zipWith (:) progs (transpose columns))

        rows1 = restrictRows mb_restrict rows0

        rows | latex     = mungeForLaTeX rows1
             | otherwise = rows1

        av_rows = map TableRow (zipWith (:) av_heads (transpose av_cols))
        width   = 10

        calc_col (SpecP _ heading _ getr gets ok)
          = (heading, column, [min,max,mean]) -- throw away the baseline result
          where (_, boxes) = unzip (map calc_one_result baseline)
                calc_one_result = calc_result [r2] getr gets ok
                column = map (\(_:b:_) -> b) boxes
                (_,mean,_) = calc_gmsd column
                (min,max) = calc_minmax column

restrictRows :: Maybe [String] -> [TableRow] -> [TableRow]
restrictRows Nothing rows = rows
restrictRows (Just these) rows = filter keep_it rows
  where keep_it (TableRow (BoxString s: _)) = s `elem` these
        keep_it TableLine = True
        keep_it _ = False

mungeForLaTeX :: [TableRow] -> [TableRow]
mungeForLaTeX = map transrow
   where
        transrow (TableRow boxes) = TableRow (map transbox boxes)
        transrow row = row

        transbox (BoxString s) = BoxString (foldr transchar "" s)
        transbox box = box

        transchar '_' s = '\\':'_':s
        transchar c s = c:s

table_layout n width =
  (str . rjustify 15) :
  (\s -> str (space 5) . str (rjustify width s)) :
  replicate (n-1) (str . rjustify width)

ascii_show_multi_results
   :: Result a
        => [ResultTable]
        -> [String]
        -> (Results -> Map String a)
        -> (a -> Bool)
        -> ShowS

ascii_show_multi_results (r:rs) ss f result_ok
        = ascii_header fIELD_WIDTH ss
        . interleave "\n" (map show_results_for_prog results_per_prog_mod_run)
        . str "\n"
        . if nodevs then id
                    else   str "\n"
                         . show_per_prog_results ("-1 s.d.",lows)
                         . str "\n"
                         . show_per_prog_results ("+1 s.d.",highs)
        . str "\n"
        . show_per_prog_results ("Average",gms)
  where
        base_results = Map.toList r :: [(String,Results)]

        -- results_per_prog_mod_run :: [(String,[(String,[BoxValue a])])]
        results_per_prog_mod_run = map get_results_for_prog base_results

        -- get_results_for_prog :: (String,Results) -> (String,[BoxValue a])
        get_results_for_prog (prog,r) = (prog, map get_results_for_mod (Map.toList (f r)))

           where fms = map get_run_results rs

                 get_run_results fm = case Map.lookup prog fm of
                                        Nothing  -> Map.empty
                                        Just res -> f res

                 get_results_for_mod (id,attr) = calc_result fms Just (const Success)
                                                             result_ok (id,attr)

        show_results_for_prog (prog,mrs) =
              str ("\n"++prog++"\n")
            . (if null mrs then
                   str "(no modules compiled)\n"
                 else
                   interleave "\n" (map show_per_prog_results mrs))

        results_per_run  = transpose [xs | (_,mods) <- results_per_prog_mod_run,
                                           (_,xs) <- mods]
        (lows,gms,highs) = unzip3 (map calc_gmsd results_per_run)


show_per_prog_results :: (String, [BoxValue]) -> ShowS
show_per_prog_results = show_per_prog_results_width fIELD_WIDTH

show_per_prog_results_width width (prog,results)
        = str (rjustify 15 prog)
        . str (space 5)
        . foldr (.) id (map (str . rjustify width . showBox) results)

-- ---------------------------------------------------------------------------
-- Generic stuff for results generation

-- calc_result is a nice exercise in higher-order programming...
calc_result
  :: Result a
        => [Map String b]               -- accumulated results
        -> (b -> Maybe a)               -- get a result from the b
        -> (b -> Status)                -- get a status from the b
        -> (a -> Bool)                  -- is this result ok?
        -> (String,b)                   -- the baseline result
        -> (String,[BoxValue])

calc_result rts get_maybe_a get_stat result_ok (prog,base_r) =
        (prog, (just_result baseline base_stat :

          let
                rts' = map (\rt -> get_stuff (Map.lookup prog rt)) rts

                get_stuff Nothing  = (Nothing, NotDone)
                get_stuff (Just r) = (get_maybe_a r, get_stat r)
          in
          (
          case baseline of
                Just base | result_ok base
                   -> map (\(r,s) -> percentage  r s base) rts'
                _other
                   -> map (\(r,s) -> just_result r s) rts'
           )))
 where
        baseline  = get_maybe_a base_r
        base_stat = get_stat base_r

        just_result Nothing  s = RunFailed s
        just_result (Just a) s = toBox a

        percentage Nothing   s base = RunFailed s
        percentage (Just a)  s base = Percentage
                                         (convert_to_percentage base a)
-----------------------------------------------------------------------------
-- Calculating geometric means and standard deviations

{-
This is done using the log method, to avoid needing really large
intermediate results.  The formula for a geometric mean is

        (a1 * .... * an) ^ 1/n

which is equivalent to

        e ^ ( (log a1 + ... + log an) / n )

where log is the natural logarithm function.

Similarly, to compute the geometric standard deviation we compute the
deviation of each log, take the root-mean-square, and take the
exponential again:

        e ^ sqrt( ( sqr(log a1 - lbar) + ... + sqr(log an - lbar) ) / n )

where lbar is the mean log,

        (log a1 + ... + log an) / n

This is a *factor*: i.e., the 1 s.d. points are (gm/sdf,gm*sdf); do
not subtract 100 from gm before performing this calculation.

We therefore return a (low, mean, high) triple.

-}

calc_gmsd :: [BoxValue] -> (BoxValue, BoxValue, BoxValue)
calc_gmsd xs
  | null percentages = (RunFailed NotDone, RunFailed NotDone, RunFailed NotDone)
  | otherwise        = let sqr x = x * x
                           len   = fromIntegral (length percentages)
                           logs  = map log percentages
                           lbar  = sum logs / len
                           devs  = map (sqr . (lbar-)) logs
                           dbar  = sum devs / len
                           gm    = exp lbar
                           sdf   = exp (sqrt dbar)
                       in
                       (Percentage (gm/sdf),
                        Percentage gm,
                        Percentage (gm*sdf))
 where
  percentages = [ if f < 5 then 5 else f | Percentage f <- xs ]
        -- can't do log(0.0), so exclude zeros
        -- small values have inordinate effects so cap at -95%.

calc_minmax :: [BoxValue] -> (BoxValue, BoxValue)
calc_minmax xs
 | null percentages = (RunFailed NotDone, RunFailed NotDone)
 | otherwise = (Percentage (minimum percentages),
                Percentage (maximum percentages))
 where
  percentages = [ if f < 5 then 5 else f | Percentage f <- xs ]


-----------------------------------------------------------------------------
-- Show the Results

class Num a => Result a where
        toBox :: a -> BoxValue
        convert_to_percentage :: a -> a -> Float

-- We assume an Int is a size, and print it in kilobytes.

instance Result Int where
        convert_to_percentage 0 size = 100
        convert_to_percentage base size = (fromIntegral size / fromIntegral base) * 100

        toBox = BoxInt

instance Result Integer where
        convert_to_percentage 0 size = 100
        convert_to_percentage base size = (fromInteger size / fromInteger base) * 100
        toBox = BoxInteger


instance Result Float where
        convert_to_percentage 0.0 size = 100.0
        convert_to_percentage base size = size / base * 100

        toBox = BoxFloat

-- -----------------------------------------------------------------------------
-- BoxValues

-- The contents of a box in a table
data BoxValue
  = RunFailed Status
  | Percentage Float
  | BoxFloat Float
  | BoxInt Int
  | BoxInteger Integer
  | BoxString String

showBox :: BoxValue -> String
showBox (RunFailed stat) = show_stat stat
showBox (Percentage f)   = show_pcntage f
showBox (BoxFloat f)     = printf "%.2f" f
showBox (BoxInt n)       = show (n `div` 1024) ++ "k"
showBox (BoxInteger n)   = show (n `div` 1024) ++ "k"
showBox (BoxString s)    = s

instance Show BoxValue where { show = showBox }

show_pcntage n = show_float_signed (n-100) ++ "%"

show_float_signed n
  | n >= 0    = printf "+%.1f" n
  | otherwise = printf "%.1f" n

show_stat Success     = "(no result)"
show_stat WrongStdout = "(stdout)"
show_stat WrongStderr = "(stderr)"
show_stat (Exit x)    = "exit(" ++ show x ++")"
show_stat OutOfHeap   = "(heap)"
show_stat OutOfStack  = "(stack)"
show_stat NotDone     = "-----"

-- -----------------------------------------------------------------------------
-- Table layout

data TableRow
  = TableRow [BoxValue]
  | TableLine

type Layout = [String -> ShowS]

makeTable :: Layout -> [TableRow] -> ShowS
makeTable p = interleave "\n" . map do_row
  where do_row (TableRow boxes) = applyLayout p boxes
        do_row TableLine = str (take 80 (repeat '-'))

makeLatexTable :: [TableRow] -> ShowS
makeLatexTable = foldr (.) id . map do_row
  where do_row (TableRow boxes)
           = applyLayout latexTableLayout boxes . str "\\\\\n"
        do_row TableLine
           = str "\\hline\n"

latexTableLayout :: Layout
latexTableLayout = box : repeat (box . (" & "++))
  where box s = str (foldr transchar "" s)

        transchar '%' s = s  -- leave out the percentage signs
        transchar c   s = c : s

applyLayout :: Layout -> [BoxValue] -> ShowS
applyLayout layout values =
 foldr (.) id [ f (show val) | (val,f) <- zip values layout ]

-- -----------------------------------------------------------------------------
-- General Utils

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

str = showString

interleave s = foldr1 (\a b -> a . str s . b)

fIELD_WIDTH = 16 :: Int

-----------------------------------------------------------------------------
