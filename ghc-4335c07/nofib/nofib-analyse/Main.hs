{-# LANGUAGE RankNTypes, ExistentialQuantification, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- (c) Simon Marlow 1997-2005
-----------------------------------------------------------------------------

module Main where

import GenUtils
import Slurp
import CmdLine

import Text.Printf
import qualified Data.Map as Map
import Data.Map (Map)
import System.Exit      ( exitWith, ExitCode(..) )

import Control.Monad
import Data.Maybe       ( isNothing )
import System.IO
import Data.List

-----------------------------------------------------------------------------
-- Top level stuff

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)

data Normalise = NormalisePercent | NormaliseRatio | NormaliseNone

main :: IO ()
main = do

 when (not (null cmdline_errors) || OptHelp `elem` flags) $
      die (concat cmdline_errors ++ usage)

 norm <- case [ n | OptNormalise n <- flags ] of
                 []          -> return NormalisePercent
                 ["percent"] -> return NormalisePercent
                 ["ratio"]   -> return NormaliseRatio
                 ["none"]    -> return NormaliseNone
                 _           -> die ("unrecognised value for --normalise\n" ++ usage)

 let { latex = [ t | OptLaTeXOutput t <- flags ];
       ascii = OptASCIIOutput `elem` flags;
       csv   = [ t | OptCSV t <- flags ];
       stddev = OptStdDev  `elem` flags;
       inc_baseline = OptShowBaseline  `elem` flags
     }

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
 sequence_ [ checkTimes prog res | result_table <- results,
                                   (prog,res) <- Map.toList result_table ]

 case () of
   _ | not (null csv) ->
        putStr (csvTable results (head csv) norm stddev)
   _ | not (null latex) ->
        putStr (latexOutput results (head latex) column_headings summary_spec summary_rows norm inc_baseline)
   _ | otherwise ->
        putStr (asciiPage results column_headings summary_spec summary_rows norm)


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
                String                  -- HTML tag for the table (currently unused)
                (Results -> Maybe a)    -- How to get the result
                (Results -> Status)     -- How to get the status of this result
                (a -> Bool)             -- Result within reasonable limits?

data PerModuleTableSpec =
        forall a . Result a =>
           SpecM
                String                  -- Name of the table
                String                  -- Short name (for column heading)
                String                  -- HTML tag for the table (currently unused)
                (Results -> Map String a)       -- get the module map
                (a -> Bool)             -- Result within reasonable limits?

aggregateModuleTable :: PerModuleTableSpec -> PerProgTableSpec
aggregateModuleTable (SpecM long_name short_name html_tag get_results is_ok)
 = SpecP long_name short_name html_tag (mapToMaybe . get_results) (const Success) is_ok
  where
    mapToMaybe m = if Map.null m then Nothing else Just (sumResult (Map.elems m))

progTableHasName :: PerProgTableSpec -> String -> Bool
progTableHasName (SpecP ln sn _ _ _ _) n = n == ln || n == sn

modTableHasName :: PerModuleTableSpec -> String -> Bool
modTableHasName (SpecM ln sn _ _ _) n = n == ln || n == sn

-- The various per-program aspects of execution that we can generate results for.
size_spec, alloc_spec, runtime_spec, elapsedtime_spec, muttime_spec, mutetime_spec,
    gctime_spec, gcelap_spec,
    gcwork_spec, instrs_spec, mreads_spec, mwrite_spec, cmiss_spec,
    gc0time_spec, gc0elap_spec, gc1time_spec, gc1elap_spec, balance_spec, totmem_spec
        :: PerProgTableSpec
size_spec    = SpecP "Binary Sizes" "Size" "binary-sizes" binary_size compile_status always_ok
alloc_spec   = SpecP "Allocations" "Allocs" "allocations" (meanInt allocs) run_status always_ok
runtime_spec = SpecP "Run Time" "Runtime" "run-times" (mean run_time) run_status mean_time_ok
elapsedtime_spec = SpecP "Elapsed Time" "Elapsed" "elapsed-times" (mean elapsed_time) run_status mean_time_ok
muttime_spec = SpecP "Mutator Time" "MutTime" "mutator-time" (mean mut_time) run_status mean_time_ok
mutetime_spec = SpecP "Mutator Elapsed Time" "MutETime" "mutator-elapsed-time" (mean mut_elapsed_time) run_status mean_time_ok
gctime_spec  = SpecP "GC Time" "GCTime" "gc-time" (mean gc_time) run_status mean_time_ok
gcelap_spec  = SpecP "GC Elapsed Time" "GCETime" "gc-elapsed-time" (mean gc_elapsed_time) run_status mean_time_ok
gc0count_spec  = SpecP "GC(0) Count" "GC0Count" "gc0-count" (meanInt gc0_count) run_status always_ok
gc0time_spec  = SpecP "GC(0) Time" "GC0Time" "gc0-time" (mean gc0_time) run_status mean_time_ok
gc0elap_spec  = SpecP "GC(0) Elapsed Time" "GC0ETime" "gc0-elapsed-time" (mean gc0_elapsed_time) run_status mean_time_ok
gc1count_spec  = SpecP "GC(1) Count" "GC1Count" "gc1-count" (meanInt gc1_count) run_status always_ok
gc1time_spec  = SpecP "GC(1) Time" "GC1Time" "gc1-time" (mean gc1_time) run_status mean_time_ok
gc1elap_spec  = SpecP "GC(1) Elapsed Time" "GC1ETime" "gc1-elapsed-time" (mean gc1_elapsed_time) run_status mean_time_ok
balance_spec  = SpecP "GC work balance" "Balance" "balance" (mean balance) run_status mean_time_ok
gcwork_spec  = SpecP "GC Work" "GCWork" "gc-work" (meanInt gc_work) run_status always_ok
instrs_spec  = SpecP "Instructions" "Instrs" "instrs" instrs run_status always_ok
mreads_spec  = SpecP "Memory Reads" "Reads" "mem-reads" mem_reads run_status always_ok
mwrite_spec  = SpecP "Memory Writes" "Writes" "mem-writes" mem_writes run_status always_ok
cmiss_spec   = SpecP "Cache Misses" "Misses" "cache-misses" cache_misses run_status always_ok
totmem_spec   = SpecP "Total Memory in use" "TotalMem" "total-mem" (meanInt total_memory) run_status always_ok

all_specs :: [PerProgTableSpec]
all_specs = [
  size_spec,
  alloc_spec,
  runtime_spec,
  elapsedtime_spec,
  muttime_spec,
  mutetime_spec,
  gctime_spec,
  gcelap_spec,
  gc0count_spec,
  gc0time_spec,
  gc0elap_spec,
  gc1count_spec,
  gc1time_spec,
  gc1elap_spec,
  balance_spec,
  gcwork_spec,
  instrs_spec,
  mreads_spec,
  mwrite_spec,
  cmiss_spec,
  totmem_spec
  ]

namedColumns :: [String] -> IO [PerProgTableSpec]
namedColumns ss = mapM findSpec ss
  where findSpec s =
           case filter (`progTableHasName` s) all_specs ++
                map aggregateModuleTable (filter (`modTableHasName` s) per_module_result_tab)
           of
                [] -> die ("unknown column: " ++ s)
                (spec:_) -> return spec

mean :: (Results -> [Float]) -> Results -> Maybe (MeanStdDev Float)
mean f results = go (f results)
  where go [] = Nothing
        go fs = Just (MeanStdDev mn stddev)
         where mn = sn / n
               stddev = (sqrt (n * sum (map (^2) fs) - sn^2)) / n
               sn = foldl' (+) 0 fs
               n = fromIntegral (length fs)

meanInt :: Integral a => (Results -> [a]) -> Results -> Maybe (MeanStdDev a)
meanInt f results = go (f results)
  where go [] = Nothing
        go fs = Just (MeanStdDev mn 0)
          where mn = foldl' (+) 0 fs `quot` fromIntegral (length fs)

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
per_prog_result_tab :: [PerProgTableSpec]
per_prog_result_tab =
        [ size_spec, alloc_spec, runtime_spec, elapsedtime_spec, muttime_spec, mutetime_spec, gctime_spec,
          gcelap_spec, gc0count_spec, gc0time_spec, gc0elap_spec, gc1count_spec, gc1time_spec, gc1elap_spec,
          gcwork_spec, balance_spec, instrs_spec, mreads_spec, mwrite_spec, cmiss_spec, totmem_spec]

-- A single summary table, giving comparison figures for a number of
-- aspects, each in its own column.  Only works when comparing at least two runs.
normal_summary_specs :: [PerProgTableSpec]
normal_summary_specs =
        [ size_spec, alloc_spec, runtime_spec, elapsedtime_spec, totmem_spec ]

cachegrind_summary_specs :: [PerProgTableSpec]
cachegrind_summary_specs =
        [ size_spec, alloc_spec, instrs_spec, mreads_spec, mwrite_spec ]

-- Pick an appropriate summary table: if we're cachegrinding, then
-- we're probably not interested in the runtime, but we are interested
-- in instructions, mem reads and mem writes (and vice-versa).
pickSummary :: [ResultTable] -> [PerProgTableSpec]
pickSummary rs
  | isNothing (instrs (head (Map.elems (head rs)))) = normal_summary_specs
  | otherwise = cachegrind_summary_specs

per_module_result_tab :: [PerModuleTableSpec]
per_module_result_tab =
        [ SpecM "Module Sizes" "Mod Size" "mod-sizes" module_size always_ok
        , SpecM "Compile Times" "Comp. Time" "compile-time" compile_time time_ok
        , SpecM "Compile Allocations" "Comp. Alloc" "compile-allocations" compile_allocs always_ok
        ]

always_ok :: a -> Bool
always_ok = const True

mean_time_ok :: (MeanStdDev Float) -> Bool
mean_time_ok  = time_ok . float

time_ok :: Float -> Bool
time_ok t = t > tooquick_threshold

-----------------------------------------------------------------------------
-- LaTeX table generation

latexOutput :: [ResultTable] -> Maybe String -> [String] -> [PerProgTableSpec]
            -> Maybe [String] -> Normalise -> Bool -> String

latexOutput results (Just table_name) _ _ _ norm inc_baseline
  = case
        [ latexProgTable results spec norm inc_baseline "\n"
        | spec <- per_prog_result_tab, spec `progTableHasName` table_name]
        ++
        [ latexModTable results spec norm inc_baseline "\n"
        | spec <- per_module_result_tab, spec `modTableHasName` table_name]
    of
        [] -> error ("can't find table named: " ++ table_name)
        (r:_) -> r

latexOutput results Nothing column_headings summary_spec summary_rows _ _ =
   (if (length results >= 2)
        then ascii_summary_table True results column_headings summary_spec summary_rows
            . str "\n\n"
        else id) ""


latexProgTable :: [ResultTable] -> PerProgTableSpec -> Normalise -> Bool -> ShowS
latexProgTable results (SpecP _long_name _ _ get_result get_status result_ok) norm inc_baseline
  = latex_show_results results get_result get_status result_ok norm inc_baseline

latex_show_results
   :: Result a
        => [ResultTable]
        -> (Results -> Maybe a)
        -> (Results -> Status)
        -> (a -> Bool)
        -> Normalise
        -> Bool
        -> ShowS

latex_show_results []      _ _    _ _ _
 = error "latex_show_results: Can't happen?"
latex_show_results (r:rs) f stat _result_ok norm inc_baseline
        = makeLatexTable $
             [ TableRow (BoxString prog : boxes) | 
               (prog,boxes) <- results_per_prog ] ++
             if nodevs then [] else
             [ TableLine,
               TableRow (BoxString "Min" : mins),
               TableRow (BoxString "Max" : maxs),
               TableRow (BoxString "Geometric Mean" : gms) ]
 where
        -- results_per_prog :: [ (String,[BoxValue a]) ]
        results_per_prog = [ (prog, if inc_baseline then xs else tail xs)
                           | (prog,xs) <- map calc (Map.toList r) ]
        calc = calc_result rs f stat (const True) (normalise norm)

        results_per_run    = transpose (map snd results_per_prog)
        (_lows,gms,_highs) = unzip3 (map calc_gmsd results_per_run)
        (mins, maxs)       = unzip  (map calc_minmax results_per_run)

normalise :: Result a => Normalise -> a -> a -> BoxValue
normalise norm = case norm of
             NormalisePercent -> convert_to_percentage
             NormaliseRatio   -> normalise_to_base
             NormaliseNone    -> \_base res -> toBox res

latexModTable :: [ResultTable] -> PerModuleTableSpec -> Normalise -> Bool -> ShowS
latexModTable results (SpecM _ _ _ get_results result_ok) norm inc_baseline
  = latex_show_multi_results results get_results result_ok norm inc_baseline

latex_show_multi_results
   :: Result a
        => [ResultTable]
        -> (Results -> Map String a)
        -> (a -> Bool)
        -> Normalise
        -> Bool
        -> ShowS

latex_show_multi_results []      _ _    _ _
 = error "latex_show_multi_results: Can't happen?"
latex_show_multi_results (r:rs) f _result_ok norm inc_baseline
        = makeLatexTable $
             [ TableRow (BoxString (prog ++ "." ++ mod) : boxes)
             | (prog,mod,boxes) <- results_per_prog_and_mod ] ++
             if nodevs then [] else
             [ TableLine,
               TableRow (BoxString "Min" : mins),
               TableRow (BoxString "Max" : maxs),
               TableRow (BoxString "Geometric Mean" : gms) ]
 where
        -- results_per_prog_and_mod :: [ (String,String,[BoxValue a]) ]
        results_per_prog_and_mod =
            [ (prog, mod, if inc_baseline then xs else tail xs)
            | (prog, results) <- Map.toList r
            , (mod, result) <- Map.toList (f results)
            , let fms = map (maybe Map.empty f . Map.lookup prog) rs
            , let (_, xs) = calc_result fms Just (const Success) (const True) (normalise norm) (mod, result)
            ]
        results_per_run    = transpose [ r | (_,_,r) <- results_per_prog_and_mod ]
        (_lows,gms,_highs) = unzip3 (map calc_gmsd results_per_run)
        (mins, maxs)       = unzip  (map calc_minmax results_per_run)

-----------------------------------------------------------------------------
-- ASCII page generation

asciiPage :: [ResultTable] -> [String] -> [PerProgTableSpec] -> Maybe [String]
          -> Normalise
          -> String
asciiPage results args summary_spec summary_rows norm =
  ( str reportTitle
  . str "\n\n"
     -- only show the summary table if we're comparing at least two runs
  . (if (length results >= 2)
        then ascii_summary_table False results args summary_spec summary_rows . str "\n\n"
        else id)
  . interleave "\n\n" (map (asciiGenProgTable results args norm) per_prog_result_tab)
  . str "\n"
  . interleave "\n\n" (map (asciiGenModTable results args)  per_module_result_tab)
  ) "\n"

asciiGenProgTable :: [ResultTable] -> [String] -> Normalise -> PerProgTableSpec -> ShowS
asciiGenProgTable results args norm (SpecP long_name _ _ get_result get_status result_ok)
  = str long_name
  . str "\n"
  . ascii_show_results results args get_result get_status result_ok norm

asciiGenModTable :: [ResultTable] -> [String] -> PerModuleTableSpec -> ShowS
asciiGenModTable results args (SpecM long_name _ _ get_result result_ok)
  = str long_name
  . str "\n"
  . ascii_show_multi_results results args get_result result_ok

ascii_header :: Int -> [String] -> ShowS
ascii_header w ss
        = str "\n-------------------------------------------------------------------------------\n"
        . str (rjustify 15 "Program")
        . str (space 5)
        . foldr (.) id (intersperse (str (space 1)) (map (str . rjustify w) ss))
        . str "\n-------------------------------------------------------------------------------\n"

ascii_show_results
   :: Result a
        => [ResultTable]
        -> [String]
        -> (Results -> Maybe a)
        -> (Results -> Status)
        -> (a -> Bool)
        -> Normalise
        -> ShowS

ascii_show_results []     _  _ _    _ _
 = error "ascii_show_results: Can't happen?"
ascii_show_results (r:rs) ss f stat result_ok norm
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
        results_per_prog = map (calc_result rs f stat result_ok (normalise norm)) (Map.toList r)

        results_per_run  = transpose (map snd results_per_prog)
        (lows,gms,highs) = unzip3 (map calc_gmsd results_per_run)

-- A summary table, useful only when we are comparing two runs.  This table
-- shows a number of different result categories, one per column.
ascii_summary_table
        :: Bool                         -- generate a LaTeX table?
        -> [ResultTable]
        -> [String]
        -> [PerProgTableSpec]
        -> Maybe [String]
        -> ShowS
ascii_summary_table _     []         _         _     _
 = error "ascii_summary_table: Can't happen?"
ascii_summary_table _     [_]        _         _     _
 = error "ascii_summary_table: Can't happen?"
ascii_summary_table latex (rbase:rs) (_:names) specs mb_restrict
  | latex     = makeLatexTable (rows ++ TableLine : av_rows)
  | otherwise =
       makeTable (table_layout (length specs * length rs) w) $
          [ TableLine
          , TableRow header_row ] ++
          [ TableRow header_row2 | length rs > 1] ++
          [ TableLine ] ++
          rows ++
          [ TableLine ] ++
          av_rows
  where
        header_row = BoxString "Program" : map BoxString headings
        header_row2 = BoxString "" : map BoxString headings2

        (headings, headings2, columns, av_cols) = unzip4 (concatMap calc_col_group specs)
        av_heads = [BoxString "Min", BoxString "Max", BoxString "Geometric Mean"]
        baseline = Map.toList rbase
        progs   = map BoxString (Map.keys rbase)
        rows0   = map TableRow (zipWith (:) progs (transpose columns))

        rows = restrictRows mb_restrict rows0

        av_rows = map TableRow (zipWith (:) av_heads (transpose av_cols))
        w   = sUMMARY_FIELD_WIDTH

        calc_col_group :: PerProgTableSpec -> [(String, String, [BoxValue], [BoxValue])]
        calc_col_group spec = [calc_col spec r n | (r,n) <- zip rs names]

        calc_col :: PerProgTableSpec -> ResultTable -> String -> (String, String, [BoxValue], [BoxValue])
        calc_col (SpecP _ heading _ getr gets ok) r n
            -- throw away the baseline result
          = (heading, n, column, [column_min, column_max, column_mean])
          where (_, boxes) = unzip (map calc_one_result baseline)
                calc_one_result = calc_result [r] getr gets ok convert_to_percentage
                column = map (\(_:b:_) -> b) boxes
                (_, column_mean, _) = calc_gmsd column
                (column_min, column_max) = calc_minmax column

restrictRows :: Maybe [String] -> [TableRow] -> [TableRow]
restrictRows Nothing rows = rows
restrictRows (Just these) rows = filter keep_it rows
  where keep_it (TableRow (BoxString s: _)) = s `elem` these
        keep_it TableLine = True
        keep_it _ = False

table_layout :: Int -> Int -> Layout
table_layout n w boxes = foldr (.) id $ intersperse (str (space 1)) $ zipWith ($) fns boxes
 where fns = (str . rjustify 15 . show ) :
             (\s -> str (space 5) . str (rjustify w (show s))) :
             replicate (n-1) (str . rjustify w . show)

ascii_show_multi_results
   :: Result a
        => [ResultTable]
        -> [String]
        -> (Results -> Map String a)
        -> (a -> Bool)
        -> ShowS

ascii_show_multi_results []     _  _ _
 = error "ascii_show_multi_results: Can't happen?"
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
        get_results_for_prog (prog, results)
            = (prog, map get_results_for_mod (Map.toList (f results)))

           where fms = map get_run_results rs

                 get_run_results fm = case Map.lookup prog fm of
                                        Nothing  -> Map.empty
                                        Just res -> f res

                 get_results_for_mod id_attr
                     = calc_result fms Just (const Success) result_ok convert_to_percentage id_attr

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

show_per_prog_results_width :: Int -> (String, [BoxValue]) -> ShowS
show_per_prog_results_width w (prog,results)
        = str (rjustify 15 prog)
        . str (space 5)
        . foldr (.) id (intersperse (str (space 1)) (map (str . rjustify w . showBox) results))

-- -----------------------------------------------------------------------------
-- CSV output

csvTable :: [ResultTable] -> String -> Normalise -> Bool -> String
csvTable results table_name norm stddev
  = case
        [ csvProgTable results spec norm stddev "\n"
        | spec <- per_prog_result_tab, spec `progTableHasName` table_name]
        ++
        [ csvModTable results spec norm stddev "\n"
        | spec <- per_module_result_tab, spec `modTableHasName` table_name]
    of
        [] -> error ("can't find table named: " ++ table_name)
        (r:_) -> r

csvProgTable :: [ResultTable] -> PerProgTableSpec -> Normalise -> Bool -> ShowS
csvProgTable results (SpecP _long_name _ _ get_result get_status result_ok)
             norm stddev
  = csv_show_results results get_result get_status result_ok norm stddev

csv_show_results
   :: Result a
        => [ResultTable]
        -> (Results -> Maybe a)
        -> (Results -> Status)
        -> (a -> Bool)
        -> Normalise
        -> Bool
        -> ShowS

csv_show_results []      _ _    _ _ _
 = error "csv_show_results: Can't happen?"
csv_show_results (r:rs) f stat _result_ok norm stddev
        = interleave "\n" results_per_prog
 where
        -- results_per_prog :: [ (String,[BoxValue a]) ]
        results_per_prog = map (result_line . calc) (Map.toList r)
        calc = calc_result rs f stat (const True) (normalise norm)

        result_line (prog,boxes)
          | stddev    = interleave "," (str prog : concat (map stddevbox boxes))
          | otherwise = interleave "," (str prog : map (str.showBox) boxes)

        stddevbox (BoxStdDev b s) = [str (showBox b), str (printf "%.3f" s)]
        stddevbox b = [str (showBox b), str "0"]

csvModTable :: [ResultTable] -> PerModuleTableSpec -> Normalise -> Bool -> ShowS
csvModTable results (SpecM _ _ _ get_result result_ok)
             norm stddev
  = csv_show_multi_results results get_result result_ok norm stddev

csv_show_multi_results
   :: Result a
        => [ResultTable]
        -> (Results -> Map String a)
        -> (a -> Bool)
        -> Normalise
        -> Bool
        -> ShowS

csv_show_multi_results []      _ _    _ _
 = error "csv_show_multi_results: Can't happen?"
csv_show_multi_results (r:rs) f _result_ok norm stddev
        = interleave "\n"
            [ result_line prog mod boxes
            | (prog, results) <- Map.toList r
            , (mod, result) <- Map.toList (f results)
            , let fms = map (maybe Map.empty f . Map.lookup prog) rs
            , let (_, boxes) = calc_result fms Just (const Success) (const True) (normalise norm) (mod, result)
            ]
 where
        result_line prog mod boxes
          | stddev    = interleave "," $ str prog : str mod : concat (map stddevbox boxes)
          | otherwise = interleave "," $ str prog : str mod : map (str.showBox) boxes

        stddevbox (BoxStdDev b s) = [str (showBox b), str (printf "%.3f" s)]
        stddevbox b = [str (showBox b), str "0"]

-- ---------------------------------------------------------------------------
-- Generic stuff for results generation

-- calc_result is a nice exercise in higher-order programming...
calc_result
  :: Result a
        => [Map String b]               -- accumulated results
        -> (b -> Maybe a)               -- get a result from the b
        -> (b -> Status)                -- get a status from the b
        -> (a -> Bool)                  -- normalise against the baseline?
        -> (a -> a -> BoxValue)             -- how to normalise
        -> (String,b)                   -- the baseline result
        -> (String,[BoxValue])

calc_result rts get_maybe_a get_stat base_ok norm_fn (prog,base_r) =
        (prog, (just_result m_baseline base_stat :

          let
                rts' = map (\rt -> get_stuff (Map.lookup prog rt)) rts

                get_stuff Nothing  = (Nothing, NotDone)
                get_stuff (Just r) = (get_maybe_a r, get_stat r)
          in
          (
          case m_baseline of
             Just baseline | base_ok baseline
                 -> map (\(r,s) -> do_norm r s baseline) rts'
             _other
                 -> map (\(r,s) -> just_result r s) rts'
           )))
 where
        m_baseline  = get_maybe_a base_r
        base_stat = get_stat base_r

        just_result Nothing  s = RunFailed s
        just_result (Just a) _ = toBox a

        do_norm Nothing   s _        = RunFailed s
        do_norm (Just a)  _ baseline = norm_fn baseline a

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
  | otherwise        = let sqr x   = x * x
                           len     = fromIntegral (length percentages)
                           logs    = map log percentages
                           lbar    = sum logs / len
                           st_devs = map (sqr . (lbar-)) logs
                           dbar    = sum st_devs / len
                           gm      = exp lbar
                           sdf     = exp (sqrt dbar)
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

convert_to_percentage :: Result a => a -> a -> BoxValue
convert_to_percentage n _val | float n < 0.0001 = Percentage 100
convert_to_percentage baseline val = Percentage  ((float val / float baseline) * 100)

normalise_to_base :: Result a => a -> a -> BoxValue
normalise_to_base n _val | float n < 0.0001  = BoxFloat 1
normalise_to_base baseline val =
  BoxStdDev (BoxFloat point)
            (point - (float baseline / (float val + variance val)))
   where
     point = (float baseline / float val)

class Result a where
   toBox    :: a -> BoxValue
   float    :: a -> Float
   variance :: a -> Float
   sumResult :: [a] -> a

-- We assume an Int is a size, and print it in kilobytes.

instance Result Int where
    toBox  = BoxInt
    float a = fromIntegral a
    variance a = 0
    sumResult = sum

data MeanStdDev a = MeanStdDev a Float

instance Result a => Result (MeanStdDev a) where
    toBox    (MeanStdDev a b) = BoxStdDev (toBox a) b
    float    (MeanStdDev a _) = float a
    variance (MeanStdDev _ b) = b
    -- This summing of standard deviations is only valid if the
    -- random variables are indepenent. If we assume that variance in measurements
    -- is random noise, then this should be ok
    sumResult mss = MeanStdDev
        (sumResult [m | MeanStdDev m _ <- mss])
        (sqrt $ sum [s^2 | MeanStdDev _ s <- mss])

instance Result Integer where
    toBox = BoxInteger
    float a = fromIntegral a
    variance a = 0
    sumResult = sum

instance Result Float where
    toBox = BoxFloat
    float a = realToFrac a
    variance a = 0
    sumResult = sum

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
  | BoxStdDev BoxValue Float

showBox :: BoxValue -> String
showBox (RunFailed stat) = show_stat stat
showBox (Percentage 100) = " 0.0%" -- pattern matching on Float is bad style, but this is not crucial
showBox (Percentage f)   = case printf "%.1f%%" (f-100) of
                               xs@('-':_) -> xs
                               xs -> '+':xs
showBox (BoxFloat f)     = printf "%.3f" f
showBox (BoxInt n)       = show n
showBox (BoxInteger n)   = show n
--showBox (BoxInt n)       = show (n `div` (1024*1024))
--showBox (BoxInteger n)   = show (n `div` (1024*1024))
--showBox (BoxInt n)       = show (n `div` 1024) ++ "k"
--showBox (BoxInteger n)   = show (n `div` 1024) ++ "k"
showBox (BoxString s)    = s
showBox (BoxStdDev b f)  = showBox b

instance Show BoxValue where
    show = showBox

show_stat :: Status -> String
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

type Layout = [BoxValue] -> ShowS

makeTable :: Layout -> [TableRow] -> ShowS
makeTable layout = interleave "\n" . map do_row
  where do_row (TableRow boxes) = layout boxes
        do_row TableLine = str (take 80 (repeat '-'))

makeLatexTable :: [TableRow] -> ShowS
makeLatexTable = foldr (.) id . map do_row
  where do_row (TableRow boxes)
           = latexTableLayout boxes . str "\\\\\n"
        do_row TableLine
           = str "\\hline\n"

latexTableLayout :: Layout
latexTableLayout boxes = 
  foldr (.) id . intersperse (str " & ") . map abox $ boxes
  where 
        abox (RunFailed NotDone) = id
        abox s = str (mungeForLaTeX (show s))

mungeForLaTeX :: String -> String
mungeForLaTeX = foldr transchar ""
   where
        transchar '_' s = '\\':'_':s
        transchar '%' s = '\\':'%':s
        transchar c s = c:s

-- -----------------------------------------------------------------------------
-- General Utils

split :: Char -> String -> [String]
split c s = case break (==c) s of
                (chunk, rest) ->
                    case rest of
                        []      -> [chunk]
                        _:rest' -> chunk : split c rest'

str :: String -> ShowS
str = showString

interleave :: String -> [ShowS] -> ShowS
interleave s = foldr1 (\a b -> a . str s . b)

fIELD_WIDTH :: Int
fIELD_WIDTH = 16

sUMMARY_FIELD_WIDTH :: Int
sUMMARY_FIELD_WIDTH = 9

-----------------------------------------------------------------------------
