
module Main where
import List
import System
import Char
import Array

--import IOExts(trace)

type Label = String
type Code  = [String]

pzipWith f []     []     = []
pzipWith f (a:as) (b:bs) = (f a b) : pzipWith f as bs
pzipWith f _      _      = error "pzipWith: unbalanced list"

main 
   = getArgs >>= \args ->
     --return ["/home/v-julsew/SOLARIS/NCG/fpt/ghc/tests/codeGen/should_run/cg001.s"]
     --                                                                     >>= \args ->
     if   length args /= 1
     then putStr ("\ndiff_gcc_nat:\n" ++
                  "   usage: create   File.s-gcc   and   File.s-nat\n" ++
                  "   then do: diff_gcc_nat File.s > synth.S\n" ++ 
                  "   and compile synth.S into your program.\n" ++
                  "diff_gcc_nat is to help debug GHC's native code generator;\n" ++
                  "it is quite useless for any other purpose.  For details, see\n" ++
                  "   fptools/ghc/utils/debugNCG/README.\n"++
                  "\n"
                 )
     else
     do
        let [f_root] = args
        f_gcc <- readFile (f_root ++ "-gcc")
        f_nat <- readFile (f_root ++ "-nat")

        let split_nat0 = breakOn is_split_line (lines f_nat)
            split_nat  = filter (not.null.getLabels) split_nat0

            split_markers_present
               = any is_split_line (lines f_nat)

            labels_nat = map getLabels split_nat
            labels_cls = map (map breakLabel) labels_nat

            labels_merged :: [(Label, [LabelKind])]
            labels_merged = map mergeBroken labels_cls

            classified :: [(Label, [LabelKind], [String])]
            classified
               = pzipWith (\ merged text -> (fst merged, snd merged, text))
                          labels_merged split_nat

            lines_gcc  = lines f_gcc

            (syncd, gcc_unused)
               = find_correspondings classified lines_gcc
            (ok_syncs, nat_unused)
               = check_syncs syncd

            num_ok = length ok_syncs
            
            preamble 
               = map (\i -> "#define NATIVE_" ++ show i ++ " 0") [1 .. num_ok]
                 ++ ["",
                     "#define UNMATCHED_NAT 0",
                     "#define UNMATCHED_GCC 1",
                     ""]

            final
               = preamble 
                 ++ concat (pzipWith pp_ok_sync ok_syncs [1 .. num_ok])
                 ++ ["",
                     "//============== unmatched NAT =================",
                     "#if UNMATCHED_NAT",
                     ""] 
                 ++ nat_unused
                 ++ ["",
                     "#endif",
                     "",
                     "//============== unmatched GCC =================",
                     "#if UNMATCHED_GCC"] 
                 ++ gcc_unused
                 ++ ["#endif"
                    ]

        if split_markers_present
         then putStr (unlines final)
         else putStr ("\ndiff_gcc_nat:\n"
                      ++ "   fatal error: NCG output doesn't contain any\n"
                      ++ "   ___ncg_debug_marker marks.  Can't continue!\n"
                      ++ "   To fix: enable these markers in\n" 
                      ++ "   fptools/ghc/compiler/nativeGen/AsmCodeGen.lhs,\n"
                      ++ "   recompile the compiler, and regenerate the assembly.\n\n")


pp_ok_sync :: (Label, [LabelKind], [String], [String])
           -> Int
           -> [String]
pp_ok_sync (lbl, kinds, nat_code, gcc_code) number
   = reconstruct number nat_code gcc_code


check_syncs :: [(Label, [LabelKind], [String], Maybe [String])] -- raw syncd
            -> ( [(Label, [LabelKind], [String], [String])],  -- ok syncs
                 [String] )                                   -- nat unsyncd

check_syncs [] = ([],[])
check_syncs (sync:syncs)
   = let (syncs_ok, syncs_uu) = check_syncs syncs
     in  case sync of
            (lbl, kinds, nat, Nothing)
               -> (syncs_ok, nat ++ syncs_uu)
            (lbl, kinds, nat, Just gcc_code)
               -> ((lbl,kinds,nat,gcc_code):syncs_ok, syncs_uu)


find_correspondings :: [(Label, [LabelKind], [String])]  -- native info
                    -> [String]                          -- gcc initial
                    -> ( [(Label, [LabelKind], [String], Maybe [String])],
                         [String] )
                       -- ( native info + found gcc stuff,
                       --   unused gcc stuff )

find_correspondings native gcc_init
   = f native gcc_init
     where
        wurble x (xs, gcc_final) = (x:xs, gcc_final)

        f [] gcc_uu = ( [], gcc_uu )
        f (nat:nats) gcc_uu
           = case nat of { (lbl, kinds, nat_code) ->
             case find_corresponding lbl kinds gcc_uu of
                Just (gcc_code, gcc_uu2)
                   |  gcc_code == gcc_code
                   -> --gcc_code `seq` gcc_uu2 `seq`
                      wurble (lbl, kinds, nat_code, Just gcc_code) (f nats gcc_uu2)
                Nothing
                   -> gcc_uu `seq`
                      wurble (lbl, kinds, nat_code, Nothing) (f nats gcc_uu)
             }


find_corresponding :: Label                      -- root
                   -> [LabelKind]                -- kinds
                   -> [String]                   -- gcc text
                   -> Maybe ([String],[String])  -- (found text, gcc leftovers)

find_corresponding root kinds gcc_lines
 = -- Enable the following trace in order to debug pattern matching problems.
   --trace (
   --   case result of 
   --      Nothing -> show (root,kinds) ++ "\nNothing\n\n"
   --      Just (found,uu)
   --         -> show (root, kinds) ++ "\n" ++ unlines found ++ "\n\n"
   --) 
   result
    where

     arr = listArray (1, length gcc_lines) gcc_lines
     pfxMatch ss t
         = let clean_t = filter (not.isSpace) t
           in  any (`isPrefixOf` clean_t) ss 

     result
      = case kinds of

        [Vtbl]
           -> let lbl_i = find_label arr (reconstruct_label root Vtbl)
                  fst_i = search_back arr lbl_i (pfxMatch [".text"])
              in
                  splice arr fst_i lbl_i

        [Closure]
           -> let lbl_i = find_label arr (reconstruct_label root Closure)
                  fst_i = search_back arr lbl_i (pfxMatch [".data"])
                  lst_i = search_fwds arr (lbl_i+1) 
                             (not . pfxMatch [".long",".uaword",".uahalf"])
              in
                  splice arr fst_i (lst_i-1)

        [Alt]
           -> let lbl_i = find_label arr (reconstruct_label root Alt)
                  fst_i = search_back arr lbl_i (pfxMatch ["."])
                  lst_i = search_fwds arr lbl_i (pfxMatch [".d", ".t", ".r", ".g"])
              in
                  splice arr fst_i (lst_i-1)

        [Dflt]
           -> let lbl_i = find_label arr (reconstruct_label root Dflt)
                  fst_i = search_back arr lbl_i (pfxMatch ["."])
                  lst_i = search_fwds arr lbl_i (pfxMatch [".d", ".t", ".r", ".g"])
              in
                  splice arr fst_i (lst_i-1)

        [Info,Entry]
           -> let info_i  = find_label arr (reconstruct_label root Info)
                  fst_i   = search_back arr info_i (pfxMatch [".text"])
                  entry_i = find_label arr (reconstruct_label root Entry)
                  lst_i   = search_fwds arr entry_i (pfxMatch [".d", ".t", ".r", ".g"])
              in
                  splice arr fst_i (lst_i-1)

        [Info,Entry,Fast k]
           -> let info_i  = find_label arr (reconstruct_label root Info)
                  fst_i   = search_back arr info_i (pfxMatch [".text"])
                  fast_i  = find_label arr (reconstruct_label root (Fast k))
                  lst_i   = search_fwds arr fast_i (pfxMatch [".d", ".t", ".r", ".g"])
              in
                  splice arr fst_i (lst_i-1)

        [Info,Ret]
           -> let info_i  = find_label arr (reconstruct_label root Info)
                  fst_i   = search_back arr info_i (pfxMatch [".text"])
                  ret_i   = find_label arr (reconstruct_label root Ret)
                  lst_i   = search_fwds arr ret_i (pfxMatch [".d", ".t", ".r", ".g"])
              in
                  splice arr fst_i (lst_i-1)

        [Srt]
           -> let lbl_i = find_label arr (reconstruct_label root Srt)
                  fst_i = search_back arr lbl_i (pfxMatch [".text",".data"])
                  lst_i = search_fwds arr (lbl_i+1)
                             (not . pfxMatch [".long",".uaword",".uahalf"])
              in
                  splice arr fst_i (lst_i-1)

        [CTbl]
           -> let lbl_i = find_label arr (reconstruct_label root CTbl)
                  fst_i = search_back arr lbl_i (pfxMatch [".text"])
                  lst_i = search_fwds arr (lbl_i+1)
                             (not . pfxMatch [".long",".uaword",".uahalf"])
              in
                  splice arr fst_i (lst_i-1)

        [Init]
           -> let lbl_i = find_label arr (reconstruct_label root Init)
                  fst_i = search_back arr lbl_i (pfxMatch [".data"])
                  lst_i = search_fwds arr lbl_i (pfxMatch [".d", ".t", ".r", ".g"])
              in
                  splice arr fst_i (lst_i-1)
        other 
           -> error ("find_corresponding: " ++ show kinds)


search_back :: Array Int String -> Int -> (String -> Bool) -> Int
search_back code start_ix pred
   = let test_ixs = [start_ix, start_ix-1 .. fst (bounds code)]
     in  case dropWhile (not . pred . (code !)) test_ixs of
            (ok:_) -> ok
            []     -> fst (bounds code) - 1

search_fwds :: Array Int String -> Int -> (String -> Bool) -> Int
search_fwds code start_ix pred
   = let test_ixs = [start_ix .. snd (bounds code)]
     in  case dropWhile (not . pred . (code !)) test_ixs of
            (ok:_) -> ok
            []     -> snd (bounds code) + 1


find_label :: Array Int String -> Label -> Int
find_label code lbl
   = --trace (unlines (map show (assocs code))) (
     case [idx | (idx, lbl2) <- assocs code, lbl == lbl2] of
        [idx] -> idx
        other -> error ("find_label `" ++ lbl ++ "'\n")
     --)

reconstruct_label :: Label -> LabelKind -> Label
reconstruct_label root Init
   = "__stginit_" ++ root ++ ":"
reconstruct_label root kind
   = root ++ "_" ++ pp kind ++ ":"
     where
        pp Info     = "info"
        pp Entry    = "entry"
        pp Closure  = "closure"
        pp Alt      = "alt"
        pp Vtbl     = "vtbl"
        pp Default  = "dflt"
        pp (Fast i) = "fast" ++ show i
        pp Dflt     = "dflt"
        pp Srt      = "srt"
        pp Ret      = "ret"
        pp CTbl     = "tbl"

splice :: Array Int String -> Int -> Int -> Maybe ([String],[String])
splice gcc_code lo hi 
   | lo <= hi && clo <= lo && hi <= chi
   = Just (map (gcc_code !) ix_used, 
           map (gcc_code !) (low_ix_uu ++ high_ix_uu))
   | otherwise
   = error "splice"
     where
        (clo,chi)  = bounds gcc_code
        low_ix_uu  = [clo .. lo-1]
        high_ix_uu = [hi+1 .. chi]
        ix_used    = [lo .. hi]

------------------------------------

getLabels :: [Label] -> [Label]
getLabels = sort . nub . filter is_interesting_label

data LabelKind
   = Info | Entry | Fast Int | Closure | Alt | Vtbl | Default 
   | Dflt | Srt | Ret | CTbl | Init
     deriving (Eq, Ord, Show)

breakLabel :: Label -> (Label,LabelKind)
breakLabel s
   = let sr = reverse s
         kr = takeWhile (/= '_') sr
         mr = drop (1 + length kr) sr
         m  = reverse mr
         k  = reverse kr
         kind
            | take 4 k == "fast"
            = Fast (read (takeWhile isDigit (drop 4 k)))
            | otherwise
            = case k of
                 "info:"    -> Info
                 "entry:"   -> Entry
                 "closure:" -> Closure
                 "alt:"     -> Alt
                 "vtbl:"    -> Vtbl
                 "dflt:"    -> Dflt
                 "srt:"     -> Srt
                 "ret:"     -> Ret
                 "tbl:"     -> CTbl
                 _ -> error ("breakLabel: " ++ show (s,k,m))
     in
        if   head m == '_' && dropWhile (== '_') m == "stginit"
        then (init k, Init)
        else (m, kind)

mergeBroken :: [(Label,LabelKind)] -> (Label, [LabelKind])
mergeBroken pairs
   = let (roots, kinds) = unzip pairs
         ok = all (== (head roots)) (tail roots)
              && length kinds == length (nub kinds)
     in 
         if ok 
         then (head roots, sort kinds)
         else error ("mergeBroken: " ++ show pairs)
       
 
reconstruct :: Int -> Code -> Code -> Code
reconstruct number nat_code gcc_code
   = ["",
      "//------------------------------------------"]
     ++ map (comment ("//--     ")) (getLabels gcc_code)
     ++ ["", "#if NATIVE_" ++ show number, "//nat version", ""]
     ++ nat_code
     ++ ["", "#else", "//gcc version", ""]
     ++ gcc_code
     ++ ["", "#endif"]

comment str x = str ++ x

-----------------------------------------------------
split_marker = "___ncg_debug_marker"

is_split_line s
   = let m = split_marker
     in  take 19 s == m || take 19 (drop 2 s) == m

is_interesting_label s
   = not (null s)
     && not (any isSpace s)
     && last s == ':'
     && '_' `elem` s

breakOn :: (a -> Bool) -> [a] -> [[a]]
breakOn p [] = []
breakOn p xs
   = let ys = takeWhile (not . p) xs
         rest = drop (1 + length ys) xs
     in
         if null ys then breakOn p rest else ys : breakOn p rest
