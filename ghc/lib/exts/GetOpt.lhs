A Haskell port of GNU's getopt library 

Sven Panne <Sven.Panne@informatik.uni-muenchen.de> Oct. 1996 (small
changes Dec. 1997)

Two rather obscure features are missing: The Bash 2.0 non-option hack
(if you don't already know it, you probably don't want to hear about
it...) and the recognition of long options with a single dash
(e.g. '-help' is recognised as '--help', as long as there is no short
option 'h').

Other differences between GNU's getopt and this implementation: * To
enforce a coherent description of options and arguments, there are
explanation fields in the option/argument descriptor.  * Error
messages are now more informative, but no longer POSIX
compliant... :-( And a final Haskell advertisement: The GNU C
implementation uses well over 1100 lines, we need only 195 here,
including a 46 line example! :-)

\begin{code}
module GetOpt (ArgOrder(..), OptDescr(..), ArgDescr(..), usageInfo, getOpt) where

import List(isPrefixOf)

data ArgOrder a                        -- what to do with options following non-options:
   = RequireOrder                      --    no option processing after first non-option
   | Permute                           --    freely intersperse options and non-options
   | ReturnInOrder (String -> a)       --    wrap non-options into options

data OptDescr a =                      -- description of a single options:
   Option [Char]                       --    list of short option characters
          [String]                     --    list of long option strings (without "--")
          (ArgDescr a)                 --    argument descriptor
          String                       --    explanation of option for user

data ArgDescr a                        -- description of an argument option:
   = NoArg                   a         --    no argument expected
   | ReqArg (String       -> a) String --    option requires argument
   | OptArg (Maybe String -> a) String --    optional argument

data OptKind a                         -- kind of cmd line arg (internal use only):
   = Opt       a                       --    an option
   | NonOpt    String                  --    a non-option
   | EndOfOpts                         --    end-of-options marker (i.e. "--")
   | OptErr    String                  --    something went wrong...

usageInfo :: String                    -- header
          -> [OptDescr a]              -- option descriptors
          -> String                    -- nicely formatted decription of options
usageInfo header optDescr = unlines (header:table)
   where (ss,ls,ds)     = (unzip3 . map fmtOpt) optDescr
         table          = zipWith3 paste (sameLen ss) (sameLen ls) (sameLen ds)
         paste x y z    = "  " ++ x ++ "  " ++ y ++ "  " ++ z
         sameLen xs     = flushLeft ((maximum . map length) xs) xs
         flushLeft n xs = [ take n (x ++ repeat ' ') | x <- xs ]

fmtOpt :: OptDescr a -> (String,String,String)
fmtOpt (Option sos los ad descr) = (sepBy ',' (map (fmtShort ad) sos),
                                    sepBy ',' (map (fmtLong  ad) los),
                                    descr)
   where sepBy _  []     = ""
         sepBy _  [x]    = x
         sepBy ch (x:xs) = x ++ ch:' ':sepBy ch xs

fmtShort :: ArgDescr a -> Char -> String
fmtShort (NoArg  _   ) so = "-" ++ [so]
fmtShort (ReqArg _ ad) so = "-" ++ [so] ++ " " ++ ad
fmtShort (OptArg _ ad) so = "-" ++ [so] ++ "[" ++ ad ++ "]"

fmtLong :: ArgDescr a -> String -> String
fmtLong (NoArg  _   ) lo = "--" ++ lo
fmtLong (ReqArg _ ad) lo = "--" ++ lo ++ "=" ++ ad
fmtLong (OptArg _ ad) lo = "--" ++ lo ++ "[=" ++ ad ++ "]"

getOpt :: ArgOrder a                   -- non-option handling
       -> [OptDescr a]                 -- option descriptors
       -> [String]                     -- the commandline arguments
       -> ([a],[String],[String])      -- (options,non-options,error messages)
getOpt _        _        []         =  ([],[],[])
getOpt ordering optDescr (arg:args) = procNextOpt opt ordering
   where procNextOpt (Opt o)    _                 = (o:os,xs,es)
         procNextOpt (NonOpt x) RequireOrder      = ([],x:rest,[])
         procNextOpt (NonOpt x) Permute           = (os,x:xs,es)
         procNextOpt (NonOpt x) (ReturnInOrder f) = (f x :os, xs,es)
         procNextOpt EndOfOpts  RequireOrder      = ([],rest,[])
         procNextOpt EndOfOpts  Permute           = ([],rest,[])
         procNextOpt EndOfOpts  (ReturnInOrder f) = (map f rest,[],[])
         procNextOpt (OptErr e) _                 = (os,xs,e:es)

         (opt,rest) = getNext arg args optDescr
         (os,xs,es) = getOpt ordering optDescr rest

-- take a look at the next cmd line arg and decide what to do with it
getNext :: String -> [String] -> [OptDescr a] -> (OptKind a,[String])
getNext ('-':'-':[]) rest _        = (EndOfOpts,rest)
getNext ('-':'-':xs) rest optDescr = longOpt xs rest optDescr
getNext ('-': x :xs) rest optDescr = shortOpt x xs rest optDescr
getNext a            rest _        = (NonOpt a,rest)

-- handle long option
longOpt :: String -> [String] -> [OptDescr a] -> (OptKind a,[String])
longOpt ls rs optDescr = long ads arg rs
   where (opt,arg) = break (=='=') ls
         options   = [ o  | o@(Option _ ls _ _) <- optDescr, l <- ls, opt `isPrefixOf` l ]
         ads       = [ ad | Option _ _ ad _ <- options ]
         optStr    = ("--"++opt)

         long (_:_:_)      _        rest     = (errAmbig options optStr,rest)
         long [NoArg  a  ] []       rest     = (Opt a,rest)
         long [NoArg  _  ] ('=':_)  rest     = (errNoArg optStr,rest)
         long [ReqArg _ d] []       []       = (errReq d optStr,[])
         long [ReqArg f _] []       (r:rest) = (Opt (f r),rest)
         long [ReqArg f _] ('=':xs) rest     = (Opt (f xs),rest)
         long [OptArg f _] []       rest     = (Opt (f Nothing),rest)
         long [OptArg f _] ('=':xs) rest     = (Opt (f (Just xs)),rest)
         long _            _        rest     = (errUnrec optStr,rest)

-- handle short option
shortOpt :: Char -> String -> [String] -> [OptDescr a] -> (OptKind a,[String])
shortOpt x xs rest optDescr = short ads xs rest
  where options = [ o  | o@(Option ss _ _ _) <- optDescr, s <- ss, x == s ]
        ads     = [ ad | Option _ _ ad _ <- options ]
        optStr  = '-':[x]

        short (_:_:_)        _  rest     = (errAmbig options optStr,rest)
        short (NoArg  a  :_) [] rest     = (Opt a,rest)
        short (NoArg  a  :_) xs rest     = (Opt a,('-':xs):rest)
        short (ReqArg f d:_) [] []       = (errReq d optStr,[])
        short (ReqArg f _:_) [] (r:rest) = (Opt (f r),rest)
        short (ReqArg f _:_) xs rest     = (Opt (f xs),rest)
        short (OptArg f _:_) [] rest     = (Opt (f Nothing),rest)
        short (OptArg f _:_) xs rest     = (Opt (f (Just xs)),rest)
        short []             [] rest     = (errUnrec optStr,rest)
        short []             xs rest     = (errUnrec optStr,('-':xs):rest)

-- miscellaneous error formatting

errAmbig :: [OptDescr a] -> String -> OptKind a
errAmbig ods optStr = OptErr (usageInfo header ods)
   where header = "option `" ++ optStr ++ "' is ambiguous; could be one of:"

errReq :: String -> String -> OptKind a
errReq d optStr = OptErr ("option `" ++ optStr ++ "' requires an argument " ++ d ++ "\n")

errUnrec :: String -> OptKind a
errUnrec optStr = OptErr ("unrecognized option `" ++ optStr ++ "'\n")

errNoArg :: String -> OptKind a
errNoArg optStr = OptErr ("option `" ++ optStr ++ "' doesn't allow an argument\n")

{-
-----------------------------------------------------------------------------------------
-- and here a small and hopefully enlightening example:

data Flag = Verbose | Version | Name String | Output String | Arg String   deriving Show

options :: [OptDescr Flag]
options =
   [Option ['v']     ["verbose"]           (NoArg Verbose)      "verbosely list files",
    Option ['V','?'] ["version","release"] (NoArg Version)      "show version info",
    Option ['o']     ["output"]            (OptArg out "FILE")  "use FILE for dump",
    Option ['n']     ["name"]              (ReqArg Name "USER") "only dump USER's files"]

out :: Maybe String -> Flag
out Nothing  = Output "stdout"
out (Just o) = Output o

test :: ArgOrder Flag -> [String] -> String
test order cmdline = case getOpt order options cmdline of
                        (o,n,[]  ) -> "options=" ++ show o ++ "  args=" ++ show n ++ "\n"
                        (_,_,errs) -> concat errs ++ usageInfo header options
   where header = "Usage: foobar [OPTION...] files..."

-- example runs:
-- putStr (test RequireOrder ["foo","-v"])
--    ==> options=[]  args=["foo", "-v"]
-- putStr (test Permute ["foo","-v"])
--    ==> options=[Verbose]  args=["foo"]
-- putStr (test (ReturnInOrder Arg) ["foo","-v"])
--    ==> options=[Arg "foo", Verbose]  args=[]
-- putStr (test Permute ["foo","--","-v"])
--    ==> options=[]  args=["foo", "-v"]
-- putStr (test Permute ["-?o","--name","bar","--na=baz"])
--    ==> options=[Version, Output "stdout", Name "bar", Name "baz"]  args=[]
-- putStr (test Permute ["--ver","foo"])
--    ==> option `--ver' is ambiguous; could be one of:
--          -v      --verbose             verbosely list files
--          -V, -?  --version, --release  show version info   
--        Usage: foobar [OPTION...] files...
--          -v        --verbose             verbosely list files  
--          -V, -?    --version, --release  show version info     
--          -o[FILE]  --output[=FILE]       use FILE for dump     
--          -n USER   --name=USER           only dump USER's files
-----------------------------------------------------------------------------------------
-}
\end{code}
