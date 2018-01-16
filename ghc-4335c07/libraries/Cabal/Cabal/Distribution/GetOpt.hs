-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.GetOpt
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- This is a fork of "System.Console.GetOpt" with the following changes:
--
-- * Treat "cabal --flag command" as "cabal command --flag" e.g.
--   "cabal -v configure" to mean "cabal configure -v" For flags that are
--   not recognised as global flags, pass them on to the sub-command. See
--   the difference in 'shortOpt'.
--
-- * Line wrapping in the 'usageInfo' output, plus a more compact
--   rendering of short options, and slightly less padding.
--
-- If you want to take on the challenge of merging this with the GetOpt
-- from the base package then go for it!
--
module Distribution.GetOpt (
   -- * GetOpt
   getOpt, getOpt',
   usageInfo,
   ArgOrder(..),
   OptDescr(..),
   ArgDescr(..),

   -- * Example
   -- | See "System.Console.GetOpt" for examples
) where

import Prelude ()
import Distribution.Compat.Prelude
import System.Console.GetOpt
         ( ArgOrder(..), OptDescr(..), ArgDescr(..) )

data OptKind a                -- kind of cmd line arg (internal use only):
   = Opt       a                --    an option
   | UnreqOpt  String           --    an un-recognized option
   | NonOpt    String           --    a non-option
   | EndOfOpts                  --    end-of-options marker (i.e. "--")
   | OptErr    String           --    something went wrong...

-- | Return a string describing the usage of a command, derived from
-- the header (first argument) and the options described by the
-- second argument.
usageInfo :: String                    -- header
          -> [OptDescr a]              -- option descriptors
          -> String                    -- nicely formatted decription of options
usageInfo header optDescr = unlines (header:table)
   where (ss,ls,ds) = unzip3 [ (intercalate ", " (map (fmtShort ad) sos)
                               ,concatMap (fmtLong  ad) (take 1 los)
                               ,d)
                             | Option sos los ad d <- optDescr ]
         ssWidth    = (maximum . map length) ss
         lsWidth    = (maximum . map length) ls
         dsWidth    = 30 `max` (80 - (ssWidth + lsWidth + 3))
         table      = [ " " ++ padTo ssWidth so' ++
                        " " ++ padTo lsWidth lo' ++
                        " " ++ d'
                      | (so,lo,d) <- zip3 ss ls ds
                      , (so',lo',d') <- fmtOpt dsWidth so lo d ]
         padTo n x  = take n (x ++ repeat ' ')

fmtOpt :: Int -> String -> String -> String -> [(String, String, String)]
fmtOpt descrWidth so lo descr =
   case wrapText descrWidth descr of
     []     -> [(so,lo,"")]
     (d:ds) -> (so,lo,d) : [ ("","",d') | d' <- ds ]

fmtShort :: ArgDescr a -> Char -> String
fmtShort (NoArg  _   ) so = "-" ++ [so]
fmtShort (ReqArg _  _) so = "-" ++ [so]
fmtShort (OptArg _  _) so = "-" ++ [so]
  -- unlike upstream GetOpt we omit the arg name for short options

fmtLong :: ArgDescr a -> String -> String
fmtLong (NoArg  _   ) lo = "--" ++ lo
fmtLong (ReqArg _ ad) lo = "--" ++ lo ++ "=" ++ ad
fmtLong (OptArg _ ad) lo = "--" ++ lo ++ "[=" ++ ad ++ "]"

wrapText :: Int -> String -> [String]
wrapText width = map unwords . wrap 0 [] . words
  where wrap :: Int -> [String] -> [String] -> [[String]]
        wrap 0   []   (w:ws)
          | length w + 1 > width
          = wrap (length w) [w] ws
        wrap col line (w:ws)
          | col + length w + 1 > width
          = reverse line : wrap 0 [] (w:ws)
        wrap col line (w:ws)
          = let col' = col + length w + 1
             in wrap col' (w:line) ws
        wrap _ []   [] = []
        wrap _ line [] = [reverse line]

{-|
Process the command-line, and return the list of values that matched
(and those that didn\'t). The arguments are:

* The order requirements (see 'ArgOrder')

* The option descriptions (see 'OptDescr')

* The actual command line arguments (presumably got from
  'System.Environment.getArgs').

'getOpt' returns a triple consisting of the option arguments, a list
of non-options, and a list of error messages.
-}
getOpt :: ArgOrder a                   -- non-option handling
       -> [OptDescr a]                 -- option descriptors
       -> [String]                     -- the command-line arguments
       -> ([a],[String],[String])      -- (options,non-options,error messages)
getOpt ordering optDescr args = (os,xs,es ++ map errUnrec us)
   where (os,xs,us,es) = getOpt' ordering optDescr args

{-|
This is almost the same as 'getOpt', but returns a quadruple
consisting of the option arguments, a list of non-options, a list of
unrecognized options, and a list of error messages.
-}
getOpt' :: ArgOrder a                         -- non-option handling
        -> [OptDescr a]                       -- option descriptors
        -> [String]                           -- the command-line arguments
        -> ([a],[String], [String] ,[String]) -- (options,non-options,unrecognized,error messages)
getOpt' _        _        []         =  ([],[],[],[])
getOpt' ordering optDescr (arg:args) = procNextOpt opt ordering
   where procNextOpt (Opt o)      _                 = (o:os,xs,us,es)
         procNextOpt (UnreqOpt u) _                 = (os,xs,u:us,es)
         procNextOpt (NonOpt x)   RequireOrder      = ([],x:rest,[],[])
         procNextOpt (NonOpt x)   Permute           = (os,x:xs,us,es)
         procNextOpt (NonOpt x)   (ReturnInOrder f) = (f x :os, xs,us,es)
         procNextOpt EndOfOpts    RequireOrder      = ([],rest,[],[])
         procNextOpt EndOfOpts    Permute           = ([],rest,[],[])
         procNextOpt EndOfOpts    (ReturnInOrder f) = (map f rest,[],[],[])
         procNextOpt (OptErr e)   _                 = (os,xs,us,e:es)

         (opt,rest) = getNext arg args optDescr
         (os,xs,us,es) = getOpt' ordering optDescr rest

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
         getWith p = [ o  | o@(Option _ xs _ _) <- optDescr
                          , isJust (find (p opt) xs)]
         exact     = getWith (==)
         options   = if null exact then getWith isPrefixOf else exact
         ads       = [ ad | Option _ _ ad _ <- options ]
         optStr    = "--" ++ opt

         long (_:_:_)      _        rest     = (errAmbig options optStr,rest)
         long [NoArg  a  ] []       rest     = (Opt a,rest)
         long [NoArg  _  ] ('=':_)  rest     = (errNoArg optStr,rest)
         long [ReqArg _ d] []       []       = (errReq d optStr,[])
         long [ReqArg f _] []       (r:rest) = (Opt (f r),rest)
         long [ReqArg f _] ('=':xs) rest     = (Opt (f xs),rest)
         long [OptArg f _] []       rest     = (Opt (f Nothing),rest)
         long [OptArg f _] ('=':xs) rest     = (Opt (f (Just xs)),rest)
         long _            _        rest     = (UnreqOpt ("--"++ls),rest)

-- handle short option
shortOpt :: Char -> String -> [String] -> [OptDescr a] -> (OptKind a,[String])
shortOpt y ys rs optDescr = short ads ys rs
  where options = [ o  | o@(Option ss _ _ _) <- optDescr, s <- ss, y == s ]
        ads     = [ ad | Option _ _ ad _ <- options ]
        optStr  = '-':[y]

        short (_:_:_)        _  rest     = (errAmbig options optStr,rest)
        short (NoArg  a  :_) [] rest     = (Opt a,rest)
        short (NoArg  a  :_) xs rest     = (Opt a,('-':xs):rest)
        short (ReqArg _ d:_) [] []       = (errReq d optStr,[])
        short (ReqArg f _:_) [] (r:rest) = (Opt (f r),rest)
        short (ReqArg f _:_) xs rest     = (Opt (f xs),rest)
        short (OptArg f _:_) [] rest     = (Opt (f Nothing),rest)
        short (OptArg f _:_) xs rest     = (Opt (f (Just xs)),rest)
        short []             [] rest     = (UnreqOpt optStr,rest)
        short []             xs rest     = (UnreqOpt (optStr++xs),rest)
        -- This is different vs upstream = (UnreqOpt optStr,('-':xs):rest)
        -- Apparently this was part of the change so that flags that are
        -- not recognised as global flags are passed on to the sub-command.
        -- But why was no equivalent change required for longOpt? So could
        -- this change go upstream?

-- miscellaneous error formatting

errAmbig :: [OptDescr a] -> String -> OptKind a
errAmbig ods optStr = OptErr (usageInfo header ods)
   where header = "option `" ++ optStr ++ "' is ambiguous; could be one of:"

errReq :: String -> String -> OptKind a
errReq d optStr = OptErr ("option `" ++ optStr ++ "' requires an argument " ++ d ++ "\n")

errUnrec :: String -> String
errUnrec optStr = "unrecognized option `" ++ optStr ++ "'\n"

errNoArg :: String -> OptKind a
errNoArg optStr = OptErr ("option `" ++ optStr ++ "' doesn't allow an argument\n")
