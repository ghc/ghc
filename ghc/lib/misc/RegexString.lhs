-----------------------------------------------------------------------------
RegexString.lhs

A simple high-level interface to Regex

(c) Simon Marlow 1997
-----------------------------------------------------------------------------

> module RegexString (Regex(..), mkRegex, matchRegex) where

> import Regex
> import PackedString
> import Array
> import GlaExts

> type Regex = PatBuffer
> 
> mkRegex :: String -> Regex
> mkRegex s = unsafePerformPrimIO (
> 	  re_compile_pattern (packString s) False False)
> 
> matchRegex :: Regex -> String -> Maybe [String]
> matchRegex p s = unsafePerformPrimIO (
> 	  re_match p str 0 True >>= \m ->
> 	  case m of
> 		  Nothing -> return Nothing
> 		  Just m  -> return (Just (matches m str))
> 	  )
>    where
>	  str = packString s
> 
> matches (REmatch arr _ _ _ _) s = 
> 	  [ unpackPS (substrPS s beg (end-1)) | 
> 		  index <- [1..], let (beg,end) = arr ! index ]
