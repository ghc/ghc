\section[regex]{Haskell binding to the GNU regex library}

What follows is a straightforward binding to the functions
provided by the GNU regex library (the GNU group of functions with Perl
like syntax)

\begin{code}
module Regex 

	(
	 PatBuffer(..),
	 re_compile_pattern,
	 re_match,
	 re_search,
	 re_match2,
	 re_search2,
	 
	 REmatch(..)

	) where

import PreludeGlaST

\end{code}

First, the higher level matching structure that the functions herein return:

\begin{code}

--
-- GroupBounds hold the interval where a group
-- matched inside a string, e.g.
--
-- matching "reg(exp)" "a regexp" returns the pair (5,7) for the
-- (exp) group. (_PackedString indices start from 0)
--  

type GroupBounds = (Int, Int)


data REmatch
 = REmatch (Array Int GroupBounds)  -- for $1, ... $n
	   GroupBounds		    -- for $` (everything before match)
	   GroupBounds		    -- for $& (entire matched string)
	   GroupBounds		    -- for $' (everything after)
	   GroupBounds		    -- for $+ (matched by last bracket)
 {- debugging    deriving Text  -}

\end{code}

Prior to any matching (or searching), the regular expression
have to compiled into an internal form, the pattern buffer.
Represent the pattern buffer as a Haskell heap object:

\begin{code}


data PatBuffer = PatBuffer# (MutableByteArray# _RealWorld)
instance _CCallable PatBuffer
instance _CReturnable PatBuffer

createPatBuffer :: Bool
		-> PrimIO PatBuffer
createPatBuffer insensitive
 = _casm_ `` %r = (int)sizeof(struct re_pattern_buffer); '' `thenPrimIO` \ sz ->
   newCharArray (0,sz)	       	    `thenPrimIO` \ (_MutableByteArray _ pbuf#) ->
   let
    pbuf = PatBuffer# pbuf#
   in
    (if insensitive then
       {-
	 See comment re: fastmap below
       -}
       ((_casm_ `` %r = (char *)malloc(256*sizeof(char)); '')::PrimIO _Addr) `thenPrimIO` \ tmap ->
       {-
         Set up the translate table so that any lowercase
         char. gets mapped to an uppercase one. Beacuse quoting
         inside CAsmStrings is Problematic, we pass in the ordinal values
         of 'a','z' and 'A'
       -}
       _casm_ `` { int i;

		  for(i=0; i<256; i++)
		     ((char *)%0)[i] = (char)i;
	          for(i=(int)%1;i <=(int)%2;i++)
		     ((char *)%0)[i] = i - ((int)%1 - (int)%3);
	          %r = 0; } '' tmap (ord 'a') (ord 'z') (ord 'A') 	`seqPrimIO`
       _casm_ `` { ((struct re_pattern_buffer *)%0)->translate = %1; %r = 0; } '' pbuf tmap
     else
       _casm_ `` { ((struct re_pattern_buffer *)%0)->translate = 0; %r = 0; } '' pbuf) `seqPrimIO`
    {-
      Use a fastmap to speed things up, would like to have the fastmap
      in the Haskell heap, but it will get GCed before we can say regexp,
      as the reference to it is buried inside a ByteArray :-(
    -}
    ((_casm_ `` %r = (char *)malloc(256*sizeof(char)); '')::PrimIO _Addr) `thenPrimIO` \ fmap ->
    _casm_ `` { ((struct re_pattern_buffer *)%0)->fastmap   = %1; %r = 0; } '' pbuf fmap `seqPrimIO`
    {-
      We want the compiler of the pattern to alloc. memory
      for the pattern.
    -}
    _casm_ `` { ((struct re_pattern_buffer *)%0)->buffer    = 0; %r = 0;} '' pbuf `seqPrimIO`
    _casm_ `` { ((struct re_pattern_buffer *)%0)->allocated = 0; %r = 0;} '' pbuf `seqPrimIO`
    returnPrimIO pbuf

\end{code}

@re_compile_pattern@ converts a regular expression into a pattern buffer,
GNU style.

Q: should we lift the syntax bits configuration up to the Haskell
programmer level ? 

\begin{code}

re_compile_pattern :: _PackedString
		   -> Bool
		   -> Bool
		   -> PrimIO PatBuffer
re_compile_pattern str single_line_mode insensitive
 = createPatBuffer insensitive	`thenPrimIO` \ pbuf ->
   (if single_line_mode then	-- match a multi-line buffer
       _casm_ `` %r = re_syntax_options = RE_PERL_SINGLELINE_SYNTAX; ''
    else
       _casm_ `` %r = re_syntax_options = RE_PERL_MULTILINE_SYNTAX; '') `seqPrimIO`

   _casm_ `` %r=(int)re_compile_pattern((char *)%0,
   					(int)%1,
					(struct re_pattern_buffer *)%2); '' (_unpackPS str)
									    (_lengthPS str)
									    pbuf	`thenPrimIO` \ err ->
   --
   -- No checking for how the compilation of the pattern went yet.
   --
   returnPrimIO pbuf

\end{code}

Got a match ?

\begin{code}

re_match :: PatBuffer
	 -> _PackedString
	 -> Int
	 -> Bool
	 -> PrimIO (Maybe REmatch)
re_match pbuf
	 str
	 start
	 reg
 = ((if reg then  -- record result of match in registers
      _casm_ `` %r = (struct re_registers *)malloc(sizeof(struct re_registers *)); ''
     else
      _casm_ `` %r = (struct re_registers *)NULL; '')::PrimIO _Addr)  `thenPrimIO` \ regs ->
   _casm_ `` %r=(int)re_match((struct re_pattern_buffer *)%0,
 			      (char *)%1,
			      (int)%2,
			      (int)%3,
			      (struct re_registers *)%4); '' pbuf
							     (_unpackPS str)
							     (_lengthPS str)
							     start
							     regs	`thenPrimIO` \ match_res ->
  if match_res == (-2) then
	error "re_match: Internal error"
  else if match_res < 0 then
     _casm_ `` { free((struct re_registers *)%0); %r = 0; } '' regs `seqPrimIO`
     returnPrimIO Nothing
  else
     build_re_match start (_lengthPS str) regs	`thenPrimIO` \ arr ->
     _casm_ `` { free((struct re_registers *)%0); %r = 0; } '' regs  `seqPrimIO`
     returnPrimIO (Just arr)

\end{code}

Matching on 2 strings is useful when you're dealing with multiple
buffers, which is something that could prove useful for PackedStrings,
as we don't want to stuff the contents of a file into one massive heap
chunk, but load (smaller chunks) on demand.

\begin{code}

re_match2 :: PatBuffer
	  -> _PackedString
	  -> _PackedString
	  -> Int
	  -> Int
	  -> Bool
	  -> PrimIO (Maybe REmatch)
re_match2 pbuf
	  str1
	  str2
	  start
	  stop
	  reg
 = ((if reg then  -- record result of match in registers
      _casm_ `` %r = (struct re_registers *)malloc(sizeof(struct re_registers *)); ''
     else
      _casm_ `` %r = (struct re_registers *)NULL; '')::PrimIO _Addr)	`thenPrimIO` \ regs ->
   _casm_ `` %r=(int)re_match_2((struct re_pattern_buffer *)%0,
 			        (char *)%1,
			        (int)%2,
 			        (char *)%3,
			        (int)%4,
			        (int)%5,
			        (struct re_registers *)%6,
			        (int)%7); '' pbuf
					     (_unpackPS str1)
					     (_lengthPS str1)
					     (_unpackPS str2)
					     (_lengthPS str2)
					     start
					     regs
					     stop    `thenPrimIO` \ match_res ->
  if match_res == (-2) then
	error "re_match2: Internal error"
  else if match_res < 0 then
     _casm_ `` { free((struct re_registers *)%0); %r = 0; } '' regs `seqPrimIO`
     returnPrimIO Nothing
  else
     build_re_match start stop regs	`thenPrimIO` \ arr ->
     _casm_ `` { free((struct re_registers *)%0); %r = 0; } '' regs  `seqPrimIO`
     returnPrimIO (Just arr)


\end{code}

Find all the matches in a string.

\begin{code}

re_search :: PatBuffer
	  -> _PackedString
	  -> Int
	  -> Int
	  -> Bool
	  -> PrimIO (Maybe REmatch)
re_search pbuf			     -- the compiled regexp
	  str			     -- the string to search
	  start			     -- start index
	  range			     -- stop index
	  reg			     -- record result of match in registers 
 = (if reg then  -- record result of match in registers
      _casm_ `` %r = (struct re_registers *)malloc(sizeof(struct re_registers *)); ''
    else
      _casm_ `` %r = (struct re_registers *)NULL; '')	`thenPrimIO` \ regs ->
   _casm_ `` %r=(int)re_search((struct re_pattern_buffer *)%0,
 			       (char *)%1,
			       (int)%2,
			       (int)%3,
			       (int)%4,
			       (struct re_registers *)%5); '' pbuf
							     (_unpackPS str)
							     (_lengthPS str)
							     start
							     range
							     regs	`thenPrimIO` \ match_res ->
  if match_res== (-1) then
     _casm_ `` { free((struct re_registers *)%0); %r = 0; } '' regs `seqPrimIO`
     returnPrimIO Nothing
  else
     let
      (st,en) = if range > start then 
		   (start,range)
	        else
		   (range,start)
     in
      build_re_match st en regs					     `thenPrimIO` \ arr ->
      _casm_ `` { free((struct re_registers *)%0); %r = 0; } '' regs `seqPrimIO`
      returnPrimIO (Just arr)

\end{code}

Double buffer search

\begin{code}

re_search2 :: PatBuffer
	   -> _PackedString
	   -> _PackedString
	   -> Int
	   -> Int
	   -> Int
	   -> Bool
	   -> PrimIO (Maybe REmatch)
re_search2 pbuf
	   str1
	   str2
	   start
	   range
	   stop
	   reg
 = (if reg then  -- record result of match in registers
      _casm_ `` %r = (struct re_registers *)malloc(sizeof(struct re_registers *)); ''
    else
      _casm_ `` %r = (struct re_registers *)NULL; '')	`thenPrimIO` \ regs ->
   _casm_ `` %r=(int)re_search_2((struct re_pattern_buffer *)%0,
 			         (char *)%1,
			         (int)%2,
 			         (char *)%3,
			         (int)%4,
			         (int)%5,
			         (int)%6,
			         (struct re_registers *)%7,
			         (int)%8); '' pbuf
					      (_unpackPS str1)
					      (_lengthPS str1)
					      (_unpackPS str2)
					      (_lengthPS str2)
					      start
					      range
					      regs
					      stop    `thenPrimIO` \ match_res ->
  if match_res== (-1) then
     _casm_ `` { free((struct re_registers *)%0); %r = 0; } '' regs `seqPrimIO`
     returnPrimIO Nothing
  else
     let
      (st,en) = if range > start then 
		   (start,range)
	        else
		   (range,start)
     in
      build_re_match st en regs					     `thenPrimIO` \ arr ->
      _casm_ `` { free((struct re_registers *)%0); %r = 0; } '' regs `seqPrimIO`
      returnPrimIO (Just arr)

\end{code}

\begin{code}

build_re_match :: Int
	       -> Int
	       -> _Addr 
	       -> PrimIO REmatch
build_re_match str_start 
	       str_end 
	       regs
 = _casm_ `` %r=(int)(*(struct re_registers *)%0).num_regs; '' regs  `thenPrimIO` \ len ->
   match_reg_to_array regs len	`thenPrimIO` \ (match_start,match_end,arr) ->
   let
    (1,x) = bounds arr

    bef  = (str_start,match_start)  -- $'
    aft  = (match_end,str_end)      -- $`
    lst  = arr!x		    -- $+
    mtch = (match_start,match_end)  -- $&
   in
    returnPrimIO (REmatch arr
			  bef
			  mtch
			  aft
			  lst)
   where
    match_reg_to_array regs len
     = trundleIO regs (0,[]) len  `thenPrimIO` \ (no,ls) ->
       let
        (st,end,ls')
         = case ls of
             [] -> (0,0,[])
	     [(a,b)] -> (a,b,ls)
             ((a,b):xs) -> (a,b,xs)
       in        
        returnPrimIO 
	   (st,
	    end,
	    array (1,max 1 (no-1)) 
		  [ i := x | (i,x) <- zip [1..] ls'])

    trundleIO :: _Addr 
	     -> (Int,[(Int,Int)])
	     -> Int 
	     -> PrimIO (Int,[(Int,Int)])
    trundleIO regs (i,acc) len
     | i==len = returnPrimIO (i,reverse acc)
     | otherwise	  
       = _casm_ ``%r = (int)(((struct re_registers *)%0)->start)[(int)%1]; '' regs i `thenPrimIO` \ start ->
         _casm_ ``%r = (int)(((struct re_registers *)%0)->end)[(int)%1]; '' regs i `thenPrimIO` \ end ->
	 let
	  acc' = (start,end):acc
	 in
	  if (start == (-1)) && (end == (-1)) then
	     returnPrimIO (i,reverse acc)
	  else
	     trundleIO regs (i+1,acc') len

\end{code}

