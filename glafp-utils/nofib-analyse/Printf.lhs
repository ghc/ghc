-----------------------------------------------------------------------------
-- $Id: Printf.lhs,v 1.1 1999/11/12 11:54:17 simonmar Exp $

-- (c) Simon Marlow 1997-1999
-----------------------------------------------------------------------------

> module Printf (showFloat, showFloat') where

> import GlaExts
> import PrelPack (unpackCString)

> showFloat 
>	:: Bool				-- Always print decimal point
>	-> Bool				-- Left adjustment
>	-> Bool				-- Always print sign
>	-> Bool				-- Leave blank before positive number
>	-> Bool				-- Use zero padding
>	-> Maybe Int			-- Field Width
>	-> Maybe Int			-- Precision
>	-> Float
>	-> String

> bUFSIZE = 512 :: Int

> showFloat alt left sign blank zero width prec num =
>	unsafePerformPrimIO ( do
>		buf <- _ccall_ malloc bUFSIZE :: IO Addr
>		_ccall_ snprintf buf bUFSIZE format num
>		let s = unpackCString buf
>		length s `seq` -- urk! need to force the string before we
>			       -- free the buffer.  A better solution would
>			       -- be to use foreign objects and finalisers,
>			       -- but that's just too heavyweight.
>		  _ccall_ free buf
>		return s
>	)
>	
>  where
>	format = '%' :
>		if_bool alt   "#" ++
>	      	if_bool left  "-" ++
>	      	if_bool sign  "+" ++
>	      	if_bool blank " " ++
>	      	if_bool zero  "0" ++
>	      	if_maybe width show ++
>	      	if_maybe prec  (\s -> "." ++ show s) ++
>	      	"f"

> showFloat' :: Maybe Int -> Maybe Int -> Float -> String
> showFloat' = showFloat False False False False False

> if_bool False s = []
> if_bool True  s = s

> if_maybe Nothing  f = []
> if_maybe (Just s) f = f s
