-----------------------------------------------------------------------------
-- $Id: Printf.lhs,v 1.3 2001/08/07 11:13:46 simonmar Exp $

-- (c) Simon Marlow 1997-2001
-----------------------------------------------------------------------------

> module Printf (showFloat, showFloat') where

> import Foreign
> import CTypes
> import CTypesISO
> import CString
> import IOExts
> import ByteArray

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
>	unsafePerformIO $ do

#if __GLASGOW_HASKELL__ < 500

>		buf <- malloc bUFSIZE
>		snprintf buf (fromIntegral bUFSIZE) (packString format) num
>		let s = unpackCString buf
>		length s `seq` -- urk! need to force the string before we
>			       -- free the buffer.  A better solution would
>			       -- be to use foreign objects and finalisers,
>			       -- but that's just too heavyweight.
>		   free buf
>		return s

#else

>		allocaBytes bUFSIZE $ \buf -> do
>		snprintf buf (fromIntegral bUFSIZE) (packString format) num
>		peekCString buf

#endif

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

> type PackedString = ByteArray Int

#if __GLASGOW_HASKELL__ < 500

> foreign import unsafe snprintf :: Addr -> CSize -> PackedString -> Float -> IO ()

#else

> foreign import unsafe snprintf :: CString -> CSize -> PackedString -> Float -> IO ()

#endif
