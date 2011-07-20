module ShouldCompile where

import Foreign
import Foreign.C

-- extra space before the function name:
foreign import ccall unsafe " g_get_application_name"
  g_get_application_name :: (IO (Ptr CChar))

-- and after:
foreign import ccall unsafe "g_get_application_name "
  g_get_application_name' :: (IO (Ptr CChar))
