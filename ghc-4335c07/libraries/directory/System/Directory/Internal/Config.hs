{-# LANGUAGE CPP #-}
#include <HsDirectoryConfig.h>
module System.Directory.Internal.Config where

exeExtension :: String
exeExtension = EXE_EXTENSION
-- We avoid using #const_str from hsc because it breaks cross-compilation
-- builds, so we use this ugly workaround where we simply paste the C string
-- literal directly in here.  This will probably break if the EXE_EXTENSION
-- contains strange characters, but hopefully no sane OS would ever do that.
