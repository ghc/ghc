module GHC.Toolchain.CheckPower ( checkPowerAbi ) where

import GHC.Platform.ArchOS

import GHC.Toolchain.Prelude
import GHC.Toolchain.Utils (lastLine)
import GHC.Toolchain.Tools.Cc

-- 64-Bit ELF V2 ABI Specification, Power Architecture, Revision 1.5 says:
-- A C preprocessor that conforms to this ABI shall predefine the macro
-- _CALL_ELF to have a value of 2 (Section 5.1.4 Predifined Macros).
-- The 64-bit PowerPC ELF Application Binary Interface Supplement 1.9
-- does not define any macro to identify the ABI.
-- So we check for ABI version 2 and default to ABI version 1.

checkPowerAbi :: Cc -> M Arch
checkPowerAbi cc = do
  checking "POWER ELF ABI" $ do
    out <- fmap lastLine $ preprocess cc $ unlines
        [ "#if defined(_CALL_ELF) && _CALL_ELF == 2"
        , "ELFv2"
        , "#else"
        , "ELFv1"
        , "#endif"
        ]
    case out of
      "ELFv1" -> pure $ ArchPPC_64 ELF_V1
      "ELFv2" -> pure $ ArchPPC_64 ELF_V2
      _       -> throwE $ "unexpected output from test program: " ++ out
