module GHC.Toolchain.CheckArm ( findArmIsa ) where

import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class
import System.Process

import GHC.Platform.ArchOS

import GHC.Toolchain.Prelude
import GHC.Toolchain.Tools.Cc

-- | Awkwardly, ARM triples sometimes contain insufficient information about
-- the platform. Consequently we instead extract this information from the
-- toolchain.
findArmIsa :: Cc -> M Arch
findArmIsa cc = do
    isa <- checkIsa
    abi <- checkAbi
    exts <- catMaybes <$> mapM checkExtension extensions
    let arch = ArchARM isa exts abi
    raspbianHack arch
  where
    checkIsa = checking "ARM ISA" $ do
        arch <- lastLine <$> preprocess cc archTestProgram
        case arch of
          _ | arch < "6" -> throwE "pre-ARMv6 is not supported"
          '6':_ -> return ARMv6
          '7':_ -> return ARMv7
          _ -> throwE "unknown ARM platform"

    checkAbi = checking "ARM ABI" $ do
        out <- fmap lastLine $ preprocess cc $ unlines
            [ "#if defined(__ARM_PCS_VFP)"
            , "HARD"
            , "#elif defined(__SOFTFP__)"
            , "SOFTFP"
            , "#else"
            , "SOFT"
            , "#endif"
            ]
        case out of
          "HARD" -> return HARD
          "SOFTFP" -> return SOFTFP
          "SOFT" -> return SOFT
          _ -> throwE $ "unexpected output from test program: " ++ out

    extensions :: [(ArmISAExt, String)]
    extensions =
        [ (NEON, "__ARM_NEON")
        , (VFPv2, "__VFP_FP__")
        , (VFPv2, "__ARM_VFPV2")
        , (VFPv3, "__ARM_VFPV3")
        ]

    checkExtension :: (ArmISAExt, String) -> M (Maybe ArmISAExt)
    checkExtension (ext, macro) = do
        supported <- checking ("for " ++ show ext ++ " support") $ testMacro macro
        return $
            if supported
              then Just ext
              else Nothing

    testMacro :: String -> M Bool
    testMacro macro = do
        out <- fmap lastLine $ preprocess cc $ unlines
            [ "#if defined(" ++ macro ++ ")"
            , "True"
            , "#else"
            , "False"
            , "#endif"
            ]
        case out of
          "True" -> return True
          "False" -> return False
          _ -> throwE $ "unexpected output from test program: " ++ out

lastLine :: String -> String
lastLine "" = ""
lastLine s  = last $ lines s

-- | Raspbian unfortunately makes some extremely questionable packaging
-- decisions, configuring gcc to compile for ARMv6 despite the fact that the
-- Raspberry Pi 4 is ARMv8. As ARMv8 doesn't support all instructions supported
-- by ARMv6 this can break. Work around this by checking uname to verify that
-- we aren't running on armv7.
-- See #17856.
--
raspbianHack :: Arch -> M Arch
raspbianHack arch@(ArchARM ARMv6 _ abi) = do
    raspbian <- isRaspbian
    armv7 <- isARMv7Host
    if raspbian && armv7
      then do logInfo "Found compiler which claims to target ARMv6 running in Raspbian on ARMv7."
              logInfo "Assuming we should actually target ARMv7 (see GHC #17856)"
              return $ ArchARM ARMv7 [VFPv2] abi
      else return arch
  where
    isRaspbian = checking "whether this is Raspbian" $ do
        issue <- readFile "/etc/issue" <|> return ""
        return $ "Raspbian" `isInfixOf` issue

    isARMv7Host = checking "whether the host is ARMv7" $ do
        uname <- liftIO $ readProcess "uname" ["-m"] ""
        return $ "armv7" `isInfixOf` uname

raspbianHack arch = return arch

archTestProgram :: String
archTestProgram = unlines $
    [ "#if defined(__ARM_ARCH)"
    , "__ARM_ARCH"
    ] ++
    [ "#elif defined(__ARM_ARCH_"++arch++"__)\n"++arch
    | arch <- armArchs
    ] ++
    [ "#else"
    , "#error \"unknown ARM platform\""
    , "#endif"
    ]

armArchs :: [String]
armArchs =
  [ "2"
  , "3", "3M"
  , "4", "4T"
  , "5", "5T", "5E", "5TE"
  , "6", "6J", "6T2", "6Z", "6ZK", "6K", "6KZ", "6M"
  , "7"
  ]
