module Main where

import Data.Maybe
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["ghc-boot"] -> interact ghcboot_settings
    ["stage1"]   -> interact stage1_settings

-- | Generate HADRIAN_SETTINGS for ghc-boot's Setup.hs, based on given settings
ghcboot_settings :: String -> String
ghcboot_settings input = output
  where
    output = show out_settings

    in_settings,out_settings :: [(String,String)]
    in_settings = read input

    out_settings =
        [ ("hostPlatformArch", fromMaybe (error "Couldn't read 'target arch' setting") (lookup "target arch" in_settings))
        , ("hostPlatformOS", fromMaybe (error "Couldn't read 'target os' setting") (lookup "target os" in_settings))
        , ("cProjectGitCommitId", "DEADBEEF") -- FIXME
        , ("cProjectVersion", "9.13")
        , ("cProjectVersionInt", "913")
        , ("cProjectPatchLevel", "0")
        , ("cProjectPatchLevel1", "0")
        , ("cProjectPatchLevel2", "0")
        ]

-- | Generate settings for stage1 compiler, based on given settings (stage0's
-- compiler settings)
stage1_settings :: String -> String
stage1_settings input = output
  where
    output = show out_settings

    in_settings,out_settings :: [(String,String)]
    in_settings = read input

    -- keep the previous setting, fail if it doesn't exist
    keep_fail s = keep_def s (error ("Couldn't find setting "<> show s))

    -- keep the previous setting, default to the given value if it doesn't exist
    keep_def s d = case lookup s in_settings of
      Nothing -> (s,d)
      Just v  -> (s,v)

    -- use the previous setting, or if it doesn't exist use the setting for the
    -- second key. Fail if both don't exist. This is useful to support
    -- bootstrapping with old compilers that mingled some settings.
    keep_or_fail s s2 = case lookup s in_settings of
      Nothing -> case lookup s2 in_settings of
        Nothing -> error ("Couldn't find any of " <> show s <> " and " <> show s2)
        Just v  -> (s,v)
      Just v  -> (s,v)


    out_settings =
        [ keep_fail "C compiler command"
        , keep_fail "C compiler flags"
        , keep_fail "C++ compiler command"
        , keep_fail "C++ compiler flags"
        , keep_fail "C compiler link flags"
        , keep_fail "C compiler supports -no-pie"
        , keep_or_fail "CPP command" "Haskell CPP command"
        , keep_or_fail "CPP flags" "Haskell CPP flags"
        , keep_fail "Haskell CPP command"
        , keep_fail "Haskell CPP flags"
        , keep_or_fail "JavaScript CPP command" "Haskell CPP command"
        , keep_or_fail "JavaScript CPP flags" "Haskell CPP flags"
        , keep_or_fail "C-- CPP command" "Haskell CPP command"
        , keep_or_fail "C-- CPP flags"   "Haskell CPP flags"
        , keep_def "C-- CPP supports -g0" "NO"
        , keep_fail "ld supports compact unwind"
        , keep_fail "ld supports filelist"
        , keep_fail "ld supports single module"
        , keep_fail "ld is GNU ld"
        , keep_fail "Merge objects command"
        , keep_fail "Merge objects flags"
        , keep_def "Merge objects supports response files" "NO"
        , keep_fail "ar command"
        , keep_fail "ar flags"
        , keep_fail "ar supports at file"
        , keep_fail "ar supports -L"
        , keep_fail "ranlib command"
        , keep_fail "otool command"
        , keep_fail "install_name_tool command"
        , keep_fail "windres command"
        , keep_fail "unlit command"
        , keep_fail "cross compiling"
        , keep_fail "target platform string"
        , keep_fail "target os"
        , keep_fail "target arch"
        , keep_fail "target word size"
        , keep_fail "target word big endian"
        , keep_fail "target has GNU nonexec stack"
        , keep_fail "target has .ident directive"
        , keep_fail "target has subsections via symbols"
        , keep_fail "target has libm"
        , keep_fail "Unregisterised"
        , keep_fail "LLVM target"
        , keep_fail "LLVM llc command"
        , keep_fail "LLVM opt command"
        , keep_def "LLVM llvm-as command" "llvm-as"
        , keep_fail "Use inplace MinGW toolchain"

        , keep_def "target RTS linker only supports shared libraries" "NO"
        , ("Use interpreter", "NO")
        , keep_fail "Support SMP"
        , keep_fail "RTS ways"
        , keep_fail "Tables next to code"
        , keep_fail "Leading underscore"
        , keep_fail "Use LibFFI"
        , keep_fail "RTS expects libdw"
        , ("Relative Global Package DB", "_build/stage1/pkgs")
        ]
