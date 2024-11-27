module Main where

import Data.Maybe

main :: IO ()
main = interact update_settings

-- | Update settings using those of the boot compilers
update_settings :: String -> String
update_settings input = output
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
