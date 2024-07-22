module Settings.Builders.Happy (happyBuilderArgs) where

import Settings.Builders.Common

happyBuilderArgs :: Args
happyBuilderArgs = builder Happy ? mconcat [ arg =<< (\s -> "-i" ++ pwdname s ++ "-" ++ filename s ++ ".hinfo") <$> getInput
                                           , arg "-dagc"
                                           , arg "--strict"
                                           , arg =<< getInput
                                           , arg "-o", arg =<< getOutput ]

filename = reverse . takeWhile (/= '/') . drop 1 . dropWhile (/='.') . reverse
pwdname = reverse . takeWhile (/= '/') . drop 1 . dropWhile (/= '/') . reverse
