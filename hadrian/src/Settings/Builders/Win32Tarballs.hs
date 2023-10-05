module Settings.Builders.Win32Tarballs (win32TarballsArgs) where

import Settings.Builders.Common

win32TarballsArgs :: Args
win32TarballsArgs = do
  scriptPath <- expr $ (-/- "mk" -/- "get-win32-tarballs.py") <$> topDirectory
  expr $ need [scriptPath]
  mconcat
    [ builder (Win32Tarballs DownloadTarballs) ? pure [scriptPath, "download", "all"]
    , builder (Win32Tarballs ListTarballs) ? pure [scriptPath, "list", "all"]
    , builder (Win32Tarballs VerifyTarballs) ? pure [scriptPath, "verify", "all"]
    ]
