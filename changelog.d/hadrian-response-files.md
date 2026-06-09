section: packaging
synopsis: Improved Hadrian's use of response files
issues: #27230
mrs: !15906 !16134
description:
  Response files are files that contain command-line arguments. Hadrian uses
  response files to shorten command-line lengths. This is important on Windows
  where command-line lengths are limited.

  Hadrian now supports response files when invoking GHC. In order to support
  manually rerunning commands issued by Hadrian, response files are no longer
  deleted. Instead they are stored under `_build/rsp`. Response files are now
  only used when the corresponding command-line is too long for the host
  platform. This greatly reduces the use of response files and avoids excessive
  file usage. Response files are overwritten on subsequent Hadrian builds.
