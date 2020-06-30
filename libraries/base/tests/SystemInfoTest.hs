module SystemInfoTest where

import           System.Info (fullCompilerVersion)

main :: IO ()
main =
  let goodVersion = "8.11.0.20200317"
   in if Version [8,11,0] == fullCompilerVersion goodVersion
      then ()
      else error "fullCompilerVersion could not parse proper version string!"
