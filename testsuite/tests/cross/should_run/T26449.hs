import Control.Monad
import GHC.Platform.ArchOS
import GHC.Platform.Host
import System.Info

main :: IO ()
main =
  when ((arch, os) /= (arch', os')) $
    fail $
      "System.Info says host platform is "
        <> show (arch, os)
        <> " but GHC.Platform.Host says "
        <> show (arch', os')
  where
    (arch', os') =
      (stringEncodeArch hostPlatformArch, stringEncodeOS hostPlatformOS)
