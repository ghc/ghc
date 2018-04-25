import Distribution.Simple
import RemoteSetupDep (message)

main = putStrLn ("remote-pkg Setup.hs: " ++ message) >> defaultMain
