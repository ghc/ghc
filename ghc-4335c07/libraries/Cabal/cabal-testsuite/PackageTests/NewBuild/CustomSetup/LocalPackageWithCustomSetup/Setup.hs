import SetupDep (message)
import Distribution.Simple

main = putStrLn ("pkg Setup.hs: " ++ message) >> defaultMain
