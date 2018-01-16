import Module (message)
import Distribution.Simple

main = putStrLn ("Setup.hs: " ++ message) >> defaultMain
