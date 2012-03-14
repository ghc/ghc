-- !!! IOErrors should have Eq defined

import System.IO

main = print (userError "urk" == userError "urk")
