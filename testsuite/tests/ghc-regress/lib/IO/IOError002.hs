-- !!! IOErrors should have Eq defined

import IO

main = print (userError "urk" == userError "urk")
