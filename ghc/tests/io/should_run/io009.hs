import Directory (getDirectoryContents)
import QSort (sort)

main =
    getDirectoryContents "." >>= \ names ->
    print (sort names)
