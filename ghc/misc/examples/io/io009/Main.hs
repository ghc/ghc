import LibDirectory (getDirectoryContents)
import QSort (sort)

main =
    getDirectoryContents "." >>= \ names ->
    putText (sort names) >>
    putChar '\n'