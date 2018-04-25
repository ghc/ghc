module LibFoo where
import Paths_foo
getData = readFile =<< getDataFileName "hello.txt"

