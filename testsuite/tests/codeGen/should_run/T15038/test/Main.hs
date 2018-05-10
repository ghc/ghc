import qualified Parser as Parser

main :: IO ()
main = print (iterate Parser.byteParserBadOnce 5 !! 100000)
