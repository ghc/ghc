import GHC.JS.Prim

main :: IO ()
main = do
  print (isNull jsNull)
  print (isNull $ toJSString "")
  print (isNull $ toJSString "test")
  print (isNull $ toJSInt 0)
  print (isNull $ toJSInt 1)
  print (isNull $ toJSInt 2)
