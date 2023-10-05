import GHC.JS.Prim

foreign import javascript "((x) => { return undefined; })"
  js_undefined :: JSVal

main :: IO ()
main = do
  print (isUndefined js_undefined)
  print (isUndefined $ toJSString "")
  print (isUndefined $ toJSString "test")
  print (isUndefined $ toJSInt 0)
  print (isUndefined $ toJSInt 1)
  print (isUndefined $ toJSInt 2)
