import GHC.JS.Prim
import System.IO

foreign import javascript "((x) => { console.log(x); })"
  log_js_string :: JSVal -> IO ()

foreign import javascript "((x, y) => { return x === y; })"
  eq_JSVal :: JSVal -> JSVal -> Bool

foreign import javascript "(() => { return 'abc\\uD83D\\uDE0A'; })"
  js_utf16_string :: JSVal
foreign import javascript "(() => { return 'abc' + String.fromCodePoint(128522); })"
  js_codepoint_string :: JSVal

-- It's important that this String is defined using a function to avoid rewrite
-- rules optimising away the use of `toJSString` called on a literal.
hsString :: String
hsString = "abc" ++ "\128522"

emptyHsString :: String
emptyHsString = drop (length hsString) hsString

main :: IO ()
main = do
  putStr "Does JS `String.fromCodePoint` decode to the expected UTF-16 values? "
  print (eq_JSVal js_utf16_string js_codepoint_string)
  hFlush stdout
  log_js_string js_utf16_string
  log_js_string js_codepoint_string

  putStr "\nDoes `GHC.JS.fromJSString` convert the JavaScript literal string correctly? "
  print (hsString == fromJSString js_utf16_string)
  putStrLn hsString
  putStrLn (fromJSString js_utf16_string)

  putStr "\nDoes `GHC.JS.toJSString` convert the Haskell-defined string correctly? "
  print (eq_JSVal js_utf16_string (toJSString hsString))
  hFlush stdout
  log_js_string js_utf16_string
  log_js_string (toJSString hsString)

  putStr "\nDo values survive the Haskell -> JavaScript -> Haskell round-trip? "
  print (hsString == fromJSString (toJSString hsString))
  putStrLn hsString
  putStrLn (fromJSString js_utf16_string)

  putStr "\nDo values survive the JavaScript -> Haskell -> JavaScript round-trip? "
  print (eq_JSVal js_utf16_string (toJSString $ fromJSString js_utf16_string))
  hFlush stdout
  log_js_string js_utf16_string
  log_js_string (toJSString $ fromJSString js_utf16_string)

  putStr "\nDoes the empty string survive the Haskell -> JavaScript -> Haskell round-trip? "
  print (emptyHsString == fromJSString (toJSString emptyHsString))
