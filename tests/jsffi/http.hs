-- A nodejs http client/server ping/pong test as a simple integration
-- test for JSFFI.

{-# LANGUAGE JavaScriptFFI #-}

import GHC.Wasm.Prim

newtype JSModule = JSModule JSVal

newtype JSServer = JSServer JSVal

newtype JSRequest = JSRequest JSVal

newtype JSResponse = JSResponse JSVal

newtype JSFunction = JSFunction JSVal

foreign import javascript safe "import($1)"
  js_import :: JSString -> IO JSModule

foreign import javascript safe "$1.text($2)"
  js_req_to_str :: JSModule -> JSRequest -> IO JSString

foreign import javascript unsafe "$1.createServer($2)"
  js_server_create :: JSModule -> JSFunction -> IO JSServer

foreign import javascript safe "new Promise(res => { $1.listen(0, () => { res($1.address().port); }); })"
  js_server_listen :: JSServer -> IO Int

foreign import javascript "wrapper"
  js_mk_req_handler :: (JSRequest -> JSResponse -> IO ()) -> IO JSFunction

foreign import javascript unsafe "$1.writeHead(200, { 'Content-Type': 'text/plain' })"
  js_resp_write_head :: JSResponse -> IO ()

foreign import javascript unsafe "$1.end($2)"
  js_resp_end :: JSResponse -> JSString -> IO ()

foreign import javascript safe "const r = await fetch($1, {method: 'POST', body: $2}); return r.text();"
  js_fetch_post :: JSString -> JSString -> IO JSString

foreign import javascript unsafe "$1.close()"
  js_server_close :: JSServer -> IO ()

foreign import javascript unsafe "console.log($1)"
  js_print :: JSString -> IO ()

foreign export javascript "main"
  main :: IO ()

main :: IO ()
main = do
  http <- js_import $ toJSString "node:http"
  stream_consumers <- js_import $ toJSString "node:stream/consumers"
  req_handler <- js_mk_req_handler $ \req resp -> do
    req_str <- fromJSString <$> js_req_to_str stream_consumers req
    js_resp_write_head resp
    js_resp_end resp $ toJSString $ "pong: " <> req_str
  server <- js_server_create http req_handler
  port <- js_server_listen server
  pong <- js_fetch_post (toJSString $ "http://localhost:" <> show port) (toJSString "ping")
  js_print pong
  js_server_close server
