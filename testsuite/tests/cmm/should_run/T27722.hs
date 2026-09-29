foreign import ccall "zork_info_addr" zork_info_addr :: IO Word

main :: IO ()
main = do
  addr <- zork_info_addr
  putStrLn (if addr /= 0 then "ok" else "no info table")
