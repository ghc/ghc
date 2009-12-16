import ObjLink

library_name = "libfoo_script_T2615.so" -- this is really a linker script

main = do
  result <- loadDLL library_name
  case result of
    Nothing -> putStrLn (library_name ++ " loaded successfully")
    Just x  -> putStrLn ("error: " ++ x)
