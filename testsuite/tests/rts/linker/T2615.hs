import GHCi.ObjLink

library_name = "libfoo_script_T2615.so" -- this is really a linker script

main = do
  initObjLinker RetainCAFs
  result <- loadDLL library_name
  case result of
    Right _ -> putStrLn (library_name ++ " loaded successfully")
    Left x  -> putStrLn ("error: " ++ x)
