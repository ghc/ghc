{-# LANGUAGE CPP #-}
import C
#ifdef VERSION_filepath
#error "Should not see macro from library"
#endif
#ifdef VERSION_directory
#error "Should not see macro from executable macros-a"
#endif
main = do
    putStrLn CURRENT_COMPONENT_ID
