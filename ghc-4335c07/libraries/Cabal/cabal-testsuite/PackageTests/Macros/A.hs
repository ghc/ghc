{-# LANGUAGE CPP #-}
import C
#ifdef VERSION_filepath
#error "Should not see macro from library"
#endif
#ifdef VERSION_containers
#error "Should not see macro from executable macros-b"
#endif
main = do
    putStrLn CURRENT_COMPONENT_ID
