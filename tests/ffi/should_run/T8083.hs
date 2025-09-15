
import Foreign
import Foreign.C
-- import System.IO

main :: IO ()
main = do
    -- hSetBuffering stdout NoBuffering
    n <- peek pn
    -- print n
    case n of
        1 -> do -- putStrLn "Calling blah"
                blah
        _ -> return ()

foreign import ccall "&n" pn :: Ptr CInt
foreign import ccall safe blah :: IO ()
