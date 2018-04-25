import Control.Monad (forM_)

main :: IO ()
main = forM_ [0..0xffff] $ \i -> do
   putStrLn $ ".section s" ++ show i ++ ",\"\",@progbits"
   putStrLn $ ".asciz \"Section " ++ show i ++ "\""
   putStrLn ""
