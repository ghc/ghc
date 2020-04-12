-- Build.hs
{-# LANGUAGE ApplicativeDo #-}
module Build (configRules) where

type Action = IO
type Rules = IO

type Config = ()

(%>) :: String -> (String -> Action ()) -> Rules ()
(%>) = undefined

command_ :: [String] -> String -> [String] -> Action ()
command_ = undefined

recursive :: Config -> String -> [String] -> IO (FilePath, [String])
recursive = undefined

liftIO :: IO a -> Action a
liftIO = id

need :: [String] -> Action ()
need = undefined

historyDisable :: Action ()
historyDisable = undefined

get_config :: () -> Action Config
get_config = undefined

configRules :: Rules ()
configRules = do
  "snapshot" %> \out -> do
    historyDisable -- 8.10-rc1 refuses to compile without bind here
    config <- get_config ()
    need []
    (exe,args) <- liftIO $ recursive config "snapshot" []
    command_ [] exe args
