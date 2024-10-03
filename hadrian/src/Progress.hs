module Progress (hadrianProgress) where

import Development.Shake

-- | A simple method for displaying progress messages, suitable for using as 'Development.Shake.shakeProgress'.
--   This is the shakeProgress function hadrian uses. It writes the current progress to the titlebar every five seconds
--   using 'progressTitlebar', and calls any @shake-progress@ program on the @$PATH@ using 'progressProgram'.
hadrianProgress :: String -> IO Progress -> IO ()
hadrianProgress cwd p = do
    program <- progressProgram
    progressDisplay 5 (\status -> let s = status<> "(" <> cwd <> ")" in progressTitlebar s >> program s) p

