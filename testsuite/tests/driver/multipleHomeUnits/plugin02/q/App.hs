{-# OPTIONS_GHC -fplugin=MyPlugin #-}
module App where

-- The plugin 'MyPlugin' lives in home unit plugin-0, but this unit (app-0)
-- depends only on reexport-0, which *reexports* MyPlugin (it does not contain
-- MyPlugin itself). Resolving -fplugin must therefore follow the reexport into
-- the sibling home unit, exactly as an ordinary import does.
app :: ()
app = ()
