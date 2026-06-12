{-# OPTIONS_GHC -fplugin=MyPlugin #-}
module App where

-- The plugin 'MyPlugin' lives in a *sibling* home unit (plugin-0), not in a
-- registered package. Resolving -fplugin must therefore search sibling home
-- units, just like an ordinary import does.
app :: ()
app = ()
