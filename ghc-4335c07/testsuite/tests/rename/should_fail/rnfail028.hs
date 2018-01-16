-- !!! illegal to export a module we haven't imported.
module ShouldFail ( module List ) where
