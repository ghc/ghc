module ShouldFail where

data Bar = Bar { flag :: Bool } deriving( Show )

data State = State { bar :: Bar, baz :: Float }

display :: State -> IO ()
display (State{ bar = Bar { flag = f, baz = b }}) = print (f,b)

-- Typo! The line above should better be:
-- display (State{ bar = Bar { flag = f }, baz = b }) = print (f,b)

-- GHC 4.04 (as released) crashed with
--	panic! (the `impossible' happened): tcLookupValue: b{-r4n-}
-- Bug reported by Sven Panne

