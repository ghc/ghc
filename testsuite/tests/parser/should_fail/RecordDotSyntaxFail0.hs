{-# LANGUAGE RecordDotSyntax #-}

no = Foo { bar.baz = 1 }
  -- Syntax error: Can't use '.' in construction.
