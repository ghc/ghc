{-# LANGUAGE RecordDotSyntax #-}

no Foo { bar.baz = x } = undefined
  -- Syntax error: Field selector syntax doesn't participate
  -- in patterns
