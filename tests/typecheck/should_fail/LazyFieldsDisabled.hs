module LazyFieldsDisabled where

data A = A { lazy :: ~Int }
