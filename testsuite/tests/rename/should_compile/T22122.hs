{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module T22122 where

import T22122_aux ( data_decls, record_upds )

-- This test checks that we can handle record declarations and updates
-- when the field 'Name's share the same underlying string.

-- data D1 = MkD1 { fld1 :: Char, fld2 :: String }
-- data D2 = MkD2A { fld1 :: Char } | MkD2B { fld2 :: String }
$(return data_decls)

-- rec_upd r = r { fld1 = 'c', fld2 = "foo" }
$(return record_upds)
