-- Ambiguity on the export list
{-# LANGUAGE DuplicateRecordFields #-}

module T24452f (S(foo)) where
    import AmbigPatSynA
    import AmbigPatSynB
    data S 
