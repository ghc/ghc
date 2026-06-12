module T27016b where
import T27016a

-- Like pprCLabelStyle in #27016: SPECIALISEd to the HLine instance.  The
-- generated rule is keyed on the normal form `HLine`.
pprLabel :: IsLine d => Int -> d
pprLabel n = if n == 0 then txt "0" else txt "x" <+> pprLabel (n-1)
{-# SPECIALIZE pprLabel :: Int -> HLine #-}
