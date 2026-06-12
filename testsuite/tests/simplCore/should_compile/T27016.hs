module T27016 where
import T27016a
import T27016b

-- Like pprNatCmmDecl in #27016: SPECIALISEd to HDoc.  Inside, pprTop @HDoc
-- calls pprLabel @(Line HDoc).  `Line HDoc` is an un-reduced type-family
-- application, so before #27016 was fixed the "USPEC pprLabel @HLine" rule
-- did not fire here.  With call type-argument normalisation it does.
pprTop :: IsDoc d => Int -> d
pprTop n = line (pprLabel n)
{-# SPECIALIZE pprTop :: Int -> HDoc #-}
