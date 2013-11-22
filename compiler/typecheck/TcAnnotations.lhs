%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[TcAnnotations]{Typechecking annotations}

\begin{code}
module TcAnnotations ( tcAnnotations, annCtxt ) where

#ifdef GHCI
import {-# SOURCE #-} TcSplice ( runAnnotation )
import Module
#endif

import HsSyn
import Annotations
import Name
import TcRnMonad
import SrcLoc
import Outputable

import FastString
\end{code}

\begin{code}

#ifndef GHCI

tcAnnotations :: [LAnnDecl Name] -> TcM [Annotation]
-- No GHCI; emit a warning (not an error) and ignore. cf Trac #4268
tcAnnotations [] = return []
tcAnnotations anns@(L loc _ : _)
  = do { setSrcSpan loc $ addWarnTc $
             (ptext (sLit "Ignoring ANN annotation") <> plural anns <> comma
             <+> ptext (sLit "because this is a stage-1 compiler or doesn't support GHCi"))
       ; return [] }

#else

tcAnnotations :: [LAnnDecl Name] -> TcM [Annotation]
-- GHCI exists, typecheck the annotations
tcAnnotations anns = mapM tcAnnotation anns

tcAnnotation :: LAnnDecl Name -> TcM Annotation
tcAnnotation (L loc ann@(HsAnnotation provenance expr)) = do
    -- Work out what the full target of this annotation was
    mod <- getModule
    let target = annProvenanceToTarget mod provenance

    -- Run that annotation and construct the full Annotation data structure
    setSrcSpan loc $ addErrCtxt (annCtxt ann) $ runAnnotation target expr

annProvenanceToTarget :: Module -> AnnProvenance Name -> AnnTarget Name
annProvenanceToTarget _   (ValueAnnProvenance name) = NamedTarget name
annProvenanceToTarget _   (TypeAnnProvenance name)  = NamedTarget name
annProvenanceToTarget mod ModuleAnnProvenance       = ModuleTarget mod
#endif

annCtxt :: OutputableBndr id => AnnDecl id -> SDoc
annCtxt ann
  = hang (ptext (sLit "In the annotation:")) 2 (ppr ann)
\end{code}