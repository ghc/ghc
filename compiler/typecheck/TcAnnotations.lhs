%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[TcAnnotations]{Typechecking annotations}

\begin{code}
module TcAnnotations ( tcAnnotations ) where

import HsSyn
import Annotations
import Name
import TcRnMonad
import SrcLoc
import Outputable

#ifdef GHCI
import Module
import TcExpr
import {-# SOURCE #-} TcSplice ( runAnnotation )
import FastString
#endif
\end{code}

\begin{code}
tcAnnotations :: [LAnnDecl Name] -> TcM [Annotation]
tcAnnotations = mapM tcAnnotation

tcAnnotation :: LAnnDecl Name -> TcM Annotation
#ifndef GHCI
-- TODO: modify lexer so ANN pragmas are parsed as comments in a stage1 compiler, so developers don't see this error
tcAnnotation (L _ (HsAnnotation _ expr)) = pprPanic "Cant do annotations without GHCi" (ppr expr)
#else
tcAnnotation ann@(L loc (HsAnnotation provenance expr)) = do
    -- Work out what the full target of this annotation was
    mod <- getModule
    let target = annProvenanceToTarget mod provenance
    
    -- Run that annotation and construct the full Annotation data structure
    setSrcSpan loc $ addErrCtxt (annCtxt ann) $ addExprErrCtxt expr $ runAnnotation target expr

annProvenanceToTarget :: Module -> AnnProvenance Name -> AnnTarget Name
annProvenanceToTarget _   (ValueAnnProvenance name) = NamedTarget name
annProvenanceToTarget _   (TypeAnnProvenance name)  = NamedTarget name
annProvenanceToTarget mod ModuleAnnProvenance       = ModuleTarget mod

annCtxt :: OutputableBndr id => LAnnDecl id -> SDoc
annCtxt ann
  = hang (ptext (sLit "In the annotation:")) 2 (ppr ann)
#endif
\end{code}