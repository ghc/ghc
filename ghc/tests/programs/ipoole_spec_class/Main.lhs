\input{LiterateUtils}
{
\DownLevel
\author{Ian Poole}
\filetitle{Specimen classification (Animation)}
\maybemaketitle
\noindent{\verb{!%W% %G%!}
\begin{vb}

> module Main (main) where
> import GoferPreludeBits
> import Io
> import Lib
> import JobImp
> import JobApp
> import Lognum


\end{verbatim}\end{vb}

Here we give the Gofer specification of the Cytoline/ILDAS
specimen classifier.  This is based on the
description given in appendix C of the software requirements
specification.  Note however that the scheme here
is modified to admit that even normal specimens contain 
abnormal objects.   Thus, beta distributions are allowed
for both the normal and abnormal specimens.


The purpose of the specimen classifier is to integrate all 
evidence from the low and high resolution passes (and in
Cytoline, from interactive review), to deliver a decision
for the slide:
\footnote{We will consistently adopt the order Abnormal, Normal
 Leukocyte, Junk for object classes, which we
will abbreviate to {\tt A, N, L, J}, and Abnormal, Normal for
specimen classifications, which we will abbreviate as {\tt Abn, Nrm}}

\begin{Dec}{SpecDecision}
The specimen level decision.
\begin{vb}

> data SpecDecision = SignOut | Review

\end{verbatim}\end{vb}\end{Dec}


\sectionH{Specimen level evidence}

\begin{Dec}{SpecEvidence}
We assume that all evidence is expressed as likelihoods of observed
features given each the four object classes, Normal, Abnormal, Leukocyte,
Junk.  A posterior probability is assumed to exist for every object seen,
based (only) on the most refined evidence available.
\begin{vb}

> type Prob = Double
> type SpecEvidence = [(Int, ObjProbVec)]
> type ObjProbVec = (Prob, Prob, Prob, Prob)

\end{verbatim}\end{vb}\end{Dec}


\begin{Def}{composeEvidence}

Extract object level evidence from the low and high resolution
target structures.   The evidence those objects which were not rescaned
is fabricated by replicating the mean PP within the object class, 
according to the count in that object class.
\begin{vb}

> type TargetList =  ()
> type TargetRecord = ()

> composeEvidence :: TargetList -> TargetList -> SpecEvidence
> composeEvidence high low
>       = extractEvidence low ++ extractEvidence high
>       where
>       extractEvidence ts
>               = [(1, getPPs t) | t <- ts] ++ 
>                 [(countA', meanPPA),
>                  (countN, meanPPN),
>                  (countL, meanPPL),
>                  (countJ, meanPPJ)]
>                  
>                 where
>                        countA' = countA - length ts  -- avoid double count

The extraction functions should come from the TargetList ADT,
but we leave this as yet unresolved.

>                        getPPs :: TargetRecord -> ObjProbVec
>                        getPPs = undefined
>                        (ts,
>                         countA, countN, countL, countJ,
>                         meanPPA, meanPPN, meanPPL, meanPPJ)
>                              = extractFromStruct ts
>                        extractFromStruct ts = undefined
>       

\end{verbatim}\end{vb}\end{Def}

\sectionH{Specimen level model}

\begin{Dec}{SpecModel}
It is assumed that the following model parameters are known:
\begin{itemize}
\item {\tt specPriorSM}      --- Prior probability for specimen classification 
\item {\tt objProportionsSM} --- Mean proportions of objects, by class, on normal and
      abnormal specimens.
\item {\tt cvAbnObjSM}       --- Coefficient of variation in proportion of abnormal cells 
      on normal and abnormal specimens.
\item {\tt fnrSM}            --- Acceptable false negative rate.
\end{itemize}
There is facility to read parameters from a file.
\begin{vb}

> type SpecModel = SpecModelImp 

#ifdef Gofer

>	in
>	specPriorSM, abnPropsSM, qLqJSM, cvAbnObjSM, betaParamsSM, fnrSM,
>	calibFuncSM, readSpecModelSM, testSpecModelSM

#endif Gofer

> specPriorSM      :: SpecModel -> (Double, Double)
> abnPropsSM       :: SpecModel -> (Double, Double)
> qLqJSM           :: SpecModel -> (Double, Double)
> cvAbnObjSM       :: SpecModel -> (Double, Double)
> betaParamsSM     :: SpecModel -> (BetaParams, BetaParams)
> fnrSM            :: SpecModel -> Double
> calibFuncSM      :: SpecModel -> (Double -> Double)
> readSpecModelSM  :: FileName  -> Job s s SpecModel
> testSpecModelSM  :: SpecModel
> 
> type FileName = String
> type BetaParams = (Double, Double, Lognum)

> type SpecModelImp = ((Double, Double), (Double, Double), (Double, Double),
>                      (Double, Double), (BetaParams, BetaParams), Double)
>                        
> readSpecModelSM = undefined

> specPriorSM (a,b,c,d,e,f)      = a
> abnPropsSM (a,b,c,d,e,f)       = b
> qLqJSM (a,b,c,d,e,f)	       = c
> cvAbnObjSM (a,b,c,d,e,f)       = d
> betaParamsSM (a,b,c,d,e,f)     = e
> fnrSM (a,b,c,d,e,f)            = f
> calibFuncSM (a,b,c,d,e,f) = id

> calcBetaParams :: SpecModel -> (BetaParams, BetaParams)
> calcBetaParams sm
>	= (betaAbn, betaNrm)
>	  where
>	      betaAbn = cv qAbnA cvAbn
>	      betaNrm = cv qNrmA cvNrm
>	      (qAbnA, qNrmA) = abnPropsSM sm
>	      (cvAbn, cvNrm) = cvAbnObjSM sm

> betaAbnDist sm  = beta p 
>	where
>	(_,p) = betaParamsSM sm



\end{verbatim}\end{vb}\end{Dec}


\sectionH{Specimen classifier decision}

\begin{Def}{SpecClassifier}
Make a Sign-out / Review decision on the basis of evidence
from all objects seen.   The decision is a simple threshold
--- is the predicted false negative rate (fnr) acceptable
in the light of the posterior probability of abnormality
and specimen prior?

\begin{vb}

> type SpecClass a = (a,a) -> a
> classAbn = fst
> classNrm = snd

> specClassifier :: SpecModel -> SpecEvidence -> SpecDecision
> specClassifier sm evidence
>
>       = if (truePostProb / pAbn) < fnr then
>               SignOut
>         else
>               Review
>
>       where
>            truePostProb = calibFunc (pAbnGvnEv sm evidence)
>            (pAbn, _) = specPriorSM sm
>            calibFunc = calibFuncSM sm
>            fnr       = fnrSM sm

\end{verbatim}\end{vb}\end{Def}

\sectionH{Specimen posterior probability}

\begin{Def}{pAbnGvnEv}
Compute the posterior probability $P(S=\mbox{Abn} | {\bf x})$.
\begin{vb}

> type LikelyHood = Prob
> type LikeVec = [LikelyHood]

> pAbnGvnEv :: SpecModel -> SpecEvidence -> Prob
> pAbnGvnEv sm evidence  
>
>           = toDouble (
>             (pEvGvnAbn * pAbnL) / 
>             (pEvGvnAbn * pAbnL + pEvGvnNrm * pNrmL)) -- Bayes theorem

>       where
>          pEvGvnAbn = lklf sm betaAbn likelihoods
>          pEvGvnNrm = lklf sm betaNrm likelihoods

Convert the evidence, which is in the  form of posterior probably vectors
on objects, into likelyhood vectors on objects.

>          likelihoods :: SpecEvidence
>          likelihoods = [(c,bayes ppvec) | (c,ppvec) <- evidence]
>                     where
>                     bayes (ppA,ppN,ppL,ppJ) =
>                               (ppA/pA, ppN/pN, ppL/pL, ppJ/pJ)
   
Form the marginal object priors from the specimen-conditional priors.

>
>          pA = let it = qAbnA * pAbn + qNrmA * pNrm in it
>          pN = let it = 1.0 - (pA + pL + pJ) in it

Extract model parameters.

>	   (pAbnL, pNrmL) = (toLognum pAbn, toLognum pNrm)
>          (pAbn, pNrm)                 = specPriorSM sm
>	   (pL, pJ)                     = qLqJSM sm
>          (betaAbn, betaNrm)           = let it = betaParamsSM sm in it
>          (qAbnA, qNrmA)		= abnPropsSM sm

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{lklf}
Computes the likelyhood $p({\bf x} | S=s)$. The specimen class, $s$,
(Normal or Abnormal) is passed by the within
specimen class objectPriors ({\tt priors}) and coefficient of
variation of abnormal objects {\tt v}).   ${\bf x}$, representing
feature data for all observed objects, is passed as the list
of likelyhood vectors for for each object.  In fact, these likelihoods
are are required only to proportionality.
\begin{vb}

> lklf :: SpecModel -> BetaParams -> SpecEvidence -> Lognum
> lklf sm betaParams evi =
>      integratePowN 6 (fromRational 0.000001, fromRational 0.999) 
>          (\ r ->
>		let r' = toDouble r in
>               beta betaParams r *
>               product [toLognum (let y =
>                                 	lA * r' * (1.0 - q) +
>	                                lN * (1.0 - r')*(1.0 - q) +
>       	                        lL * qL +
>               	                lJ * qJ 
>				   in 
>--                                     trace 
>--                                     ("y="++show y ++ " r=" ++ show r ++ 
>--                                      " r'="++ show r' ++ " lA=" ++ show lA) 
>                                        y)`pow` c
>                              | (c,(lA,lN,lL,lJ)) <- evi]
>          )
>       where 
>		(qL, qJ) = qLqJSM sm
>               q = qL + qJ

> pow :: (Real b) => Lognum -> b -> Lognum
 
> (LN x) `pow` n = LN (x* toDouble n)

\end{verbatim}\end{vb}\end{Def}




\sectionH{Function integration}

(These definitions more properly belong in a library module).

\begin{Def}{integrate, limitFromAbove}~~~

\begin{itemize}
\item
        \verb@integrate (a,b) f@ = $ \int_a^b f (x) dx $
\item
        \verb@limitFromAbove a f@ = $  \lim_{x \rightarrow a} f (x) $
\end{itemize}

\begin{vb} 

> integrate :: (Enum a, Fractional a) => 
>              (a, a) -> (a -> a) -> a
> integrate (a, b) f =
>--       limitFromAbove zero 
>--              (\dx -> sum [ (f x) * dx | x <- [a, a+dx ..b]])
>       simpsons (a,b) f

> integratePowN :: (Enum a, Fractional a, Floating a) => 
>                Int -> (a, a) -> (a -> a) -> a
> integratePowN n (a,b) f
>	= integrate (invufunc a, invufunc b) (\u -> (f . ufunc) u * dxdu u)
>		      where
>			invufunc x = x ** (fromInt 1 / fromInt n)
>			ufunc u = u ^ n
>			dxdu u = (u ^ (n-1)) * fromInt n

> simpsons :: (Enum a, Fractional a) =>
>		(a, a) -> (a -> a) -> a
> simpsons (a,b) f
>	= (h/v3) * 
>         (f a + f b + v4 * odds + v2 * evens)
>	  where
>	    h = ((b-a)/fromInt(n-1))
>	    odds =  (sum) [f x | x <- (take ((n-1)`div`2) [a+h,    a+v3*h..])]
>	    evens = (sum) [f x | x <- (take ((n-3)`div`2) [a+v2*h, a+v4*h..])]
>	    n=201
>	    [v1,v2,v3,v4] = map fromInt [1..4]
>          

\end{verbatim}\end{vb}\end{Def}

\sectionH{The beta distribution}

A beta distribution is used to model the proportion of abnormal objects
on normal and abnormal specimens.

\begin{Def}{beta}
This is a distribution of a continuous variable in the range 0 to 1.
\begin{vb}

> beta :: BetaParams -> Lognum -> Lognum
> beta (a,b,n) x =
>	  f x / n
>         where
>         f x = (x `pow` (a-1.0)) * (((one - x) `pow` (b-1.0)))
>	  one = fromInt 1

\end{verbatim}\end{vb}\end{Def}

\sectionH{Testing}

\begin{vb}

> testSpecModelSM 
> 	= sm 
>	where     
>		sm   = ((0.5, 0.5), 			-- P(S)
>		       (0.005, 0.0002),			-- (P(O=A|S=Abn), P(O=A|S=Nrm))
>		       (0.2,0.2),			-- qL, qN
>		       (0.7, 0.7),                      -- cv - (Abn, Nrm)
>		        calcBetaParams sm,		-- beta dist parmaeters
>			0.01)                           -- FNR

> showSpecModel sm
>	= unlines [s "P(S)=" ++ (show . specPriorSM) sm,
>		   s "(P(O=A|S=Abn), P(O=A|S=Nrm))=" ++ (show . abnPropsSM) sm,
>		   s "(qL, qN)=" ++ (show . qLqJSM) sm,
>		   s "cvAbnObj=" ++ (show . cvAbnObjSM) sm]
>	  where s = (ljustify 33 . show)

> nrm = (0.0, 1.0, 0.0, 0.0)
> abn = (1.0, 0.0, 0.0, 0.0)

> runS nAbn nNrm =
>          pAbnGvnEv testSpecModelSM [(nAbn, abn), (nNrm, nrm)]


> cv :: Double -> Double -> BetaParams
> cv m v = (a, b,n)
>	   where
>               a = (1.0 - m) / v^2 - m
>               b = a / m - a
>		n = integratePowN 6 
>			(toLognum 0.000001, toLognum 0.999) 
>			(\x-> beta (a,b, toLognum 1) x)


> main = go (
>		(putLine . showSpecModel) testSpecModelSM >>
>		putLine (tabulate2D 5
>                     [0, 10]
>                     [0, 100, 20000]
>		       runS))

}
\EndFile


  tabulate2D :: (Show{-was:Text-} a, Show{-was:Text-} b, Show{-was:Text-} c) => Int -> [a] -> [b] -> (a->b->c) -> String

> tabulate2D w alist blist f
>	= (jshow2' " ") ++ "   " ++ concat (map jshow alist) ++ "\n\n" ++
>	  unlines [jshow2 b ++ " : " ++ concat [jshow (f a b) | a <- alist ] 
>                | b <- blist]
>	  where
>	  jshow x =  (ljustify w (take w (showFixed x))) ++ " "
>	  jshow2 x = (ljustify (w*2) (take w (showFixed x))) ++ " "
>	  jshow2' x = (ljustify (w*2) (take w (show x))) ++ " "
>	  

> tabulate1D :: (Show{-was:Text-} a, Show{-was:Text-} b) => Int -> [a] -> (a->b) -> String
> tabulate1D w alist f
>	= unlines [jshow a ++ "   " ++ jshow (f  a)
>                | a <- alist]
>	  where
>	  jshow x = (ljustify w (show x))
>	  

Showfixed formats a Real in *non* exponent form.  
Big hack --- must be a better way !?!

> showFixed :: Real a => a -> String
> showFixed x
>	| dx >= 1.0 = show x
>	| dx < 0.0000001 = show x
>	| otherwise = showFixed' 0 dx
>	  where
>	  dx = toDouble x
>	  showFixed' n x = if x > 0.1 then
>			      "0." ++ (take n (repeat '0')) ++ show (round (x*1000000.0))
>			   else
>			      showFixed' (n+1) (x*fromInt 10)
>			   

