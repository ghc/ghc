\begin{verbatim}

> module Main(main) where

>#ifndef __GLASGOW_HASKELL__
> import Trace
> import Maybe   -- an hbc library module defining the ``Maybe'' type
>#endif

> import Native  -- an hbc library module for native-mode binary IO

> import LPA     -- the linear predictive analysis module


> main = getContents	>>= \bs ->
>        putStr (program bs)
> {- 1.2
> main = readChan stdin exit                            $ \bs ->
>        appendChan stdout (program bs) exit done
> -}
> {- ORIGINAL: partain:
> main = getArgs exit                                   $ \args ->
>        case args of
>        [file1, file2] -> readFile file1 exit          $ \bs ->
>                          writeFile file2 (program bs) exit done
>        _              -> error usage
> -}

> usage = "usage: lpa  <speech file>  <output file>"


> window_width      = 384 :: Int  -- 24 ms @ 16 kHz
> window_offset     = 160 :: Int  -- 10 ms @ 16 kHz
> p                 =  14 :: Int  -- LP analysis order
> q                 =  16 :: Int  -- cepstral analysis order


> readRawSpeech :: Bytes -> Signal Int
> readRawSpeech bs =
>       case bytesToShortInt bs of
>       Nothing      -> if null bs then [] else error read_error
>       Just (v,bs') -> v : readRawSpeech bs'

> read_error = "Left-over byte encountered by readRawSpeech"


> castSignal :: Signal Int -> Signal Float
> castSignal = map fromInt


> program :: Bytes -> String
> {-TEST:
> program bs
>  = let signal = readRawSpeech bs in
>    -- trace (shows (take 200 signal) "\n\n") (
>    ((foldr writesFrame []
>         . map (analyze p q)
>         . windows window_width window_offset
>         . preemph 0.95
>         . castSignal) signal)
>    -- )
> -}
>
> program = foldr writesFrame []
>         . map (analyze p q)
>         . windows window_width window_offset
>         . preemph 0.95
>         . castSignal
>         . readRawSpeech

\end{verbatim}

        It only remains to define a function for writing the analysis
parameters to a file.
        \begin{verbatim}

> writesFrame :: (Float, [Float]) -> Bytes -> Bytes
> writesFrame (log_energy, cep) bs =
>       showBytes log_energy (listShowBytes cep bs)

\end{verbatim}

