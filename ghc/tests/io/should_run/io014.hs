import IO -- 1.3

main = 
    accumulate (map hIsOpen [stdin, stdout, stderr]) >>= \ opens ->
    print opens >>
    accumulate (map hIsClosed [stdin, stdout, stderr]) >>= \ closeds ->
    print closeds >>
    accumulate (map hIsReadable [stdin, stdout, stderr]) >>= \ readables ->
    print readables >>
    accumulate (map hIsWritable [stdin, stdout, stderr]) >>= \ writables ->
    print writables >>
    accumulate (map hIsBlockBuffered [stdin, stdout, stderr]) >>= \ buffereds ->
    print buffereds >>
    accumulate (map hIsLineBuffered [stdin, stdout, stderr]) >>= \ buffereds ->
    print buffereds >>
    accumulate (map hIsNotBuffered [stdin, stdout, stderr]) >>= \ buffereds ->
    print buffereds
  where
    -- these didn't make it into 1.3
    hIsBlockBuffered h = hGetBuffering h >>= \ b -> return $ case b of { BlockBuffering _ -> True; _ -> False }
    hIsLineBuffered  h = hGetBuffering h >>= \ b -> return $ case b of { LineBuffering -> True; _ -> False }
    hIsNotBuffered   h = hGetBuffering h >>= \ b -> return $ case b of { NoBuffering -> True; _ -> False }
