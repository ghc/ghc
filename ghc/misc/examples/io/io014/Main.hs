main = 
    accumulate (map hIsOpen [stdin, stdout, stderr]) >>= \ opens ->
    putText opens >>
    putChar '\n' >>
    accumulate (map hIsClosed [stdin, stdout, stderr]) >>= \ closeds ->
    putText closeds >>
    putChar '\n' >>
    accumulate (map hIsReadable [stdin, stdout, stderr]) >>= \ readables ->
    putText readables >>
    putChar '\n' >>
    accumulate (map hIsWritable [stdin, stdout, stderr]) >>= \ writables ->
    putText writables >>
    putChar '\n' >>
    accumulate (map hIsBlockBuffered [stdin, stdout, stderr]) >>= \ buffereds ->
    putText buffereds >>
    putChar '\n' >>
    accumulate (map hIsLineBuffered [stdin, stdout, stderr]) >>= \ buffereds ->
    putText buffereds >>
    putChar '\n' >>
    accumulate (map hIsNotBuffered [stdin, stdout, stderr]) >>= \ buffereds ->
    putText buffereds >>
    putChar '\n'
