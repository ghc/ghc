%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[LibPosixTTY]{Haskell 1.3 POSIX Device-Specific Functions}

\begin{code}
module LibPosixTTY (
    BaudRate(..),
    ControlCharacter(..),
    FlowAction(..),
    QueueSelector(..),
    TerminalAttributes(..),
    TerminalMode(..),
    TerminalState(..),
    bitsPerByte,
    controlChar,
    controlFlow,
    discardData,
    drainOutput,
    getTerminalAttributes,
    getTerminalProcessGroupID,
    inputSpeed,
    inputTime,
    minInput,
    outputSpeed,
    sendBreak,
    setTerminalAttributes,
    setTerminalProcessGroupID,
    terminalMode,
    withBits,
    withCC,
    withInputSpeed,
    withMinInput,
    withMode,
    withOutputSpeed,
    withTime,
    withoutCC,
    withoutMode
    ) where

import PreludeGlaST

import LibPosixUtil

type TerminalAttributes = _ByteArray ()

data TerminalMode = InterruptOnBreak
                  | MapCRtoLF
		  | IgnoreBreak
		  | IgnoreCR
		  | IgnoreParityErrors
		  | MapLFtoCR
		  | CheckParity
		  | StripHighBit
		  | StartStopInput
		  | StartStopOutput
                  | MarkParityErrors
		  | ProcessOutput
		  | LocalMode
                  | ReadEnable
                  | TwoStopBits
                  | HangupOnClose
                  | EnableParity
                  | OddParity
                  | EnableEcho
                  | EchoErase
                  | EchoKill
                  | EchoLF
                  | ProcessInput
                  | ExtendedFunctions
                  | KeyboardInterrupts
                  | NoFlushOnInterrupt
                  | BackgroundWriteInterrupt

withoutMode :: TerminalAttributes -> TerminalMode -> TerminalAttributes
withoutMode termios InterruptOnBreak = clearInputFlag ``BRKINT'' termios
withoutMode termios MapCRtoLF = clearInputFlag ``ICRNL'' termios
withoutMode termios IgnoreBreak = clearInputFlag ``IGNBRK'' termios
withoutMode termios IgnoreCR = clearInputFlag ``IGNCR'' termios
withoutMode termios IgnoreParityErrors = clearInputFlag ``IGNPAR'' termios
withoutMode termios MapLFtoCR = clearInputFlag ``INLCR'' termios
withoutMode termios CheckParity = clearInputFlag ``INPCK'' termios
withoutMode termios StripHighBit = clearInputFlag ``ISTRIP'' termios
withoutMode termios StartStopInput = clearInputFlag ``IXOFF'' termios
withoutMode termios StartStopOutput = clearInputFlag ``IXON'' termios
withoutMode termios MarkParityErrors = clearInputFlag ``PARMRK'' termios
withoutMode termios ProcessOutput = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
	     ((struct termios *)%0)->c_oflag &= ~OPOST;'' bytes termios
						    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)
withoutMode termios LocalMode = clearControlFlag ``CLOCAL'' termios
withoutMode termios ReadEnable = clearControlFlag ``CREAD'' termios
withoutMode termios TwoStopBits = clearControlFlag ``CSTOPB'' termios
withoutMode termios HangupOnClose = clearControlFlag ``HUPCL'' termios
withoutMode termios EnableParity = clearControlFlag ``PARENB'' termios
withoutMode termios OddParity = clearControlFlag ``PARODD'' termios
withoutMode termios EnableEcho = clearLocalFlag ``ECHO'' termios
withoutMode termios EchoErase = clearLocalFlag ``ECHOE'' termios
withoutMode termios EchoKill = clearLocalFlag ``ECHOK'' termios
withoutMode termios EchoLF = clearLocalFlag ``ECHONL'' termios
withoutMode termios ProcessInput = clearLocalFlag ``ICANON'' termios
withoutMode termios ExtendedFunctions = clearLocalFlag ``IEXTEN'' termios
withoutMode termios KeyboardInterrupts = clearLocalFlag ``ISIG'' termios
withoutMode termios NoFlushOnInterrupt = setLocalFlag ``NOFLSH'' termios
withoutMode termios BackgroundWriteInterrupt = clearLocalFlag ``TOSTOP'' termios

withMode :: TerminalAttributes -> TerminalMode -> TerminalAttributes
withMode termios InterruptOnBreak = setInputFlag ``BRKINT'' termios
withMode termios MapCRtoLF = setInputFlag ``ICRNL'' termios
withMode termios IgnoreBreak = setInputFlag ``IGNBRK'' termios
withMode termios IgnoreCR = setInputFlag ``IGNCR'' termios
withMode termios IgnoreParityErrors = setInputFlag ``IGNPAR'' termios
withMode termios MapLFtoCR = setInputFlag ``INLCR'' termios
withMode termios CheckParity = setInputFlag ``INPCK'' termios
withMode termios StripHighBit = setInputFlag ``ISTRIP'' termios
withMode termios StartStopInput = setInputFlag ``IXOFF'' termios
withMode termios StartStopOutput = setInputFlag ``IXON'' termios
withMode termios MarkParityErrors = setInputFlag ``PARMRK'' termios
withMode termios ProcessOutput = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
	     ((struct termios *)%0)->c_oflag |= OPOST;'' bytes termios
						    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)
withMode termios LocalMode = setControlFlag ``CLOCAL'' termios
withMode termios ReadEnable = setControlFlag ``CREAD'' termios
withMode termios TwoStopBits = setControlFlag ``CSTOPB'' termios
withMode termios HangupOnClose = setControlFlag ``HUPCL'' termios
withMode termios EnableParity = setControlFlag ``PARENB'' termios
withMode termios OddParity = setControlFlag ``PARODD'' termios
withMode termios EnableEcho = setLocalFlag ``ECHO'' termios
withMode termios EchoErase = setLocalFlag ``ECHOE'' termios
withMode termios EchoKill = setLocalFlag ``ECHOK'' termios
withMode termios EchoLF = setLocalFlag ``ECHONL'' termios
withMode termios ProcessInput = setLocalFlag ``ICANON'' termios
withMode termios ExtendedFunctions = setLocalFlag ``IEXTEN'' termios
withMode termios KeyboardInterrupts = setLocalFlag ``ISIG'' termios
withMode termios NoFlushOnInterrupt = clearLocalFlag ``NOFLSH'' termios
withMode termios BackgroundWriteInterrupt = setLocalFlag ``TOSTOP'' termios

terminalMode :: TerminalMode -> TerminalAttributes -> Bool
terminalMode InterruptOnBreak = testInputFlag ``BRKINT''
terminalMode MapCRtoLF = testInputFlag ``ICRNL''
terminalMode IgnoreBreak = testInputFlag ``IGNBRK''
terminalMode IgnoreCR = testInputFlag ``IGNCR''
terminalMode IgnoreParityErrors = testInputFlag ``IGNPAR''
terminalMode MapLFtoCR = testInputFlag ``INLCR''
terminalMode CheckParity = testInputFlag ``INPCK''
terminalMode StripHighBit = testInputFlag ``ISTRIP''
terminalMode StartStopInput = testInputFlag ``IXOFF''
terminalMode StartStopOutput = testInputFlag ``IXON''
terminalMode MarkParityErrors = testInputFlag ``PARMRK''
terminalMode ProcessOutput = \ termios -> unsafePerformPrimIO (
    _casm_ ``%r = ((struct termios *)%0)->c_oflag & OPOST;'' termios
						    `thenPrimIO` \ (W# flags#) ->
    returnPrimIO (flags# `neWord#` int2Word# 0#))
terminalMode LocalMode = testControlFlag ``CLOCAL''
terminalMode ReadEnable = testControlFlag ``CREAD''
terminalMode TwoStopBits = testControlFlag ``CSTOPB''
terminalMode HangupOnClose = testControlFlag ``HUPCL''
terminalMode EnableParity = testControlFlag ``PARENB''
terminalMode OddParity = testControlFlag ``PARODD''
terminalMode EnableEcho = testLocalFlag ``ECHO''
terminalMode EchoErase = testLocalFlag ``ECHOE''
terminalMode EchoKill = testLocalFlag ``ECHOK''
terminalMode EchoLF = testLocalFlag ``ECHONL''
terminalMode ProcessInput = testLocalFlag ``ICANON''
terminalMode ExtendedFunctions = testLocalFlag ``IEXTEN''
terminalMode KeyboardInterrupts = testLocalFlag ``ISIG''
terminalMode NoFlushOnInterrupt = not . testLocalFlag ``NOFLSH''
terminalMode BackgroundWriteInterrupt = testLocalFlag ``TOSTOP''

bitsPerByte :: TerminalAttributes -> Int
bitsPerByte termios = unsafePerformPrimIO (
    _casm_ ``%r = ((struct termios *)%0)->c_cflag & CSIZE;'' termios
						    `thenPrimIO` \ w ->
    returnPrimIO (word2Bits w))
  where
    word2Bits :: _Word -> Int
    word2Bits x =
	if x == ``CS5'' then 5
	else if x == ``CS6'' then 6
	else if x == ``CS7'' then 7
	else if x == ``CS8'' then 8
	else 0

withBits :: TerminalAttributes -> Int -> TerminalAttributes
withBits termios bits = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
	     ((struct termios *)%0)->c_cflag = 
             (((struct termios *)%1)->c_cflag & ~CSIZE) | %2;'' 
	bytes termios (mask bits)		    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)
  where
    mask :: Int -> _Word
    mask 5 = ``CS5''
    mask 6 = ``CS6''
    mask 7 = ``CS7''
    mask 8 = ``CS8''
    mask _ = error "withBits bit value out of range [5..8]"

data ControlCharacter = EndOfFile
                      | EndOfLine
                      | Erase
                      | Interrupt
                      | Kill
                      | Quit
                      | Suspend
                      | Start
                      | Stop

controlChar :: TerminalAttributes -> ControlCharacter -> Maybe Char
controlChar termios cc = unsafePerformPrimIO (
    _casm_ ``%r = ((struct termios *)%0)->c_cc[%1];'' termios (cc2Word cc)
						    `thenPrimIO` \ val ->
    if val == ``_POSIX_VDISABLE'' then
	returnPrimIO Nothing
    else
	returnPrimIO (Just (chr val)))

withCC :: TerminalAttributes 
       -> (ControlCharacter, Char)
       -> TerminalAttributes
withCC termios (cc, c) = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
             ((struct termios *)%0)->c_cc[%2] = %3;'' 
	bytes termios (cc2Word cc) c		    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)

withoutCC :: TerminalAttributes 
          -> ControlCharacter 
          -> TerminalAttributes
withoutCC termios cc = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
             ((struct termios *)%0)->c_cc[%2] = _POSIX_VDISABLE;'' 
	bytes termios (cc2Word cc)		    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)

inputTime :: TerminalAttributes -> Int
inputTime termios = unsafePerformPrimIO (
    _casm_ ``%r = ((struct termios *)%0)->c_cc[VTIME];'' termios
						    `thenPrimIO` \ count ->
    returnPrimIO count)

withTime :: TerminalAttributes -> Int -> TerminalAttributes
withTime termios time = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
             ((struct termios *)%0)->c_cc[VTIME] = %2;'' bytes termios time
						    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)

minInput :: TerminalAttributes -> Int
minInput termios = unsafePerformPrimIO (
    _casm_ ``%r = ((struct termios *)%0)->c_cc[VMIN];'' termios
						    `thenPrimIO` \ count ->
    returnPrimIO count)

withMinInput :: TerminalAttributes -> Int -> TerminalAttributes
withMinInput termios count = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
             ((struct termios *)%0)->c_cc[VMIN] = %2;'' bytes termios count
						    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)

data BaudRate = B0 
              | B50 
              | B75 
              | B110 
              | B134 
              | B150 
              | B200 
              | B300 
              | B600
              | B1200 
              | B1800 
              | B2400 
              | B4800 
              | B9600 
              | B19200 
              | B38400

inputSpeed :: TerminalAttributes -> BaudRate
inputSpeed termios = unsafePerformPrimIO (
    _casm_ ``%r = cfgetispeed((struct termios *)%0);'' termios
						    `thenPrimIO` \ w ->
    returnPrimIO (word2Baud w))

withInputSpeed :: TerminalAttributes -> BaudRate -> TerminalAttributes
withInputSpeed termios br = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
             cfsetispeed((struct termios *)%0, %2);'' bytes termios (baud2Word br)
						    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)

outputSpeed :: TerminalAttributes -> BaudRate
outputSpeed termios = unsafePerformPrimIO (
    _casm_ ``%r = cfgetospeed((struct termios *)%0);'' termios
						    `thenPrimIO` \ w ->
    returnPrimIO (word2Baud w))

withOutputSpeed :: TerminalAttributes -> BaudRate -> TerminalAttributes
withOutputSpeed termios br = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
             cfsetospeed((struct termios *)%0, %2);'' bytes termios (baud2Word br)
						    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)

getTerminalAttributes :: Channel -> IO TerminalAttributes
getTerminalAttributes fd =
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``%r = tcgetattr(%0,(struct termios *)%1);'' fd bytes
						    `thenPrimIO` \ rc ->
    if rc /= -1 then
	freeze bytes				    `thenStrictlyST` \ termios ->
	return termios
    else
	syserr "getTerminalAttributes"

data TerminalState = Immediately
                   | WhenDrained 
                   | WhenFlushed

setTerminalAttributes :: Channel 
                      -> TerminalAttributes 
                      -> TerminalState
                      -> IO ()
setTerminalAttributes fd termios state =
    _casm_ ``%r = tcsetattr(%0,%1,(struct termios *)%2);'' fd (state2Int state) termios
						    `thenPrimIO` \ rc ->
    if rc /= -1 then
	return ()
    else
	syserr "setTerminalAttributes"
  where
    state2Int :: TerminalState -> Int
    state2Int Immediately = ``TCSANOW''
    state2Int WhenDrained = ``TCSADRAIN''
    state2Int WhenFlushed = ``TCSAFLUSH''

sendBreak :: Channel -> Int -> IO ()
sendBreak fd duration = 
    _ccall_ tcsendbreak fd duration		    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "sendBreak"

drainOutput :: Channel -> IO ()
drainOutput fd = 
    _ccall_ tcdrain fd				    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "drainOutput"

data QueueSelector = InputQueue 
		   | OutputQueue 
		   | BothQueues

discardData :: Channel -> QueueSelector -> IO ()
discardData fd queue =
    _ccall_ tcflush fd (queue2Int queue)	    `thenPrimIO` \ rc ->
    if rc /= -1 then
	return ()
    else
	syserr "discardData"
  where
    queue2Int :: QueueSelector -> Int
    queue2Int InputQueue  = ``TCIFLUSH''
    queue2Int OutputQueue = ``TCOFLUSH''
    queue2Int BothQueues  = ``TCIOFLUSH''

data FlowAction = SuspendOutput 
                | RestartOutput 
                | TransmitStop 
                | TransmitStart

controlFlow :: Channel -> FlowAction -> IO ()
controlFlow fd action = 
    _ccall_ tcflow fd (action2Int action)	    `thenPrimIO` \ rc ->
    if rc /= -1 then
	return ()
    else
	syserr "controlFlow"
  where
    action2Int :: FlowAction -> Int
    action2Int SuspendOutput = ``TCOOFF''
    action2Int RestartOutput = ``TCOON''
    action2Int TransmitStop  = ``TCIOFF''
    action2Int TransmitStart = ``TCION''

getTerminalProcessGroupID :: Channel -> IO ProcessGroupID
getTerminalProcessGroupID fd =
    _ccall_ tcgetpgrp fd			    `thenPrimIO` \ pgid ->
    if pgid /= -1 then
	return pgid
    else
	syserr "getTerminalProcessGroupID"

setTerminalProcessGroupID :: Channel -> ProcessGroupID -> IO ()
setTerminalProcessGroupID fd pgid =
    _ccall_ tcsetpgrp fd pgid			    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "setTerminalProcessGroupID"

\end{code}

Local utility functions

\begin{code}

-- Convert Haskell ControlCharacter to Int

cc2Word :: ControlCharacter -> _Word
cc2Word EndOfFile = ``VEOF''
cc2Word EndOfLine = ``VEOL''
cc2Word Erase     = ``VERASE''
cc2Word Interrupt = ``VINTR''
cc2Word Kill      = ``VKILL''
cc2Word Quit      = ``VQUIT''
cc2Word Suspend   = ``VSUSP''
cc2Word Start     = ``VSTART''
cc2Word Stop      = ``VSTOP''

-- Convert Haskell BaudRate to unsigned integral type (_Word)

baud2Word :: BaudRate -> _Word
baud2Word B0 = ``B0''
baud2Word B50 = ``B50''
baud2Word B75 = ``B75''
baud2Word B110 = ``B110''
baud2Word B134 = ``B134''
baud2Word B150 = ``B150''
baud2Word B200 = ``B200''
baud2Word B300 = ``B300''
baud2Word B600 = ``B600''
baud2Word B1200 = ``B1200''
baud2Word B1800 = ``B1800''
baud2Word B2400 = ``B2400''
baud2Word B4800 = ``B4800''
baud2Word B9600 = ``B9600''
baud2Word B19200 = ``B19200''
baud2Word B38400 = ``B38400''

-- And convert a word back to a baud rate
-- We really need some cpp macros here.

word2Baud :: _Word -> BaudRate
word2Baud x =
    if x == ``B0'' then B0
    else if x == ``B50'' then B50
    else if x == ``B75'' then B75
    else if x == ``B110'' then B110
    else if x == ``B134'' then B134
    else if x == ``B150'' then B150
    else if x == ``B200'' then B200
    else if x == ``B300'' then B300
    else if x == ``B600'' then B600
    else if x == ``B1200'' then B1200
    else if x == ``B1800'' then B1800
    else if x == ``B2400'' then B2400
    else if x == ``B4800'' then B4800
    else if x == ``B9600'' then B9600
    else if x == ``B19200'' then B19200
    else if x == ``B38400'' then B38400
    else error "unknown baud rate"

-- Clear termios i_flag

clearInputFlag :: _Word -> TerminalAttributes -> TerminalAttributes
clearInputFlag flag termios = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
	     ((struct termios *)%0)->c_iflag &= ~%2;'' bytes termios flag
						    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)

-- Set termios i_flag

setInputFlag :: _Word -> TerminalAttributes -> TerminalAttributes
setInputFlag flag termios = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
	     ((struct termios *)%0)->c_iflag |= %2;'' bytes termios flag
						    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)

-- Examine termios i_flag

testInputFlag :: _Word -> TerminalAttributes -> Bool
testInputFlag flag termios = unsafePerformPrimIO (
    _casm_ ``%r = ((struct termios *)%0)->c_iflag & %1;'' termios flag
						    `thenPrimIO` \ (W# flags#) ->
    returnPrimIO (flags# `neWord#` int2Word# 0#))

-- Clear termios c_flag

clearControlFlag :: _Word -> TerminalAttributes -> TerminalAttributes
clearControlFlag flag termios = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
	     ((struct termios *)%0)->c_cflag &= ~%2;'' bytes termios flag
						    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)

-- Set termios c_flag

setControlFlag :: _Word -> TerminalAttributes -> TerminalAttributes
setControlFlag flag termios = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
	     ((struct termios *)%0)->c_cflag |= %2;'' bytes termios flag
						    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)

-- Examine termios c_flag

testControlFlag :: _Word -> TerminalAttributes -> Bool
testControlFlag flag termios = unsafePerformPrimIO (
    _casm_ ``%r = ((struct termios *)%0)->c_cflag & %1;'' termios flag
						    `thenPrimIO` \ (W# flags#) ->
    returnPrimIO (flags# `neWord#` int2Word# 0#))

-- Clear termios l_flag

clearLocalFlag :: _Word -> TerminalAttributes -> TerminalAttributes
clearLocalFlag flag termios = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
	     ((struct termios *)%0)->c_lflag &= ~%2;'' bytes termios flag
						    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)

-- Set termios l_flag

setLocalFlag :: _Word -> TerminalAttributes -> TerminalAttributes
setLocalFlag flag termios = unsafePerformPrimIO (
    allocChars ``sizeof(struct termios)''	    `thenStrictlyST` \ bytes ->
    _casm_ ``*(struct termios *)%0 = *(struct termios *)%1;
	     ((struct termios *)%0)->c_lflag |= %2;'' bytes termios flag
						    `thenPrimIO` \ () ->
    freeze bytes				    `thenStrictlyST` \ termios ->
    returnPrimIO termios)

-- Examine termios l_flag

testLocalFlag :: _Word -> TerminalAttributes -> Bool
testLocalFlag flag termios = unsafePerformPrimIO (
    _casm_ ``%r = ((struct termios *)%0)->c_iflag & %1;'' termios flag
						    `thenPrimIO` \ (W# flags#) ->
    returnPrimIO (flags# `neWord#` int2Word# 0#))

\end{code}
