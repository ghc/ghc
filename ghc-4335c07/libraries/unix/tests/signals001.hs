{-# LANGUAGE CPP #-}

import System.Posix.Signals

#include "ghcconfig.h"

main = do
  print (testMembers emptySignalSet)
  print (testMembers emptyset)
  print (testMembers fullSignalSet)
  print (testMembers fullset)

fullset = internalAbort `addSignal`
	  realTimeAlarm `addSignal`
	  busError `addSignal`
	  processStatusChanged `addSignal`
	  continueProcess `addSignal`
	  floatingPointException `addSignal`
	  lostConnection `addSignal`
	  illegalInstruction `addSignal`
	  keyboardSignal `addSignal`
	  killProcess `addSignal`
	  openEndedPipe `addSignal`
	  keyboardTermination `addSignal`
	  segmentationViolation `addSignal`
	  softwareStop `addSignal`
	  softwareTermination `addSignal`
	  keyboardStop `addSignal`
	  backgroundRead `addSignal`
	  backgroundWrite `addSignal`
	  userDefinedSignal1 `addSignal`
	  userDefinedSignal2 `addSignal`
#if HAVE_SIGPOLL
	  pollableEvent `addSignal`
#endif
	  profilingTimerExpired `addSignal`
	  badSystemCall `addSignal`
	  breakpointTrap `addSignal`
	  urgentDataAvailable `addSignal`
	  virtualTimerExpired `addSignal`
	  cpuTimeLimitExceeded `addSignal`
	  fileSizeLimitExceeded `addSignal`
	  emptySignalSet

emptyset = internalAbort `deleteSignal`
	  realTimeAlarm `deleteSignal`
	  busError `deleteSignal`
	  processStatusChanged `deleteSignal`
	  continueProcess `deleteSignal`
	  floatingPointException `deleteSignal`
	  lostConnection `deleteSignal`
	  illegalInstruction `deleteSignal`
	  keyboardSignal `deleteSignal`
	  killProcess `deleteSignal`
	  openEndedPipe `deleteSignal`
	  keyboardTermination `deleteSignal`
	  segmentationViolation `deleteSignal`
	  softwareStop `deleteSignal`
	  softwareTermination `deleteSignal`
	  keyboardStop `deleteSignal`
	  backgroundRead `deleteSignal`
	  backgroundWrite `deleteSignal`
	  userDefinedSignal1 `deleteSignal`
	  userDefinedSignal2 `deleteSignal`
#if HAVE_SIGPOLL
	  pollableEvent `deleteSignal`
#endif
	  profilingTimerExpired `deleteSignal`
	  badSystemCall `deleteSignal`
	  breakpointTrap `deleteSignal`
	  urgentDataAvailable `deleteSignal`
	  virtualTimerExpired `deleteSignal`
	  cpuTimeLimitExceeded `deleteSignal`
	  fileSizeLimitExceeded `deleteSignal`
	  fullSignalSet
  
testMembers set = [
	  internalAbort `inSignalSet` set,
	  realTimeAlarm `inSignalSet` set,
	  busError `inSignalSet` set,
	  processStatusChanged `inSignalSet` set,
	  continueProcess `inSignalSet` set,
	  floatingPointException `inSignalSet` set,
	  lostConnection `inSignalSet` set,
	  illegalInstruction `inSignalSet` set,
	  keyboardSignal `inSignalSet` set,
	  killProcess `inSignalSet` set,
	  openEndedPipe `inSignalSet` set,
	  keyboardTermination `inSignalSet` set,
	  segmentationViolation `inSignalSet` set,
	  softwareStop `inSignalSet` set,
	  softwareTermination `inSignalSet` set,
	  keyboardStop `inSignalSet` set,
	  backgroundRead `inSignalSet` set,
	  backgroundWrite `inSignalSet` set,
	  userDefinedSignal1 `inSignalSet` set,
	  userDefinedSignal2 `inSignalSet` set,
#if HAVE_SIGPOLL
	  pollableEvent `inSignalSet` set,
#endif
	  profilingTimerExpired `inSignalSet` set,
	  badSystemCall `inSignalSet` set,
	  breakpointTrap `inSignalSet` set,
	  urgentDataAvailable `inSignalSet` set,
	  virtualTimerExpired `inSignalSet` set,
	  cpuTimeLimitExceeded `inSignalSet` set,
	  fileSizeLimitExceeded `inSignalSet` set
    ]
