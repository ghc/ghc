import Posix

main =
    putStr "Blocking real time alarms.\n" >>
    blockSignals (addSignal realTimeAlarm emptySignalSet) >>
    putStr "Scheduling an alarm in 2 seconds...\n" >>
    scheduleAlarm 2 >>
    putStr "Sleeping 5 seconds.\n" >>
    sleep 5 >>
    getPendingSignals >>= \ ints ->
    putStr "Checking pending interrupts for RealTimeAlarm\n" >>
    print (inSignalSet realTimeAlarm ints) >>
    putChar '\n'

