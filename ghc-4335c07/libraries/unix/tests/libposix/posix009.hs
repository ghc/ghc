import System.Posix.Signals
import System.Posix.Unistd

main = do
    putStrLn "Blocking real time alarms."
    blockSignals (addSignal realTimeAlarm reservedSignals)
    putStrLn "Scheduling an alarm in 2 seconds..."
    scheduleAlarm 2
    putStrLn "Sleeping 5 seconds."
    sleep 5
    putStrLn "Woken up"
    ints <- getPendingSignals
    putStrLn "Checking pending interrupts for RealTimeAlarm"
    print (inSignalSet realTimeAlarm ints)

