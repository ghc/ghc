import Posix

main =
    installHandler realTimeAlarm (Catch alarmclock) Nothing >>
    putStr "Scheduling an alarm in 5 seconds...\n" >>
    scheduleAlarm 5 >>
    putStr "Sleeping one minute.\n" >>
    sleep 60 >>
    putStr "How did I get here?\n"

alarmclock =
    putStr "The alarm went off.\n"
