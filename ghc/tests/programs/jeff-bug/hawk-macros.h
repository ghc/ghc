#define step4(w,x,y,z) loop(bundle4(w,x,y,z)) $ \ ~(w,x,y,z) -> do
#define step3(x,y,z) loop(bundle3(x,y,z)) $ \ ~(x,y,z) -> do
#define step2(x,y) loop(bundle2 (x,y)) $ \ ~(x,y) -> do {- stp2(x,y) -}
#define step1(x) loop(x) $ \ ~x -> do
#define step loop(lift0 ()) $ \ ~() -> do
#define _probeST () <- stpProbe
