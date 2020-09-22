
void pauseAndResume(bool assertNotPaused, volatile unsigned int * count);
unsigned long pauseAndResumeViaThread(volatile unsigned int * count);
void pauseAndUseRtsAPIAndResume
    ( HaskellObj haskellFn
    , HaskellObj haskellFnArgument
    , HaskellObj obj1
    , HaskellObj obj2
    , HsStablePtr stablePtrIO
    );
