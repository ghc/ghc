/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

extern void initProfTimer      ( void );
extern void handleProfTick     ( void );

extern void stopProfTimer      ( void );
extern void startProfTimer     ( void );
extern void stopHeapProfTimer  ( void );
extern void startHeapProfTimer ( void );

extern rtsBool performHeapProfile;
