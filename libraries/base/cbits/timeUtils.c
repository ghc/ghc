/* 
 * (c) The University of Glasgow 2002
 *
 * Time Runtime Support
 */
#include "HsBase.h"

#if defined(mingw32_HOST_OS) /* to the end */

HsAddr __hscore_timezone( void )
{ return (HsAddr)&_timezone; }

HsAddr __hscore_tzname( void )
{ return (HsAddr)_tzname; }
#endif
