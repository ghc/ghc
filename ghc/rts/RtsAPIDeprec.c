/* ----------------------------------------------------------------------------
 * $Id: RtsAPIDeprec.c,v 1.1 2001/01/11 17:25:56 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2001
 *
 * RTS API functions that are deprecated
 *
 * --------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "Storage.h"
#include "Prelude.h"

HaskellObj
rts_mkAddr (HsAddr a)
{
  StgClosure *p = (StgClosure *)allocate(sizeofW(StgHeader)+1);
  p->header.info = Azh_con_info;
  p->payload[0]  = (StgClosure *)a;
  return p;
}

HsAddr
rts_getAddr (HaskellObj p)
{
  if ( p->header.info == Azh_con_info || 
       p->header.info == Azh_static_info ) {
  
    return (void *)(p->payload[0]);
  } else {
    barf("getAddr: not an Addr");
  }
}
