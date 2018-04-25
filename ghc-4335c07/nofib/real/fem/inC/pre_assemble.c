/* generates the diagonal element address array of global stiffness matrix
   with variable bandwidth data structure                                  */

#include "database"

extern int diagadr[];

pre_assemble() {

  int element, nl, nr;
  int dgrslist[6], nzds, smstd;
  int i,dgr;

  for (i=0; i<=ndgrs; i=i+1) diagadr[i] = 0;

  /* generate semibandwidth array of global stiffness matrix */
  for (element=1; element<=nelem; element=element+1) {
 
    nl = elem_info[element].nl;
    nr = elem_info[element].nr;
    
    /* get the valid degree list of this element, nzds is the number of
       valid degrees(i.e. non zero degrees) of this element */
    i = 0;
    if (node_info[nl].u != 0) {dgrslist[i]=node_info[nl].u; i=i+1;};
    if (node_info[nl].v != 0) {dgrslist[i]=node_info[nl].v; i=i+1;};
    if (node_info[nl].theta != 0) {dgrslist[i]=node_info[nl].theta; i=i+1;};
    if (node_info[nr].u != 0) {dgrslist[i]=node_info[nr].u; i=i+1;};
    if (node_info[nr].v != 0) {dgrslist[i]=node_info[nr].v; i=i+1;};
    if (node_info[nr].theta != 0) {dgrslist[i]=node_info[nr].theta; i=i+1;};
    nzds = i;


    /*smstd ---- smallest degree number of this element */    
    smstd = dgrslist[0];
    for (i=1; i<nzds; i=i+1) {
       if (dgrslist[i]<smstd) smstd = dgrslist[i];
    }

    for (i=0; i<nzds; i=i+1) {
      dgr = dgrslist[i];
      if ((dgr - smstd + 1) > diagadr[dgr])
         diagadr[dgr] = dgr - smstd + 1;
    }
  
  }

  /* generate diagonal element address array of global stiffness matrix */
  for (i=1; i<=ndgrs; i=i+1) {
    diagadr[i] = diagadr[i] + diagadr[i-1];
  }

}
