/* assemble global stiffness matrix */

#include "database"

extern int diagadr[];
extern float  stiff_vbm[];
extern void elstiff();

assemble_stiffness() {

  int element, nl,nr, dgrslist[6];
  int i,j,degree_i,degree_j,adr;
  float  ke[6][6];

  for (i=0; i<=diagadr[ndgrs]; i=i+1) stiff_vbm[i] = 0.0;

  for (element=1; element<=nelem; element=element+1) {

    nl = elem_info[element].nl;
    nr = elem_info[element].nr;
    dgrslist[0] = node_info[nl].u;
    dgrslist[1] = node_info[nl].v;
    dgrslist[2] = node_info[nl].theta;
    dgrslist[3] = node_info[nr].u;
    dgrslist[4] = node_info[nr].v;
    dgrslist[5] = node_info[nr].theta;

    elstiff(element,ke);

    for (i=0; i<6; i=i+1) {
      for (j=0; j<=i; j=j+1) {
        degree_i = dgrslist[i];
        degree_j = dgrslist[j];
        if ( (degree_i!=0) && (degree_j!=0) ) {
           if (degree_i >= degree_j) 
             adr = diagadr[degree_i] + degree_j - degree_i;
           if (degree_i<degree_j)
             adr = diagadr[degree_j] + degree_i - degree_j;

           stiff_vbm[adr] = stiff_vbm[adr] + ke[i][j];
        }
      }
    }

  }

}
