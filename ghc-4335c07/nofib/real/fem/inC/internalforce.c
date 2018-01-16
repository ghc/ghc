#include <stdio.h>
#include <math.h>
#include "database"

extern float load_vec[];
extern float bar_2d_force();
extern float  elforce();
extern FILE *fp;

forces() {

  int element,nl,nr;

  printf("\n\n\nINTERNAL FORCES OF ELEMENT \n\n");
  printf("    Element   NodeL   NodeR          F \n");

  for (element=1; element<=nelem; element=element+1) {

    nl = elem_info[element].nl;
    nr = elem_info[element].nr;
    printf("%8d%10d%8d%  15.6f\n",element,nl,nr,elforce(element));
    fprintf(fp,"%16f\n",elforce(element));

  }

} 
