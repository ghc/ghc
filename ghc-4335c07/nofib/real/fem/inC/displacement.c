/* compute structural displacement and print them out */

#include <stdio.h>
#include "database"
extern FILE *fp;


extern int diagadr[];
extern float  stiff_vbm[], load_vec[];

getnuvw(uvw,node,nuvw)

int node;
float  uvw[],nuvw[];          

/* build up the displacement array of a node */
{ 
  int dgr;
  float  u,v,theta;

  dgr = node_info[node].u;
  if (dgr!=0) 
     u = uvw[dgr];
  else 
     u = 0.0;

  dgr = node_info[node].v;
  if (dgr!=0)
     v = uvw[dgr];
  else
     v = 0.0;

  dgr = node_info[node].theta;
  if (dgr!=0)
     theta = uvw[dgr];
  else
     theta = 0.0;

  nuvw[0] = u;
  nuvw[1] = v;
  nuvw[2] = theta;

}

displacement() {

  int node, bc;
  float  x,y,u,v,theta,nuvw[3];

  vblldecomp(ndgrs,diagadr,stiff_vbm);
  vbllsolution(ndgrs,diagadr,stiff_vbm,load_vec);

  printf("\n\nDISPLACEMENT OF THE STRUCTURE \n\n");
  printf(" Node      X             Y       BC        U             V");
  printf("           Theta\n");

  for (node=1; node<=nnode; node=node+1) {

    x = node_info[node].x;
    y = node_info[node].y;
    bc= node_info[node].bc;
    getnuvw(load_vec,node,nuvw);
    u = nuvw[0];
    v = nuvw[1];
    theta = nuvw[2];

    printf("%4d%14.6f%14.6f%4d%14.6f%14.6f%14.6f\n",node,x,y,bc,u,v,theta);
    fprintf(fp,"%14.6f   %14.6f\n",u,v);

  }

}
