/*   Two dimensional bar element                                     */
/*                                                                   */
/*   bar_2d_stif computes the element stiffness matrix corresponding */
/*               to displacement vector [ul,vl,thetal,ur,vr,thetar]. */
/*                                                                   */
/*   bar_2d_force computes the element internal force caused by the  */
/*               nodal displacement at the two ends. Positive result */
/*               means the element is in tension, negative pressure. */


#include <math.h>
#include "database"

extern float load_vec[];

bar_2d_stif(ea,xl,yl,xr,yr,ke)

float ke[6][6],ea,xl,xr,yl,yr;

{
  int i,j;
  float dx,dy,l,c,s,al,b1,b2,b3;

  for (i=0; i<6; i=i+1) {
    for (j=0; j<6; j=j+1) {
      ke[i][j] = 0.0;
    }
  }

  dx = xr-xl;
  dy = yr-yl;
  l  = sqrt(dx*dx + dy*dy);
  c  = dx/l;
  s  = dy/l;
  al = ea/l;
  b1 = al*c*c;
  b2 = al*c*s;
  b3 = al*s*s;

  ke[0][0] = b1;
  ke[1][0] = b2;
  ke[1][1] = b3;
  ke[0][1] = b2;

  ke[3][3] = b1;
  ke[4][3] = b2;
  ke[4][4] = b3;
  ke[3][4] = b2;

  ke[0][3] = -b1;
  ke[1][3] = -b2;
  ke[1][4] = -b3;
  ke[0][4] = -b2;

  ke[3][0] = -b1;
  ke[4][0] = -b2;
  ke[4][1] = -b3;
  ke[3][1] = -b2;

}

void elstiff(element,ke)
int element;
float ke[6][6];

{
  int nl,nr,i;
  float  ea,xl,xr,yl,yr;
  nl = elem_info[element].nl;
  nr = elem_info[element].nr;
  i  = elem_info[element].mat;
  ea = mat_info[i].ea;
  xl = node_info[nl].x;
  yl = node_info[nl].y;
  xr = node_info[nr].x;
  yr = node_info[nr].y;

  bar_2d_stif(ea,xl,yl,xr,yr,ke);

}


float bar_2d_force(ea,xl,yl,xr,yr,ul,vl,ur,vr)

float ea,xl,yl,xr,yr,ul,vl,ur,vr;

{
  float dx,dy,l,c,s;

  dx = xr - xl;
  dy = yr - yl;
  l  = sqrt(dx*dx + dy*dy);
  c = dx/l;
  s = dy/l;

  return ea/l*((ur-ul)*c+(vr-vl)*s) ;

}

float  elforce(element)

int element;

{
  int nl,nr;
  float  ea,xl,xr,yl,yr,ul,vl,thetal,ur,vr,thetar;
  float  nuvw[3],force;

  nl = elem_info[element].nl;
  nr = elem_info[element].nr;
  ea = mat_info[elem_info[element].mat].ea;
  xl = node_info[nl].x;
  xr = node_info[nr].x;
  yl = node_info[nl].y;
  yr = node_info[nr].y;
  getnuvw(load_vec,nl,nuvw);
  ul = nuvw[0];
  vl = nuvw[1];
  getnuvw(load_vec,nr,nuvw);
  ur = nuvw[0];
  vr = nuvw[1];

  force = bar_2d_force(ea,xl,yl,xr,yr,ul,vl,ur,vr);

  return force;

}

