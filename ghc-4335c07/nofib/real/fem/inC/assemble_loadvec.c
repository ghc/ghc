/* assemble global load vector */

#include "database"

extern float  load_vec[];

assemble_loadvec() {

  int i, dgr, to_point;
  float  px,py,m;

  for (i=1; i<= ndgrs; i=i+1) load_vec[i] = 0.0;

  for (i=1; i<= nplds; i=i+1) {
 
    to_point = load_info[i].to_point;
    px       = load_info[i].px;
    py       = load_info[i].py;
    m        = load_info[i].m;
   
    dgr = node_info[to_point].u;
    if (dgr!=0) load_vec[dgr] = load_vec[dgr] + px;

    dgr = node_info[to_point].v;
    if (dgr!=0) load_vec[dgr] = load_vec[dgr] + py;

    dgr = node_info[to_point].theta;
    if (dgr!=0) load_vec[dgr] = load_vec[dgr] + m;

  }

}
