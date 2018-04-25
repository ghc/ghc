/* fem.c  ---- main program of 2d truss structral analysis */

#include <sys/types.h>
#include <sys/times.h>
#include <time.h>
#include <stdio.h>

#define MAXNODES  1000
#define MAXDEGREES  MAXNODES * 3
#define MAXELEMENTS  3000
#define MAXMATERIALS  10
#define MAXNODELOAD  500
#define MAXVOLUMESTIFF  90000

int nnode, nelem, nmats, nplds;
int ndgrs;
struct {
        float  x,y;
        int   bc;
        int   u,v,theta; } node_info[MAXNODES];
struct {
        int nl,nr,mat;  }  elem_info[MAXELEMENTS];
struct {
        float ea,ei; }     mat_info[MAXMATERIALS];
struct {
        int   to_point;
        float  px,py,m; }   load_info[MAXNODELOAD];
int diagadr[MAXDEGREES];
float  stiff_vbm[MAXVOLUMESTIFF];
float  load_vec[MAXDEGREES];
FILE *fp;

main() {

  struct tms buffer;
  float t1,t2,ut,st;

  t1 = clock();
  times(&buffer);
  ut = buffer.tms_utime;
  st = buffer.tms_stime;

  read_control_data();
  read_node_data();
  read_element_data();

  gen_fem_model_data();

  read_material_data();
  read_load_data();

  pre_assemble();
  assemble_stiffness();
  assemble_loadvec();
  displacement();
  forces();

  t2 = clock();

  times(&buffer);
  printf("User time = %f  System time = %f  ",
         (buffer.tms_utime-ut) / 60.0, (buffer.tms_stime -st) / 60.0);
  printf("Real = %f  \n", (t2-t1)/1.0e6);


}
