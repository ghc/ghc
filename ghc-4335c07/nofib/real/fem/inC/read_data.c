#include <stdio.h>
#include "database"
extern  FILE *fp;

read_control_data() {

  scanf("%d",&nnode);
  scanf("%d",&nelem);
  scanf("%d",&nmats);
  scanf("%d",&nplds);

  printf("\n\n\nCONTROL DATA :\n\n");
  printf("   Total number of nodes = %3d\n", nnode);
  printf("   Number of elements    = %3d\n", nelem);
  printf("   Total number of materials = %3d\n", nmats);
  printf("   Number of point loads = %3d\n", nplds);

}

read_node_data() {

  int i,j,m,n;
  float  r;

  printf("\n\n\nNODE INFORMATION:\n\n");
  ndgrs = 0 ;
  for (i=1; i<= nnode; i=i+1) {

    printf("  Node.no = %2d", i);
    scanf("%f",&r); node_info[i].x = r;
    printf("   x = %8.3f",r);     
    scanf("%f",&r); node_info[i].y = r;
    printf("   y = %8.3f",r);
    scanf("%d",&j); node_info[i].bc = j;
    printf("   bc = %3d\n",j);

    m = j / 100;
    n = j - m  * 100;
    if (m==1) {
              ndgrs = ndgrs + 1 ;
              node_info[i].u = ndgrs;
             }
    else 
              node_info[i].u = 0;

    m = n / 10;
    n = n - m  * 10;
    if (m==1) {
              ndgrs = ndgrs + 1 ;
              node_info[i].v = ndgrs;
             }
    else
              node_info[i].v = 0;

    if (n==1) {
              ndgrs = ndgrs + 1 ;
              node_info[i].theta = ndgrs;
             }
    else
              node_info[i].theta = 0;

  }

}

read_element_data() {

  int i,j;

  printf("\n\n\n ELEMENT DATA: \n\n");

  for (i=1; i<= nelem; i=i+1) {

           printf("  Element No.=%2d",i);
           scanf("%d",&j); elem_info[i].nl = j;
           printf("  Node.L =%2d",j);
           scanf("%d",&j); elem_info[i].nr = j;
           printf("  Node.R =%2d",j);
           scanf("%d",&j); elem_info[i].mat = j;
           printf("  Material No=%2d\n",j);

  }
}

gen_fem_model_data()
{
 int i,j,k;

 fp = fopen("FEM_MODEL_DATA","w");

 fprintf(fp," 1  1  %d\n",nnode);
 for (i=1; i<= nnode; i=i+1) {
    fprintf(fp,"%f  %f %d\n",node_info[i].x,node_info[i].y,node_info[i].bc);
 }

 fprintf(fp,"%d %d\n",2,nelem);
 for (i=1; i<= nelem; i=i+1) {
    fprintf(fp,"%d %d \n",elem_info[i].nl,elem_info[i].nr);
 }

}

read_material_data() {

  int i;
  float  r;

  printf("\n\n\n MATERIAL INFORMATION :\n\n");
  for (i=1; i<=nmats; i=i+1) { 
           printf("  Material No.=%2d",i);
           scanf("%f", &r); mat_info[i].ea = r;
           printf("   EA = %12.4f",r);
           scanf("%f", &r); mat_info[i].ei = r;
           printf("   EI = %12.4f\n",r);
  }
}

read_load_data() {

  int i,j;
  float  r;

  printf("\n\n\n POINT LOADS DATA:\n\n");
  for (i=1; i<=nplds; i=i+1) {
           
           scanf("%d", &j); load_info[i].to_point = j;
           printf("  To_point%2d",j);
           scanf("%f", &r); load_info[i].px = r;
           printf("   Px =  %9.3f",r);
           scanf("%f", &r); load_info[i].py = r;
           printf("   Py =  %9.3f",r);
           scanf("%f", &r); load_info[i].m  = r;
           printf("   M =  %9.3f\n",r);

  }
}
