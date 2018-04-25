/*
  C Simple
  
  Andy Shaw and Kofi Fynn.
  Wed Feb  2 14:59:18 EST 1994

  This is a version of EK's Simple which I am writing as a preliminary to getting
  it running on the CM-5 using the work-stealing RTS.  It will be based on the
  lastest Id version of ek-simple which RPaul has written which is running fairly
  well on Gita and Monsoon.
  */
  
#include <math.h>
#include <stdio.h>

/* Problem Specification Parameters */
int niterations, problem_size;

/*
  Constants
  */
double pi = 3.1415926535898;
double constant_heat_source = 0.0;
double deltat_maximum = 0.01;
double specific_heat = 0.1;
double p_coeffs[3][3] = {0.0, 0.0, 0.0,
			   0.0, 0.06698, 0.0,
			   0.0, 0.0, 0.0,
			 };
double e_coeffs[3][3] = {0.0, 0.1, 0.0,
			   0.0, 0.0, 0.0,
			   0.0, 0.0, 0.0,
			 };

/* MATRIX FOR THE PRESSURE POLYNOMIAL */
double *p_poly[4][5] = {&p_coeffs[0][0], &p_coeffs[0][0], &p_coeffs[0][0], &p_coeffs[0][0],
			  &p_coeffs[0][0], &p_coeffs[0][0], &p_coeffs[0][0], &p_coeffs[0][0],
			  &p_coeffs[0][0], &p_coeffs[0][0], &p_coeffs[0][0], &p_coeffs[0][0],
			  &p_coeffs[0][0], &p_coeffs[0][0], &p_coeffs[0][0], &p_coeffs[0][0],
			  &p_coeffs[0][0], &p_coeffs[0][0], &p_coeffs[0][0], &p_coeffs[0][0]};

/*MATRIX FOR THE ENERGY POLYNOMIAL */
double *e_poly[4][5] = {&e_coeffs[0][0], &e_coeffs[0][0], &e_coeffs[0][0], &e_coeffs[0][0],
			  &e_coeffs[0][0], &e_coeffs[0][0], &e_coeffs[0][0], &e_coeffs[0][0],
			  &e_coeffs[0][0], &e_coeffs[0][0], &e_coeffs[0][0], &e_coeffs[0][0],
			  &e_coeffs[0][0], &e_coeffs[0][0], &e_coeffs[0][0], &e_coeffs[0][0],
			  &e_coeffs[0][0], &e_coeffs[0][0], &e_coeffs[0][0], &e_coeffs[0][0]};

/* RHO MATRIX FOR POLYNOMIAL CALCULATION */
double rho_table[] = {0.0 ,0.0, 1.0, 100.0};

/* THETA MATRIX FOR POLYNOMIAL CALCULATION */
double theta_table[] = {0.0 ,0.0, 3.0, 300.0, 3000.0};

/* MATRICES FOR BOUNDARY PRESSURE CALCULATION */
double pbb[] = {0.0 ,0.0, 6.0, 0.0, 0.0};
double pb[]  = {0.0 ,1.0, 0.0, 0.0, 1.0};

/*MATRIX FOR BOUNDARY VISCOSITY CALCULATION */
double *qb = pb;

/* Artificial Viscosity */
double c0 = 1.22474487;
double c1 = 0.50;

/*
  Simple State
  */
#define MAXSIZE 150


double vx[MAXSIZE][MAXSIZE], vy[MAXSIZE][MAXSIZE];
double new_vx[MAXSIZE][MAXSIZE], new_vy[MAXSIZE][MAXSIZE];
double xx[MAXSIZE][MAXSIZE], xy[MAXSIZE][MAXSIZE];
double new_xx[MAXSIZE][MAXSIZE], new_xy[MAXSIZE][MAXSIZE];
double alpha[MAXSIZE][MAXSIZE], s[MAXSIZE][MAXSIZE];
double new_alpha[MAXSIZE][MAXSIZE], new_s[MAXSIZE][MAXSIZE];
double rho[MAXSIZE][MAXSIZE],new_rho[MAXSIZE][MAXSIZE];
double p[MAXSIZE][MAXSIZE], q[MAXSIZE][MAXSIZE];
double new_p[MAXSIZE][MAXSIZE], new_q[MAXSIZE][MAXSIZE];
double d[MAXSIZE][MAXSIZE],new_d[MAXSIZE][MAXSIZE];
double epsilon[MAXSIZE][MAXSIZE],new_epsilon[MAXSIZE][MAXSIZE];
double theta[MAXSIZE][MAXSIZE],theta_hat[MAXSIZE][MAXSIZE];
double new_theta[MAXSIZE][MAXSIZE];
double Gamma_k[MAXSIZE][MAXSIZE],Gamma_l[MAXSIZE][MAXSIZE];
double deltat,new_deltat;
double c,new_c;

/*
  Print Functions
  and Debug Functions
  */
int print_2d_matrix(double matrix[MAXSIZE][MAXSIZE], int node_or_zone)
{
  int i, j;
  int llimit = node_or_zone ? 1: 0;
  int ulimit = node_or_zone ? problem_size+1: problem_size+1;

  printf("\n\n");
  printf("[");
  for (j=llimit;j<ulimit;j++)
    {
      printf("[");
      for (i=llimit;i<ulimit;i++)
	{
	  printf(" %lf", matrix[i][j]);
	}
      printf("]\n ");
    }
  printf("]\n");
}

#define PRINT_NODE_MATRIX(matrix) print_2d_matrix(matrix, 0)
#define PRINT_ZONE_MATRIX(matrix) print_2d_matrix(matrix, 1)

/* This functions computes the line integral of p with
   respect to z around the node [nodex][nodey] */
double line_integral (double p[MAXSIZE][MAXSIZE], double z[MAXSIZE][MAXSIZE],
		      int nodex, int nodey)
{
  double integral =
    /* Northwest */
    p[nodex][nodey] * (z[nodex-1][nodey] - z[nodex][nodey-1]) +
      /* Southwest */
      p[nodex][nodey+1] * (z[nodex][nodey+1] - z[nodex-1][nodey]) +
	/* Southeast */
	p[nodex+1][nodey+1] * (z[nodex+1][nodey] - z[nodex][nodey+1]) +
	  /* Northeast */
	  p[nodex+1][nodey] * (z[nodex][nodey-1] - z[nodex+1][nodey]);
  return integral ;			       
};

/*Computes the mass o fth eregion around a node */
double regional_mass (int nodex, int nodey)
{
  double mass =
    0.5 * 
      /* Northwest */    
      (rho[nodex][nodey] * alpha[nodex][nodey] +
	/* Northeast */
	rho[nodex+1][nodey] * alpha[nodex+1][nodey] +
	  /* Southeast */
	  rho[nodex+1][nodey+1] * alpha[nodex+1][nodey+1] +
	    /* Southwest */
	    rho[nodex][nodey+1] * alpha[nodex][nodey+1]) ;

  return mass;
}


/* This function computes the position vectors of the interior, in the main loop of
   Simple.  It is simply an adjustment of the old position modified by the product of the
   duration of the time step and the velocity of the node at the time step.
   */
int compute_position_vectors_interior
  (double new_r[MAXSIZE][MAXSIZE], double new_z[MAXSIZE][MAXSIZE],
   double r[MAXSIZE][MAXSIZE], double z[MAXSIZE][MAXSIZE],
   double deltat,
   double new_u[MAXSIZE][MAXSIZE], double new_w[MAXSIZE][MAXSIZE])
{
  int i, j;
  
  /* first, fill in the intererior of the position vector arrays*/
  for (i=1;i<problem_size;i++)
    for (j=1;j<problem_size;j++)
      {
	new_r[i][j] = r[i][j] + deltat * new_u[i][j];
	new_z[i][j] = z[i][j] + deltat * new_w[i][j];
      }
}

/* This function computes the position vectors of the boundaries, and it assumes that
   the interior has already been computed.

   The boundaries are computed by merely reflecting the positions of the interior, with respect
   a reflection vector.  This is kind of tricky, as the corners must be handled specially, and
   the ends of the sides must also be handled especially.
   */
int reflect_boundary(double r[MAXSIZE][MAXSIZE], double z[MAXSIZE][MAXSIZE],
  int xlo, int xhi, int ylo, int yhi, int xdeltax, int xdeltay,
  int ydeltax, int ydeltay, int adeltax, int adeltay)
{
  int i, j;
  
  for (i=xlo;i<xhi;i++)
    for (j=ylo;j<yhi;j++)
      {
	double rx = r[i+xdeltax][j+xdeltay];
	double zx = z[i+xdeltax][j+xdeltay];
	double ry = r[i+ydeltax][j+ydeltay];
	double zy = z[i+ydeltax][j+ydeltay];
	double ra = r[i+adeltax][j+adeltay];
	double za = z[i+adeltax][j+adeltay];
	double rax = ra - rx;
	double zax = za - zx;
	double ryx = ry - rx;
	double zyx = zy - zx;
	double omega = 2.0 * (rax * ryx + zax * zyx) / (ryx*ryx + zyx*zyx);
	double rb  =  rx - rax + omega * ryx;
	double zb  =  zx - zax + omega * zyx;

	r[i][j] = rb;
	z[i][j] = zb;
      }
}

int compute_position_vectors_boundaries(double xx[MAXSIZE][MAXSIZE],
					double xy[MAXSIZE][MAXSIZE])
{
  /* NORTH */
  reflect_boundary(xx, xy, 1, problem_size-1,
		   0, 1, 0, 1, 1, 1, 0, 2);
  /* SOUTH */
  reflect_boundary(xx, xy, 1, problem_size-1, problem_size, problem_size+1,
		   0, -1, 1, -1, 0, -2);
  /* WEST */
  reflect_boundary(xx, xy, 0, 1, 1, problem_size-1,
		   1, 0, 1, 1, 2, 0);
  /* EAST */
  reflect_boundary(xx, xy, problem_size, problem_size+1, 1, problem_size-1,
		   -1, 0, -1, 1, -2, 0);

  /* WEST of NE CORNER */
  reflect_boundary(xx, xy, problem_size-1, problem_size,
		   0, 1, 0, 1, -1, 1, 0, 2);
  /* WEST of SE CORNER */
  reflect_boundary(xx, xy, problem_size-1, problem_size, problem_size, problem_size+1,
		   0, -1, -1, -1, 0, -2);
  /* NORTH of SW CORNER */
  reflect_boundary(xx, xy, 0, 1, problem_size-1, problem_size,
		   1, 0, 1, -1, 2, 0);
  /* NORTH of SE CORNER */
  reflect_boundary(xx, xy, problem_size, problem_size+1, problem_size-1, problem_size,
		   -1, 0, -1, -1, -2, 0);

  /* NW CORNER */
  reflect_boundary(xx, xy, 0, 1, 0, 1, 1, 1, 0, 1, 0, 2);
  /* NE CORNER */
  reflect_boundary(xx, xy, problem_size, problem_size+1, 0, 1, -1, 1, -1, 0, -2, 0);
  /* SW CORNER */
  reflect_boundary(xx, xy, 0, 1, problem_size, problem_size+1, 1, -1, 1, 0, 2, 0);
  /* SE CORNER */
  reflect_boundary(xx, xy, problem_size, problem_size+1, problem_size, problem_size+1,
		   -1, -1, -1, 0, -2, 0);
}


/* This function calculates the area and volume of a zone.
   A zone is represented by the coordinates of its southeast corner. */

int zone_area_vol(double *zone_area, double *zone_volume, double r[MAXSIZE][MAXSIZE],
		  double z[MAXSIZE][MAXSIZE], int zone_x, int zone_y)
{
  double Rsw = r[zone_x-1][zone_y];
  double Zsw = z[zone_x-1][zone_y];
  double Rse = r[zone_x][zone_y];
  double Zse = z[zone_x][zone_y];
  double Rne = r[zone_x][zone_y-1];
  double Zne = z[zone_x][zone_y-1];
  double Rnw = r[zone_x-1][zone_y-1];
  double Znw = z[zone_x-1][zone_y-1];

  double area1 = Rse*(Zne - Zsw) + Rsw*(Zse-Zne) + Rne*(Zsw-Zse);
  double area2  = Rnw * (Zsw-Zne) + Rne * (Znw-Zsw) + Rsw*(Zne-Znw);
  double average1  =  (Rsw+Rse+Rne);
  double volume1  =  (area1 * average1)/3.0;
  double average2  =  (Rsw+Rne+Rnw);
  double volume2  =  (area2 * average2)/3.0;
  
  *zone_area = (area1 + area2)/2.0; 
  *zone_volume = (volume1 + volume2) * pi;;
};

/* The velocities for the interior nodes are calculated by multiplying 
   their accelerations to the time step deltat. Since the boundary velocities are 
   not needed, we do not calculate them. */

int make_velocity (double new_u[MAXSIZE][MAXSIZE], double new_w[MAXSIZE][MAXSIZE],
		   double u[MAXSIZE][MAXSIZE], double w[MAXSIZE][MAXSIZE],
		   double r[MAXSIZE][MAXSIZE], double z[MAXSIZE][MAXSIZE],
		   double p[MAXSIZE][MAXSIZE], double q[MAXSIZE][MAXSIZE],
		   double deltat)
{
  int i, j;
  
  for (i=1;i<problem_size;i++)
    for (j=1;j<problem_size;j++)
      {
	double d;
	double n1, n2;
	double u_dot, w_dot;

	d = regional_mass(i, j);
	n1 = - (line_integral(p, z, i, j)) - (line_integral(q, z, i, j));
	n2 = line_integral(p, r, i, j) + line_integral(q, r, i, j);
	u_dot = n1 / d;  /*acceleration in the u direction */
	w_dot = n2 / d;   /*acceleration in the w direction */
	new_u[i][j] = u[i][j] + deltat * u_dot;
	new_w[i][j] = w[i][j] + deltat * w_dot;
      }
};


int compute_position_vectors
  (double new_r[MAXSIZE][MAXSIZE], double new_z[MAXSIZE][MAXSIZE],
   double r[MAXSIZE][MAXSIZE], double z[MAXSIZE][MAXSIZE],
   double deltat, double u[MAXSIZE][MAXSIZE], double w[MAXSIZE][MAXSIZE])
{
  compute_position_vectors_interior(new_r, new_z, r, z, deltat, u, w);
  compute_position_vectors_boundaries(new_r, new_z);
}


/* Computes the area volume and density of the interior zones. */
int compute_alpha_s_rho_vectors_interior
(double new_alpha[MAXSIZE][MAXSIZE],double new_s[MAXSIZE][MAXSIZE],
 double new_r[MAXSIZE][MAXSIZE],double new_z[MAXSIZE][MAXSIZE],
 double new_rho[MAXSIZE][MAXSIZE],double rho[MAXSIZE][MAXSIZE],
 double s[MAXSIZE][MAXSIZE])
{
  int i, j;
  
  for (i=2;i<problem_size;i++)
    for (j=2;j<problem_size;j++){
      zone_area_vol(&new_alpha[i][j], &new_s[i][j], new_r, new_z, i, j);
      new_rho[i][j]=rho[i][j]*s[i][j]/new_s[i][j];
      }
}

/* This function computes the boundary area, volume and density by reflecting the adjoining zone.*/
int compute_alpha_s_rho_vectors_boundaries
(double new_alpha[MAXSIZE][MAXSIZE],double new_s[MAXSIZE][MAXSIZE],
 double new_rho[MAXSIZE][MAXSIZE])
{
  int i;
  
  /* Reflect South Border */
  for (i=2;i<problem_size;i++)
    {
      new_alpha[i][problem_size] = new_alpha[i][problem_size-1];
      new_s[i][problem_size]     = new_s[i][problem_size-1];
      new_rho[i][problem_size]     = new_rho[i][problem_size-1];
     
    }
  /* Reflect West Border */
  for (i=2;i<problem_size+1;i++)
    {
      new_alpha[1][i] = new_alpha[2][i];
      new_s[1][i]     = new_s[2][i];
      new_rho[1][i]     = new_rho[2][i];
    }
  /* Reflect East Border */
  for (i=2;i<problem_size+1;i++)
    {
      new_alpha[problem_size][i] = new_alpha[problem_size-1][i];
      new_s[problem_size][i]     = new_s[problem_size-1][i];      
      new_rho[problem_size][i]     = new_rho[problem_size-1][i];
    }
  /* Reflect North Border */
  for (i=1;i<problem_size+1;i++)
    {
      new_alpha[i][1] = new_alpha[i][2];
      new_s[i][1]     = new_s[i][2];
      new_rho[i][1]     = new_rho[i][2];
    }
}


/* This function computes the area,density and volume of each zone. */
int make_area_density_volume
(double rho[MAXSIZE][MAXSIZE],double s[MAXSIZE][MAXSIZE],
 double new_r[MAXSIZE][MAXSIZE],double new_z[MAXSIZE][MAXSIZE],
 double new_alpha[MAXSIZE][MAXSIZE],double new_rho[MAXSIZE][MAXSIZE],
 double new_s[MAXSIZE][MAXSIZE])
{
  compute_alpha_s_rho_vectors_interior(new_alpha,new_s,new_r,new_z,new_rho,rho,s);
  compute_alpha_s_rho_vectors_boundaries(new_alpha,new_s,new_rho);
};

/* This is the function mean_k_difference in the SIMPLE report.*/
double upper_del(double f[MAXSIZE][MAXSIZE],int zone_x,int zone_y)
{
  double u_del = 0.5 * ((f[zone_x][zone_y]-f[zone_x][zone_y-1]) + 
			(f[zone_x-1][zone_y]-f[zone_x-1][zone_y-1]));
  return u_del;
}

/* This is the function mean_l_difference in the SIMPLE report.*/
double lower_del(double f[MAXSIZE][MAXSIZE],int zone_x,int zone_y)
{
  double l_del = 0.5 * ((f[zone_x][zone_y]-f[zone_x-1][zone_y]) + 
			(f[zone_x][zone_y-1]-f[zone_x-1][zone_y-1]));
  return l_del;
}


/* Computes the viscosity of interior regions. */
int compute_viscosity_interior
(double p[MAXSIZE][MAXSIZE],double new_q[MAXSIZE][MAXSIZE],
 double d[MAXSIZE][MAXSIZE],double new_u[MAXSIZE][MAXSIZE],
 double new_w[MAXSIZE][MAXSIZE], double new_r[MAXSIZE][MAXSIZE],
 double new_z[MAXSIZE][MAXSIZE],double new_alpha[MAXSIZE][MAXSIZE],
 double new_rho[MAXSIZE][MAXSIZE])
{
  int i,j;
  double xi,eta,upper_disc,lower_disc,upper_ubar;
  double lower_ubar,speed_of_sound,ubar,length;
  double gamma = 1.6;
  double c0 = 1.22474487;
  double c1 = 0.50;
  for (i=2;i<problem_size;i++)
    for (j=2;j<problem_size;j++)
      {
	double u_r_del = upper_del(new_r,i,j);
	double l_r_del = lower_del(new_r,i,j);
	double u_z_del = upper_del(new_z,i,j);
	double l_z_del = lower_del(new_z,i,j);
	xi = u_r_del*u_r_del + u_z_del*u_z_del;
	eta = l_r_del*l_r_del + l_z_del*l_z_del;
	upper_disc = u_r_del * lower_del(new_w,i,j) - 
	  u_z_del*lower_del(new_u,i,j);
	lower_disc = upper_del(new_u,i,j) * l_z_del - 
	  upper_del(new_w,i,j)*l_r_del;
	if (upper_disc < 0.0) 
	  upper_ubar = upper_disc*upper_disc/xi;
	else upper_ubar=0.0;
	if (lower_disc<0.0) 
	  lower_ubar = lower_disc*lower_disc/eta;
	else lower_ubar=0.0;
      
	speed_of_sound=gamma * p[i][j] / new_rho[i][j];
	ubar=upper_ubar+lower_ubar;
	new_q[i][j] = new_rho[i][j]* ((c0*c0)/4.0)*ubar +
	  (c1/2.0) * speed_of_sound * sqrt(ubar);

	length=sqrt(u_r_del*u_r_del + l_r_del*l_r_del);
	d[i][j]=0.5 * new_alpha[i][j]/( speed_of_sound * length);
      }
}

/* Computes the viscosity of boundary regione. The indexes of qb[]
   are got from the matirix nbc[]. South has value 2, North, East and West
   have value 1 1 and all the corners are have value 4.*/
int compute_viscosity_boundaries
(double new_q[MAXSIZE][MAXSIZE],double d[MAXSIZE][MAXSIZE])
{
  int i;
  
   /* Compute North */
  for(i=2;i<problem_size;i++){
    new_q[i][1] = qb[1] * new_q[i][2];
    d[i][1]=0.0;}
  /* Compute South */
  for(i=2;i<problem_size;i++){
    new_q[i][problem_size] = qb[2] * new_q[i][problem_size-1];
    d[i][problem_size]=0.0;}
  /* Compute West */
  for(i=2;i<problem_size;i++){
    new_q[1][i] = qb[1] * new_q[2][i];
    d[1][i]=0.0;}
  /* Compute East */
  for(i=2;i<problem_size;i++){
    new_q[problem_size][i] = qb[1] * new_q[problem_size-1][i];
    d[problem_size][i] = 0.0;}
  /* Compute Northwest */
  new_q[1][1] = qb[4] * new_q[1][2];
  d[1][1]=0.0;
  /* Compute Northeast */
  new_q[problem_size][1] = qb[4] * new_q[problem_size][2];
  d[problem_size][1]=0.0;
  /* Compute Southwest */
  new_q[1][problem_size] = qb[4] * new_q[2][problem_size];
  d[1][problem_size]=0.0;
  /* Compute Southeast */
  new_q[problem_size][problem_size] = qb[4] * new_q[problem_size-1][problem_size];
  d[problem_size][problem_size] = 0.0;
}


int make_viscosity(double p[MAXSIZE][MAXSIZE],double new_q[MAXSIZE][MAXSIZE],
		   double d[MAXSIZE][MAXSIZE],
		   double new_u[MAXSIZE][MAXSIZE],double new_w[MAXSIZE][MAXSIZE],
		   double new_r[MAXSIZE][MAXSIZE],double new_z[MAXSIZE][MAXSIZE],
		   double new_alpha[MAXSIZE][MAXSIZE],double new_rho[MAXSIZE][MAXSIZE])
{
  compute_viscosity_interior(p,new_q,d,new_u,new_w,new_r,new_z,new_alpha,new_rho);
  compute_viscosity_boundaries(new_q,d);
};

int make_temperature(double p[MAXSIZE][MAXSIZE], double epsilon[MAXSIZE][MAXSIZE], 
		     double rho[MAXSIZE][MAXSIZE],double theta[MAXSIZE][MAXSIZE],
		     double theta_hat[MAXSIZE][MAXSIZE],
		     double new_rho[MAXSIZE][MAXSIZE],double new_q[MAXSIZE][MAXSIZE])
{
  compute_temperature_interior(p,epsilon,rho,theta,theta_hat,new_rho,new_q);
  compute_temperature_boundaries(theta_hat);

};


double newton_raphson
(double f(double,double,double),double const1,double const2,double x)
{
  double fxdx,denom;
  double  dx = 0.000001;
  double  tiny = 0.000001;
  double fx = f(const1,const2,x);
  while (fx > tiny)
    { fxdx = f(const1,const2,x+dx);
      denom=fxdx-fx;
      if (denom < tiny)
	fx = tiny;
      else
	{
	  x=x - (fx*dx/denom);
	  fx=fxdx;
	}
    }
  return x;
}

inline double power(double i, int j)
{
  if (j==0)
    return 1.0;
  else if (j==1)
    return i;
  else if (j==2)
    return i*i;

  printf("Power called with j>2, exiting ...\n");
  exit(1);
}

/* Function for searching table in polynomial calculation.
   I made a change here so that it looks like
   the one in the id version. I am still not sure though.Kofi Fynn.*/
inline int table_search(int table_size, double table[], double value)
{
  int i;
  
  if (value > table[table_size])
    return table_size+1;        
  else if (value <= table[1])     
    return 1;
  else
    for (i=table_size;i>=1;i--)
      {
	if (value > table[i-1])
	  return i;
      }
  printf("Error in table search ... off the end of the table ...\n");
  exit(1);
}

/* This function  yields and sums up thecoefficients of the polynomials
   used in the energy and pressure calculations.*/
inline double polynomial
(double *G[4][5], int degree, double rho_table[], double theta_table[],
 double rho_value, double theta_value)
{
  int rho_index;
  int theta_index;
  double *A;
  double A_sum = 0.0;
  int i,j;
  
  rho_index = table_search(3, rho_table, rho_value);
  theta_index = table_search(4, theta_table, theta_value);
  A = G[rho_index-1][theta_index-1];
  for (i=0;i<=degree;i++)
    for (j=0;j<=degree;j++)
      {
	double A_val;
	
        A_val = *(A+3*i+j);
	A_sum += (A_val * power(rho_value, i) * power(theta_value, j));
      }
  return(A_sum);
}


double zonal_energy(double rho_value, double theta_value)
{
  /*
  printf("Zonal Energy %lf %lf\n", rho_value, theta_value);
  */

  return(polynomial(e_poly, 2, rho_table, theta_table, rho_value, theta_value));
}


double energy_equation(double epsilon_kl,double rho_kl,double theta_kl)
{ 
  double energy_equation_ans = epsilon_kl- zonal_energy(rho_kl,theta_kl);
  return energy_equation_ans;
}

double revised_temperature(double f(double,double,double), double epsilon_kl, double rho_kl, 
			   double theta_kl)
{
  double theta1= newton_raphson(f,epsilon_kl,rho_kl,theta_kl);
  return theta1;
}


double zonal_pressure(double rho_value, double theta_value)
{
  /*
  printf("Zonal Pressure %lf %lf\n", rho_value, theta_value);
  */
  return(polynomial(p_poly, 2, rho_table, theta_table, rho_value, theta_value));
  
}


/* Calculates the interior temperature */
int compute_temperature_interior
(double p[MAXSIZE][MAXSIZE], double epsilon[MAXSIZE][MAXSIZE], 
 double rho[MAXSIZE][MAXSIZE], double theta[MAXSIZE][MAXSIZE],
 double theta_hat[MAXSIZE][MAXSIZE],
 double new_rho[MAXSIZE][MAXSIZE],double new_q[MAXSIZE][MAXSIZE])
{
  int i,j;
  double qkl,rho_kl,new_rho_kl,tau_kl;
  double epsilon_0,p_0,theta_0;
  double epsilon_1,p_1,theta_1,epsilon_2;
  
  for (i=2;i<problem_size;i++)
    for (j=2;j<problem_size;j++){
      qkl=new_q[i][j];
      rho_kl=rho[i][j];
      new_rho_kl=new_rho[i][j];
      tau_kl=(1.0/new_rho_kl) - (1.0/rho_kl);
      epsilon_0=epsilon[i][j];
      p_0=p[i][j];
      theta_0=theta[i][j];
      epsilon_1=epsilon_0-(p_0+qkl)*tau_kl;
      theta_1=revised_temperature(energy_equation,epsilon_1,rho_kl,theta_0);
      p_1=zonal_pressure(rho_kl,theta_1);
      epsilon_2=epsilon_0-(p_1+qkl)*tau_kl;
      theta_hat[i][j]=revised_temperature(energy_equation,epsilon_2,rho_kl,theta_1);
    }      
}  

/* Calculates the temperature at the border.*/
int compute_temperature_boundaries(double theta_hat[MAXSIZE][MAXSIZE])
{
  int i;
  
  /* Compute North */
  for(i=2;i<problem_size;i++)
    theta_hat[i][1]=constant_heat_source;
  /* Compute South */
  for(i=2;i<problem_size;i++)
    theta_hat[i][problem_size] = constant_heat_source;
  /* Compute West */
  for(i=2;i<problem_size;i++)
    theta_hat[1][i] = constant_heat_source;
  /* Compute East */
  for(i=2;i<problem_size;i++)
    theta_hat[problem_size][i] = constant_heat_source; 
}

/* int make_energy()
   {
   };*/

int make_cc_interior
(double new_alpha[MAXSIZE][MAXSIZE], double theta_hat[MAXSIZE][MAXSIZE],
 double cc[MAXSIZE][MAXSIZE])
{
  int i,j;
  for (i=2;i<problem_size;i++)
    for (j=2;j<problem_size;j++){
      cc[i][j]=0.0001*pow(theta_hat[i][j],2.5) / new_alpha[i][j];
    }
}

int make_cc_boundaries(double cc[MAXSIZE][MAXSIZE])
{
  int i;
  /* Reflect South Border */
  for (i=2;i<problem_size;i++)
    cc[i][problem_size]     = cc[i][problem_size-1];

  /* Reflect West Border */
  for (i=2;i<problem_size+1;i++)
    cc[1][i]     = cc[2][i];

  /* Reflect East Border */
  for (i=2;i<problem_size+1;i++)
    cc[problem_size][i]     = cc[problem_size-1][i];
  
  /* Reflect North Border */
  for (i=1;i<problem_size+1;i++)
    cc[i][1]     = cc[i][2];
  
}


int make_sigma(double deltat, double new_rho[MAXSIZE][MAXSIZE],
	       double new_alpha[MAXSIZE][MAXSIZE], double sigma[MAXSIZE][MAXSIZE])
{
  int i,j;
  for (i=2;i<problem_size;i++)
    for (j=2;j<problem_size;j++)
      sigma[i][j]=new_rho[i][j] * new_alpha[i][j] * specific_heat/deltat;
}


int make_gamma_interior(double new_r[MAXSIZE][MAXSIZE],double new_z[MAXSIZE][MAXSIZE],
			double cc[MAXSIZE][MAXSIZE],int x_succ, int y_succ,
			int x_adj, int y_adj, double gamma[MAXSIZE][MAXSIZE])
{
  int i,j;
  double r1,z1,r2,z2,cross_section,c1,c2,specific_conductivity;
  for (i=2;i<problem_size;i++)
    for (j=2;j<problem_size;j++){
      r1=new_r[i][j];
      z1=new_z[i][j];
      r2=new_r[i+x_adj][j+y_adj];
      z2=new_z[i+x_adj][j+y_adj];
      cross_section = 0.5 * (r1+r2) * ((r1-r2)*(r1-r2)) +
	((z1-z2)*(z1-z2));
      c1=cc[i][j];
      c2=cc[i+x_succ][j+y_succ];
      specific_conductivity=2.0 * c1 * c2 / (c1 + c2);
      /*printf("spec_cond = %lf\n",specific_conductivity);*/
      gamma[i][j]=cross_section*specific_conductivity;
      
    }
}

int make_gamma_boundaries(double gamma[MAXSIZE][MAXSIZE])
{
  int i;
  /* South Border */
  for (i=2;i<problem_size;i++)
    gamma[i][problem_size]     = 0.0;

  /* West Border */
  for (i=2;i<problem_size+1;i++)
    gamma[1][i]     = 0.0;

  /* East Border */
  for (i=2;i<problem_size+1;i++)
    gamma[problem_size][i]     = 0.0;
  
  /* North Border */
  for (i=1;i<problem_size+1;i++)
    gamma[i][1]     = 0.0;
  
}

int make_ab_boundaries(double a[MAXSIZE][MAXSIZE],double b[MAXSIZE][MAXSIZE],
		       double theta[MAXSIZE][MAXSIZE])
{
  int i;
  /* South Border */
  for (i=2;i<problem_size;i++){
    a[i][problem_size]     = 0.0;
    b[i][problem_size]     = theta[i][problem_size];}
  /* West Border */
  for (i=2;i<problem_size+1;i++){
    a[1][i]     = 0.0;
    b[1][i]     = theta[1][i];}
  /* East Border */
  for (i=2;i<problem_size+1;i++){
    a[problem_size][i]     = 0.0;
    b[problem_size][i]     = theta[problem_size][i];}
  /* North Border */
  for (i=1;i<problem_size+1;i++){
    a[i][1]     = 0.0;
    b[i][1]     = theta[i][1];}
}

int make_ab_interior(double theta[MAXSIZE][MAXSIZE],double sigma[MAXSIZE][MAXSIZE],
		     double gamma[MAXSIZE][MAXSIZE],int x_prec, int y_prec,
		     double a[MAXSIZE][MAXSIZE],double b[MAXSIZE][MAXSIZE])
{
  int i,j;
  for (i=2;i<problem_size;i++)
    for (j=2;j<problem_size;j++){
      double denom=sigma[i][j] + gamma[i][j] + gamma[i+x_prec][j+y_prec]
	* (1.0 - a[i+x_prec][j+y_prec]);
      double nume1 = gamma[i][j];
      double nume2 = gamma[i+x_prec][j+y_prec] * b[i+x_prec][j+y_prec] +
	sigma[i][j] * theta[i][j];
      a[i][j]=nume1/denom;
      b[i][j]=nume2/denom;
    }
}

int make_theta_boundaries(double theta[MAXSIZE][MAXSIZE])
{
  int i;
  /* South Border */
  for (i=2;i<problem_size;i++)
    theta[i][problem_size]     = constant_heat_source;

  /* West Border */
  for (i=2;i<problem_size+1;i++)
    theta[1][i]     = constant_heat_source;

  /* East Border */
  for (i=2;i<problem_size+1;i++)
    theta[problem_size][i]     = constant_heat_source;
  /* North Border */
  for (i=1;i<problem_size+1;i++)
    theta[i][1]     = constant_heat_source;
}

int make_theta_interior(double a[MAXSIZE][MAXSIZE],double b[MAXSIZE][MAXSIZE],
			int x_succ,int y_succ,int x_init, int x_step, int x_term, 
			int y_init, int y_step, int y_term,
			double theta[MAXSIZE][MAXSIZE])
{
  int i,j;
  for (i=x_init;i!=x_term;i=i+x_step)
    for (j=y_init;j!=y_term;j=j+y_step)
      theta[i][j]=a[i][j] * theta[i+x_succ][j+y_succ] + b[i][j];
  
  
}
      

int compute_heat_conduction(double theta_hat[MAXSIZE][MAXSIZE], double deltat,
			    double new_r[MAXSIZE][MAXSIZE],double new_z[MAXSIZE][MAXSIZE],
			    double new_alpha[MAXSIZE][MAXSIZE], double new_rho[MAXSIZE][MAXSIZE],
			    double theta_l[MAXSIZE][MAXSIZE],double Gamma_k[MAXSIZE][MAXSIZE],
			    double Gamma_l[MAXSIZE][MAXSIZE])
{
  double sigma[MAXSIZE][MAXSIZE] ;
  double cc[MAXSIZE][MAXSIZE] ;
  double a_k[MAXSIZE][MAXSIZE],b_k[MAXSIZE][MAXSIZE];
  double theta_k[MAXSIZE][MAXSIZE];
  double a_l[MAXSIZE][MAXSIZE],b_l[MAXSIZE][MAXSIZE];
  
  make_sigma(deltat,new_rho,new_alpha,sigma);
  make_cc_interior(new_alpha,theta_hat,cc);
  make_cc_boundaries(cc);
  make_gamma_interior(new_r,new_z,cc,0,-1,1,0,Gamma_k);
  make_gamma_boundaries(Gamma_k);
  make_ab_boundaries(a_k,b_k,theta_hat);
  make_ab_interior(theta_hat,sigma,Gamma_k,0,-1,a_k,b_k);
  make_theta_boundaries(theta_k);
  make_theta_interior(a_k,b_k,0,1,2,1,problem_size,problem_size-1,-1,1,theta_k);
  make_gamma_interior(new_r,new_z,cc,-1,0,0,1,Gamma_l);
  make_gamma_boundaries(Gamma_l);
  
  make_ab_boundaries(a_l,b_l,theta_k);
  make_ab_interior(theta_k,sigma,Gamma_l,-1,0,a_l,b_l);
  make_theta_boundaries(theta_l);
  make_theta_interior(a_l,b_l,1,0,problem_size-1,-1,1,2,1,problem_size,theta_l);
};

double mass(int i, int j)
{
  double mass = new_rho[i][j] * new_alpha[i][j];
  return mass;
}

double compute_internal_energy()
{
  int i,j;
  double i_energy=0.0;
  for (i=2;i<problem_size;i++)
    for (j=2;j<problem_size;j++)
      i_energy+=new_epsilon[i][j] * mass(i,j);
  return i_energy;
}

double kinetic(int i,int j,
	       double new_u[MAXSIZE][MAXSIZE], double new_w[MAXSIZE][MAXSIZE])
{
  double average_mass = 0.25 * ((mass(i,j)) + (mass(i,j+1)) + 
				(mass(i+1,j+1)) + (mass(i+1,j)));
  double v_square = new_u[i][j]*new_u[i][j] + new_w[i][j]*new_w[i][j];
  
  double kinetic = 0.5 * average_mass * v_square;
  return kinetic;
}

double compute_kinetic_energy(double new_u[MAXSIZE][MAXSIZE],
			      double new_w[MAXSIZE][MAXSIZE])
{
  int i,j;
  double k_energy=0.0;
  for (i=1;i<problem_size;i++)
    for (j=1;j<problem_size;j++)
      k_energy += kinetic(i,j,new_u,new_w);
  return k_energy;
}

double work_done(int i1,int j1, int i2, int j2, double new_r[MAXSIZE][MAXSIZE],
		 double new_z[MAXSIZE][MAXSIZE], double new_u[MAXSIZE][MAXSIZE], 
		 double new_w[MAXSIZE][MAXSIZE], double new_p[MAXSIZE][MAXSIZE], 
		 double new_q[MAXSIZE][MAXSIZE], double deltat)
{
  double r1,r2,z1,z2,u1,u2,w1,w2,p1,p2,q1,q2;
  double force, radius, area, w_done;
  
  r1 = new_r[i1][j1];
  r2 = new_r[i2][j2];
  z1 = new_z[i1][j1];
  z2 = new_z[i2][j2];
  u1 = new_p[i1][j1]; /*ANDY I BELIEVE THERE WAS AN ERROR HERE */
  u2 = new_p[i2][j2]; /*I TRIED CHANGING IT BUT THE ID VERSION WAS CRASHING*/
  w1 = new_z[i1][j1]; /*SO I AM DUPLICATING TH ERROR.*/
  w2 = new_z[i2][j2];
  /*u1 = new_u[i1][j1];  
    u2 = new_u[i2][j2];  
    w1 = new_w[i1][j1];  
    w2 = new_w[i2][j2];*/
  p1 = new_p[i1][j1];
  p2 = new_p[i2][j2];
  q1 = new_q[i1][j1];
  q2 = new_q[i2][j2];
  
  force = 0.5 * (p1 + p2 + q1 + q2);
  radius = 0.5 * (r1 + r2);
  /* I also think this calculation was wrong in the original id code 
     area = 0.5 * ((r1 - r2)*(w1-w2) - (z1-z2)*(u1-u2));*/
  area = 0.5 * ((r1 - r2)*(u1-u2) - (z1-z2)*(w1-w2));
  w_done = force * radius * area * deltat;
  return w_done;
}

double compute_boundary_work(double new_r[MAXSIZE][MAXSIZE],
		 double new_z[MAXSIZE][MAXSIZE], double new_u[MAXSIZE][MAXSIZE], 
		 double new_w[MAXSIZE][MAXSIZE], double new_p[MAXSIZE][MAXSIZE], 
		 double new_q[MAXSIZE][MAXSIZE], double deltat)
{
  int i,j,k,l;
  double boundary_work;
  double w1=0.0;
  double w2=0.0;
  double w3=0.0;
  double w4=0.0;
    
  /*work done on north line */
  l=1;
  j=1;
  for (k=2;k<problem_size;k++){
    i=k-1;
    w1+=work_done(i,j,k,l,new_r,new_z,new_u,new_w,new_p,new_q,deltat);
  }
  
  /*work done on south line */
  l=problem_size-1;
  j=problem_size-1;
  for (k=2;k<problem_size;k++){
    i=k-1;
    w2+=work_done(i,j,k,l,new_r,new_z,new_u,new_w,new_p,new_q,deltat);
  }
  
  /*work done on east line */
  k=problem_size-1;
  i=problem_size-1;
  for (l=2;l<problem_size;l++){
    j=l+1;
    w3+=work_done(i,j,k,l,new_r,new_z,new_u,new_w,new_p,new_q,deltat);
  }
  
  /*work done on west line */
  k=2;
  i=2;
  for (l=2;l<problem_size;l++){
    j=l+1;
    w4+=work_done(i,j,k,l,new_r,new_z,new_u,new_w,new_p,new_q,deltat);
  }
  boundary_work = w1 + w2 + w3 + w4;
  return boundary_work;
}

double heat_flow(double gamma[MAXSIZE][MAXSIZE],double new_theta[MAXSIZE][MAXSIZE],
		 int i1,int j1,int i2,int j2,double deltat)
{
  double h_flow = deltat * gamma[i1][j1] * (new_theta[i1][j1] - new_theta[i2][j2]);
  return h_flow;
}
  
double compute_boundary_heat(double Gamma_k[MAXSIZE][MAXSIZE],
			     double Gamma_l[MAXSIZE][MAXSIZE],
			     double new_theta[MAXSIZE][MAXSIZE], double deltat)
{
  int i,j,k,l;
  double boundary_heat;
  double h1=0.0;
  double h2=0.0;
  double h3=0.0;
  double h4=0.0;

  /* heat flow on north */
  for (k=2;k<problem_size;k++){
    l=2;
    i=k;
    j=1;
    h1+=heat_flow(Gamma_k,new_theta,i,j,k,l,deltat);
  }
  /* heat flow on south */
  for (k=3;k<problem_size-1;k++){
    l=problem_size-1;
    i=k;
    j=problem_size;
    h2+=heat_flow(Gamma_k,new_theta,i,j,k,l,deltat);
  }
  /* heat flow on east */
  for (l=3;l<problem_size;l++){
    k=problem_size-1;
    i=problem_size;
    j=l;
    h3+=heat_flow(Gamma_l,new_theta,i,j,k,l,deltat);
  }
  /* heat flow on west */
  for (l=3;l<problem_size;l++){
    k=2;
    i=1;
    j=l;
    h4+=heat_flow(Gamma_l,new_theta,i,j,k,l,deltat);
  }
  boundary_heat= h1 + h2 + h3 + h4;
  return boundary_heat;
}



int compute_energy_error(double new_u[MAXSIZE][MAXSIZE], double new_w[MAXSIZE][MAXSIZE],
			 double new_r[MAXSIZE][MAXSIZE], double new_z[MAXSIZE][MAXSIZE],
			 double new_p[MAXSIZE][MAXSIZE], double new_q[MAXSIZE][MAXSIZE],
			 double new_epsilon[MAXSIZE][MAXSIZE], double new_theta[MAXSIZE][MAXSIZE],
			 double new_rho[MAXSIZE][MAXSIZE], double new_alpha[MAXSIZE][MAXSIZE],
			 double Gamma_k[MAXSIZE][MAXSIZE], double Gamma_l[MAXSIZE][MAXSIZE],
			 double deltat, double *new_c)
{
  double internal_energy = compute_internal_energy();
  double kinetic_energy =  compute_kinetic_energy(new_u,new_w);
  double boundary_work = compute_boundary_work(new_r,new_z,new_u,new_w,new_p,new_q,deltat);
  double boundary_heat = compute_boundary_heat(Gamma_k,Gamma_l,new_theta,deltat);
  *new_c = internal_energy + kinetic_energy - 
    boundary_heat - boundary_work;
};

double get_deltat_conduct(double theta_hat[MAXSIZE][MAXSIZE],double new_theta[MAXSIZE][MAXSIZE],
			  int i, int j)
{
  double d_conduct = fabs((theta_hat[i][j]-new_theta[i][j])/theta_hat[i][j]);
  return d_conduct;
}

double minn(double a,double b)
{
  if (a>b)
    return b;
  else return a;
}
      

int compute_time_step(double d[MAXSIZE][MAXSIZE],double new_d[MAXSIZE][MAXSIZE],
		      double theta_hat[MAXSIZE][MAXSIZE],double new_theta[MAXSIZE][MAXSIZE],
		      double *new_deltat)
{
  double deltat_courant = d[2][2];
  double deltat_conduct = fabs((theta_hat[2][2]-new_theta[2][2])/theta_hat[2][2]);
  /* double deltat_conduct = get_deltat_conduct(theta_hat,new_theta,2,2); */
  double deltat_minimum;
  int i,j;
  for (i=2;i<problem_size;i++)
    for (j=2;j<problem_size;j++){
      if (deltat_courant > d[i][j] )
	deltat_courant = d[i][j];
    }
  for (i=2;i<problem_size;i++)
    for (j=2;j<problem_size;j++){
      double temp = fabs((theta_hat[i][j]-new_theta[i][j])/theta_hat[i][j]);
      /*get_deltat_conduct(theta_hat,new_theta,i,j);*/
      if (deltat_conduct < temp)
	deltat_conduct = temp;
    }
  deltat_minimum = minn(deltat_courant, deltat_conduct);
  *new_deltat = minn(deltat_maximum,deltat_minimum);
  
};


int compute_pressure_interior(double rho[MAXSIZE][MAXSIZE],double theta[MAXSIZE][MAXSIZE],
			      double p[MAXSIZE][MAXSIZE])
{
  int i, j;
  
  for (i=2;i<problem_size;i++)
    for (j=2;j<problem_size;j++)
      p[i][j] = zonal_pressure(rho[i][j], theta[i][j]);
}

/* For the border pressure calculations the indexes for
   pbb[] and pb[] are supposed to be got from nbc[]. 
   We have just taken those values from nbc[] and put them 
   in. North, West and East are 1. South is 2, and the
   corners are all 4.*/
int compute_pressure_border(double p[MAXSIZE][MAXSIZE])
{
  int i;
  /* Compute North */
  for(i=2;i<problem_size;i++)
    p[i][1] = pbb[1] + pb[1] * p[i][2];
  /* Compute South */
  for(i=2;i<problem_size;i++)
    p[i][problem_size] = pbb[2] + pb[2] * p[i][problem_size-1];
  /* Compute West */
  for(i=2;i<problem_size;i++)
    p[1][i] = pbb[1] + pb[1] * p[2][i];
  /* Compute East */
  for(i=2;i<problem_size;i++)
    p[problem_size][i] = pbb[1] + pb[1] * p[problem_size-1][i];
  /* Compute Northwest */
  p[1][1] = pbb[4] + pb[4] * p[1][2];
  /* Compute Northeast */
  p[problem_size][1] = pbb[4] + pb[4] * p[problem_size][2];
  /* Compute Southwest */
  p[1][problem_size] = pbb[4] + pb[4] * p[2][problem_size];
  /* Compute Southeast */
  p[problem_size][problem_size] = pbb[4] + pb[4] * p[problem_size-1][problem_size];
}

int make_pressure(double rho[MAXSIZE][MAXSIZE],double theta[MAXSIZE][MAXSIZE],
		  double p[MAXSIZE][MAXSIZE])
{
  compute_pressure_interior(rho,theta,p);
  compute_pressure_border(p);
}

/*
  ENERGY
  */

int compute_energy_interior(double rho[MAXSIZE][MAXSIZE],double theta[MAXSIZE][MAXSIZE],
			    double epsilon[MAXSIZE][MAXSIZE])
{
  int i, j;
  
  for (i=2;i<problem_size;i++)
    for (j=2;j<problem_size;j++)
      epsilon[i][j] = zonal_energy(rho[i][j], theta[i][j]);
}


/* Boundary energy are the reflections of adjacent zones. */
int compute_energy_border(double epsilon[MAXSIZE][MAXSIZE])
     /* The pamphlet says that the internal energy of the
	boundary zones is set to zero. The id code does not do that.*/
{
  int i;
  
  /* Reflect South Border */
  for (i=2;i<problem_size;i++)
    epsilon[i][problem_size] = epsilon[i][problem_size-1];
  /* Reflect West Border */
  for (i=2;i<problem_size+1;i++)
    epsilon[1][i] = epsilon[2][i];
  /* Reflect East Border */
  for (i=2;i<problem_size+1;i++)
    epsilon[problem_size][i] = epsilon[problem_size-1][i];
  /* Reflect North Border */
  for (i=1;i<problem_size+1;i++)
    epsilon[i][1] = epsilon[i][2];
  
}

int make_energy(double rho[MAXSIZE][MAXSIZE],double theta[MAXSIZE][MAXSIZE],
		double epsilon[MAXSIZE][MAXSIZE])
{
  compute_energy_interior(rho,theta,epsilon);
  compute_energy_border(epsilon);
}

/*
  Initialization Routines
  */ 
int compute_initial_position_vectors_interior()
{
  int i, j;
  
  /* first, fill in the intererior of the position vector arrays*/
  for (i=1;i<problem_size;i++)
    for (j=1;j<problem_size;j++)
      {
	double rp = problem_size-2.0;
	double z1 = 10 + j - 1;
	double zz = (-0.5 + (i - 1) / rp) * 3.1415926535898;
	
	xx[i][j] = z1 * cos(zz);
	xy[i][j] = z1 * sin(zz);
      }
}

int compute_initial_position_vectors()
{
  compute_initial_position_vectors_interior();
  compute_position_vectors_boundaries(xx, xy);
}

int compute_initial_alpha_s_vectors(double alpha[MAXSIZE][MAXSIZE],double s[MAXSIZE][MAXSIZE],
				    double r[MAXSIZE][MAXSIZE],double z[MAXSIZE][MAXSIZE])
{
  /*compute_alpha_s_rho_vectors_interior(alpha,);*/
  int i, j;
  
  for (i=2;i<problem_size;i++)
    for (j=2;j<problem_size;j++)
      zone_area_vol(&alpha[i][j], &s[i][j], r, z, i, j);

  /*  compute_alpha_s_rho_vectors_boundaries();*/
  for (i=2;i<problem_size;i++)
    {
      alpha[i][problem_size] = alpha[i][problem_size-1];
      s[i][problem_size]     = s[i][problem_size-1];
    }
  /* Reflect West Border */
  for (i=2;i<problem_size+1;i++)
    {
      alpha[1][i] = alpha[2][i];
      s[1][i]     = s[2][i];
    }
  /* Reflect East Border */
  for (i=2;i<problem_size+1;i++)
    {
      alpha[problem_size][i] = alpha[problem_size-1][i];
      s[problem_size][i]     =s[problem_size-1][i];      
    }
  /* Reflect North Border */
  for (i=1;i<problem_size+1;i++)
    {
      alpha[i][1] = alpha[i][2];
      s[i][1]     = s[i][2];
    }

};

int compute_initial_theta()
{
  int i, j;

  /* Fill in the middle with 10.0 */
  for (i=2;i<problem_size;i++)
    for (j=2;j<problem_size;j++)
      theta[i][j] = 10.0;

  /* Fill in the borders with constant_heat_source */
  for (i=1;i<problem_size+1;i++)
    {
      theta[1][i] = constant_heat_source;
      theta[problem_size][i] = constant_heat_source;
      theta[i][1] = constant_heat_source;
      theta[i][problem_size] = constant_heat_source;
    }      
}

int compute_initial_p()
{
  make_pressure(rho,theta,p);
}

int compute_initial_epsilon()
{
  make_energy(rho, theta, epsilon);
};

/* Initialize the state variables */
int compute_initial_state()
{
  int i, j;

  /* zero out the velocity vectors */
  for (i=0;i<problem_size+1;i++)
    for (j=0;j<problem_size+1;j++)
      {
	vx[i][j] = 0.0;
	vy[i][j] = 0.0;
      }

  compute_initial_position_vectors();
  compute_initial_alpha_s_vectors(alpha,s,xx,xy);

  /* Initialize rho zone values */
  for (i=1;i<problem_size+1;i++)
    for (j=1;j<problem_size+1;j++)
      {
	rho[i][j] = 1.4;
	q[i][j]   = 0.0;
      }

  compute_initial_theta();

  /* the p array depends of rho and theta */
  compute_initial_p();
  
  /* the epsilon array depends of rho and theta */
  compute_initial_epsilon();
  
  deltat = 0.01;
  c = 0.0;
}

int print_simple_state()
{
  PRINT_NODE_MATRIX(vx);
  PRINT_NODE_MATRIX(vy);
  PRINT_NODE_MATRIX(xx);
  PRINT_NODE_MATRIX(xy);

  PRINT_ZONE_MATRIX(alpha);
  PRINT_ZONE_MATRIX(s);
  PRINT_ZONE_MATRIX(rho);
  PRINT_ZONE_MATRIX(p);
  PRINT_ZONE_MATRIX(q);
  PRINT_ZONE_MATRIX(epsilon);
  PRINT_ZONE_MATRIX(theta);

  printf("deltat = %lf, c = %lf\n",deltat, c);
}

int print_next_state()
{
  PRINT_NODE_MATRIX(new_vx);
  PRINT_NODE_MATRIX(new_vy);
  PRINT_NODE_MATRIX(new_xx);
  PRINT_NODE_MATRIX(new_xy);
  
  PRINT_ZONE_MATRIX(new_alpha);
  PRINT_ZONE_MATRIX(new_s);
  PRINT_ZONE_MATRIX(new_rho);
  PRINT_ZONE_MATRIX(new_p);
  PRINT_ZONE_MATRIX(new_q);
  PRINT_ZONE_MATRIX(new_epsilon);
  PRINT_ZONE_MATRIX(new_theta);
  printf("deltat = %lf, c = %lf\n",new_deltat, new_c);
}

/* Main function to modify state for next time step */
int compute_next_state()
{ 
  int i,j;
  make_velocity(new_vx,new_vy,vx,vy,xx,xy,p,q,deltat); 
  compute_position_vectors(new_xx, new_xy, xx, xy, deltat, new_vx, new_vy);
  make_area_density_volume(rho,s,new_xx,new_xy,new_alpha,new_rho,new_s);
  make_viscosity(p,new_q,d,new_vx,new_vy,new_xx,new_xy,new_alpha,new_rho);
  make_temperature(p,epsilon,rho,theta,theta_hat,new_rho,new_q);
  compute_heat_conduction(theta_hat,deltat,new_xx,new_xy,new_alpha,new_rho,new_theta,Gamma_k,Gamma_l);
  make_pressure(new_rho,new_theta,new_p);
  make_energy(new_rho,new_theta,new_epsilon);
  compute_energy_error(new_vx,new_vy,new_xx,new_xy,
		       new_p,new_q,new_epsilon,new_theta,new_rho,
		       new_alpha,Gamma_k,Gamma_l,deltat,&new_c);
  compute_time_step(d,new_d,theta_hat,new_theta,&new_deltat);

  /*  Copy the new version to the old version  */
  for (i=0;i<=problem_size;i++)
    for (j=0;j<=problem_size;j++)
      {
	vx[i][j] = new_vx[i][j];
	vy[i][j] = new_vy[i][j];
	xx[i][j] = new_xx[i][j];
	xy[i][j] = new_xy[i][j];
	alpha[i][j] = new_alpha[i][j];
	s[i][j] = new_s[i][j];
	rho[i][j] = new_rho[i][j];
	q[i][j] = new_q[i][j];
	p[i][j] = new_p[i][j];
	epsilon[i][j] = new_epsilon[i][j];
	theta[i][j] = new_theta[i][j];
      }  
  deltat = new_deltat;
  c = new_c;
}

/*
  Simple will take two command line arguments: number of iterations and problem
  size.  The problem size is assumed to be square, and the parameter for each
  should be an integer.
  */
main(int argc, char *argv[])
{
  int iter;

  if (argc!=3)
    {
      printf("EK's Simple expects two arguments: # iterations, problem size\n");
      printf("The two arguments are integers, and the problem size should\n");
      printf("be at least 4\n");
      exit(1);
    }
  
  niterations = atoi(argv[1]);
  problem_size = atoi(argv[2]);
  
  compute_initial_state();
  
  for (iter=0;iter<niterations;iter++)
    compute_next_state();

  print_simple_state();

  exit(0);
}
