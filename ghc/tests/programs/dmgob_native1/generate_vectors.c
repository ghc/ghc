#include <stdio.h>

int main ( )
{
  FILE *ptr_output_file;

  int dim;
  float x[10];


  ptr_output_file = fopen("test_data", "wb");
  if ( ptr_output_file == (FILE *) NULL ) {
    fprintf(stderr, "Can't open file test_data\n");
    perror("fopen");
    exit(1);
  }


  dim = 3;
  x[0] = 1.0F;
  x[1] = 2.0F;
  x[2] = 3.0F;
  fwrite( &dim, sizeof(int), 1, ptr_output_file );
  fwrite( x, sizeof(float), dim, ptr_output_file );

  dim = 2;
  x[0] = 1.0F;
  x[1] = 2.0F;
  fwrite( &dim, sizeof(int), 1, ptr_output_file );
  fwrite( x, sizeof(float), dim, ptr_output_file );

  dim = 4;
  x[0] = 1.0F;
  x[1] = 2.0F;
  x[2] = 3.0F;
  x[3] = 4.0F;
  fwrite( &dim, sizeof(int), 1, ptr_output_file );
  fwrite( x, sizeof(float), dim, ptr_output_file );

  fclose( ptr_output_file );
  return 0;
}
