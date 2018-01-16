// Test data generator for SMVM.
//   Be careful to compile it with the correct machine specifier (ie -m32)
//   or you'll get errors when you try and run the Haskell version on it.
//  
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>

#include <HsFFI.h>


// Number of columns in the matrix.
HsInt cols;

// Number of rows in the matrix.
HsInt rows;

// Ratio of non-zero to zero values in the matrix.
double ratio;

// The length of each row in the matrix.
HsInt *lengths;

// Index buffer.
// Used when generating the indices for each row.
HsInt *indices;

// We can make matricies of floats or doubles.
enum { FLOAT, DOUBLE } type;


// Write some random non-zero doubles to a file.
// Returns the sum of all the values.
HsDouble gen_doubles( int file, HsInt n )
{
  HsDouble d;
  HsDouble sum = 0;

  int a, b;

  while( n != 0 )
  {
    a = random() % 1000;
    b = random() % 1000;
    if (a == 0 || b == 0)
      d = 0.1;
    else
      d = ((HsDouble)a) / ((HsDouble)b);

    write( file, &d, sizeof(HsDouble) );
    sum += d;
    --n;
  }
  return sum;
}


// Write some random, non-zero floats to a file.
// Returns the sum of all the values.
HsFloat gen_floats( int file, HsInt n )
{
  HsFloat d;
  HsFloat sum = 0;

  int a, b;

  while( n != 0 )
  {
    a = random() % 1000;
    b = random() % 1000;
    if (a == 0 || b == 0)
      d = 0.1;
    else
      d = ((HsFloat)a) / ((HsFloat)b);

    write( file, &d, sizeof(HsFloat) );
    sum += d;
    --n;
  }
  return sum;
}


// Generate lengths of each of the rows in the matrix.
// Returns the total number of elements.
HsInt gen_lengths()
{
  HsInt i;
  HsInt n = 0;

  // Set the maximum number of elements for the row.
  // Doing it this way give us between  0 and COLS  elems for 
  // a ratio of 0.5. Ratios of > 0.5 don't work.
  int range = ((double)cols * 2) * ratio;
  
  for( i = 0; i < rows; ++i ) {
    lengths[i] = random() % range;
    n += lengths[i];
  }

  return n;
}


// Check if an index has already been used.
int find_index( int from, int to, HsInt idx )
{
  while( from != to ) {
    if( indices[from] == idx ) return 1;
    ++from;
  }
  return 0;
}


// Comparison function for HsInt.
int cmp_HsInt( const void *p, const void *q )
{
  HsInt x = *(HsInt *)p;
  HsInt y = *(HsInt *)q;

  if( x < y ) return -1;
  if( x > y ) return 1;
  return 0;
}


// Generate indices for a single row of the sparse matrix,
// and write them to the given file.
void gen_indices( int file )
{
  HsInt i, j;

  for( i = 0; i < rows; ++i ) {

    // We want an index for each of the (packed) elements in the row.
    for( j = 0; j < lengths[i]; ++j ) {

      // Keep looping around until we find an index that hasn't already been used.
      // NOTE: This sucks up most of the runtime.
      do {
        indices[j] = random() % cols;
      } while( find_index( 0, j, indices[j] ) );
    }
    
    // Sort the indices so they're in accending order.
    qsort( indices, j, sizeof(HsInt), cmp_HsInt );

    // Write indices out to file.
    write( file, indices, sizeof(HsInt) * j );
  }
}


int usage()
{
  puts( "mksm [float|double] COLS ROWS RATIO FILE" );
  exit(1);
}


int main( int argc, char *argv[] )
{
  HsInt n;

  int file;
  int arg;

  HsDouble sum1,sum2;

  if( argc == 1 || argc < 5 )
    usage();

  if( isdigit( argv[1][0] ) )
  {
    arg = 1;
    type = DOUBLE;
  }
  else
  {
    arg = 2;
    if( !strcmp( argv[1], "float" ) )
      type = FLOAT;
    else if( !strcmp( argv[1], "double" ) )
      type = DOUBLE;
    else
    {
      fputs( "mksm: Invalid type\n", stderr );
      usage();
    }
  }

  cols  = atoi( argv[arg++] );
  rows  = atoi( argv[arg++] );

  ratio = atof( argv[arg++] );
  if (ratio > 0.5) {
	fputs ("mksm: RATIO must be < 0.5\n", stderr);
	exit(1);
  }
   
  lengths = (HsInt *)malloc( rows * sizeof(HsInt) );
  indices = (HsInt *)malloc( cols * sizeof(HsInt) );
 
  if( arg >= argc )
    usage();

  file = creat( argv[arg], 0666 );

  n = gen_lengths();

  // Write some magic numbers to ward against wordsize screwups.
  HsInt magic1 = 0xc0ffee00;
  HsInt magic2 = 0x12345678;
  write( file, &magic1,  sizeof(HsInt));
  write( file, &magic2,  sizeof(HsInt));

  // Number of elements in each row.
  write( file, &rows,   sizeof(rows) );
  write( file, lengths, sizeof(HsInt) * rows );

  // Indices of all the elements.
  write( file, &n, sizeof(n) );
  gen_indices( file );

  // Values of the matrix elements.
  write( file, &n, sizeof(n) );
  if( type == DOUBLE )
    sum1 = gen_doubles(file, n);
  else
    sum1 = (HsDouble)gen_floats(file, n);

  // The dense vector.
  write( file, &cols, sizeof(cols) );
  if( type == DOUBLE )
    sum2 = gen_doubles(file, cols);
  else
    sum2 = (HsDouble)gen_floats(file, n);

  // Close the output file.
  close(file);

  printf( "columns              = %d\n", (int)cols);
  printf( "rows                 = %d\n", (int)rows);
  printf( "non-zero elements    = %d\n", (int)n);
  printf( "element size (bytes) = %d\n", (int)(type == FLOAT ? sizeof(HsFloat) : sizeof(HsDouble)));
  printf( "sum of matrix elements %Lf\n", (long double)sum1 );
  printf( "sum of vector elements %Lf\n", (long double)sum2 );

  return 0;
}

