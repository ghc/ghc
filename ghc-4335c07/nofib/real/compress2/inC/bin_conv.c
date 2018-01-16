/*
 * bin_conv.c
 *
 * Take two codes and turn them into three 8-bit ints (ala LZW)
 */

conv ()
{
  register short int x, y;
  register short int o1, o2, o3;
  int i;

  x = 3077; y = 1192;

  o1 = x >> 4;
  o2 = ((x & 15) << 4) + (y >> 8);
  o3 = y & 255;

  printf ("%d, %d, %d\n", o1, o2, o3);
}

main ()
{

  register int i;
  
  for (i=0; i < 1000; i++)
    conv ();
}
