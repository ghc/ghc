#define UNICODE
#include <windows.h>
#include <math.h>

/*
 * Rotatable Ellipse hack
 *
 * Win95 (Win32?) doesn't support rotating ellipses - so we
 * implement them with polygons.
 *
 * We use a fixed number of edges rather than varying the number
 * according to the radius of the ellipse.
 * If anyone feels like improving the code (to vary the number),
 * they should place a fixed upper bound on the number of edges
 * since it takes a relatively long time to draw 1000 edges.
 */

int transformedEllipse(
	HDC hdc, LONG x0, LONG y0, LONG x1, LONG y1, LONG x2, LONG y2) {
  static BOOL firstTime = 1;
  static double sins[20];
  static double coss[20];

  int   i;
  POINT pts[20];

  double x = (x1 + x2) / 2;  /* centre of parallelogram */
  double y = (y1 + y2) / 2;

  double dx1 = (x1 - x0) / 2; /* distance to corners from centre */
  double dy1 = (y1 - y0) / 2;
  double dx2 = (x2 - x0) / 2;
  double dy2 = (y2 - y0) / 2;

  if (firstTime) {
    double a  = 0.0;
    double da = 2.0*3.14159 / 20;
    for (i=0; i < 20; ++i, a+=da) {
        sins[i] = sin(a);
        coss[i] = cos(a);
    }
    firstTime = 0;
  }
  for(i=0; i < 20; ++i) {
    double c = coss[i];
    double s = sins[i];
    pts[i].x = x + c*dx1 + s*dx2;
    pts[i].y = y + c*dy1 + s*dy2;
  }
  return Polygon(hdc,pts,20);
}
