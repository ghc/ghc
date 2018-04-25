
#include <math.h>
#include <stdlib.h>

#include "Vector.h"

// Compute the square of the distance between a point and a line.
static inline double distance
	( double x0, double y0		// point
	, double x1, double y1		// a point on the line
	, double x2, double y2)		// another point on the line
{
	return  ((x1 - x0) * (y2 - y0)) - ((y1 - y0) * (x2 - x0));
}


int hsplit
	( int depth,		int maxDepth
	, Vector** pointss,	Vector* hull
	, double  x1, 		double y1
	, double  x2, 		double y2)
{
	// The maximum depth to use is statically determined by the caller.
	assert(depth < maxDepth);

	// We read our points from the buffer at the current depth.
	Vector* points	= pointss[depth];

	// No hull points here.
	if (points->length == 0) 
		return depth;

	// The packed points go into the buffer at the next lowest depth.
	Vector* packed	= pointss[depth + 1];
	vector_rewind(packed);
		
	// Find the point that is furthest away from the line.
	// While we're doing this, record the points that are on the left of the line.
	double maxX	= 0;
	double maxY	= 0;
	double maxDist	= 0;
	for (int i = 0; i < points->length; i++) {
		double px	= points->x[i];
		double py	= points->y[i];
		double d 	= distance(px, py, x1, y1, x2, y2);

		if (d > 0) {
			vector_append(packed, px, py);

			if (d > maxDist) {
				maxDist	= d;
				maxX	= px;
				maxY	= py;
			}
		}
	}
	
	// All the points were on the right of the line
	if (packed->length == 0) {
		vector_append(hull, x1, y1);
		return depth;
	} 
	else {
		
		// Check the left segment
		int depthLeft  = hsplit(depth + 1, maxDepth, pointss, hull, x1, y1, maxX, maxY);

		// Check the right segment
		int depthRight = hsplit(depth + 1, maxDepth, pointss, hull, maxX, maxY, x2, y2);
			
		if (depthLeft > depthRight)
			return depthLeft;
		else	return depthRight;
	}
}


// Compute the convex hull of a set of points.
//	Returns the maximum depth used in the recursion.
int quickHull 
	( Vector* points	// The 2d points to use.
	, Vector* hull)		// Buffer to write the hull points to.
{
	// We're preallocating buffers to hold packed points at each level
	// to avoid the overhead of calls to malloc/free.
	// Maximum algorithm depth we're handling. 
	// Instead of statically setting this we could do a check at each level
	// And allocate some more if needed.
	int maxDepth		= 15;

	// No points, nothing to do.
	if (points->length == 0)
		return 0;

	// Handle special case of just one point here, so we don't end up
	// adding it to the hull twice.
	if (points->length == 1) {
		vector_append(hull, points->x[0], points->y[1]);
		return 1;
	}
	
	// Initialise point buffers.
	// Buffer 0 is set to the incoming points.
	Vector** pointss	= malloc(sizeof(Vector*) * maxDepth);
	pointss[0]		= points;
	for(int i = 1; i < maxDepth; i++)
		pointss[i]	= vector_new(points->length);
	
	// Find the left and right-most points.
	double leftX	= points->x[0];
	double leftY	= points->y[0];
	double rightX	= points->x[0];
	double rightY	= points->y[0];
	for(int i = 0; i < points->length; i++) {
		double px	= points->x[i];
		double py	= points->y[i];

		if (px < leftX) {
			leftX	= px;
			leftY	= py;
		}
		if (px > rightX) {
			rightX	= px;
			rightY	= py;
		}
	}

	// Determine hull points above and below the split.
	int depthLeft  = hsplit (0, maxDepth, pointss, hull, leftX,  leftY,  rightX, rightY);
	int depthRight = hsplit (0, maxDepth, pointss, hull, rightX, rightY, leftX,  leftY);

	// Return maximum recursion depth reached.
	if(depthLeft > depthRight)
		return depthLeft;
	else	return depthRight;
}
