#include <stdlib.h>
#include <stdio.h>
#include "Vector.h"

void svgLine
	( FILE*		file
	, char*		strColor
	, int x1, int y1
	, int x2, int y2)
{	
	fprintf ( file
		, "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" style=\"stroke:%s;stroke-width:2\"/>\n"
		, x1, y1, x2, y2, strColor);
}


void dumpPoints
	( FILE*		file
	, char*		strColor
	, int		radius
	, Vector*	points)
{
	for(int i = 0; i < points->length; i++) {
		double px	= points->x[i];
		double py	= points->y[i];
		fprintf	( file
			, "<circle cx=\"%d\" cy=\"%d\" r=\"%d\" style=\"stroke:%s\"/>\n"
			, (int)px
			, (int)py
			, radius
			, strColor);
	}
}


void dumpLineLoop 
	( FILE* 	file
	, char*		strColor
	, Vector*	points)
{
	for(int i = 0; i < points->length - 1; i++)
		svgLine	( file, strColor
			, points->x[i],   points->y[i]
			, points->x[i+1], points->y[i+1]);
	
	int j = points->length - 1;
	svgLine	( file, strColor
		, points->x[0], points->y[0]
		, points->x[j], points->y[j]);
}
	

void dumpSVG
	( FILE*		file
	, Vector*	points
	, Vector*	hull)
{
	fprintf(file, "<svg width=\"100%%\" height=\"100%%\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n");
	dumpPoints   (file, "#000000", 1, points);
	dumpPoints   (file, "#ff0000", 5, hull);
	dumpLineLoop (file, "#0000ff", hull);
	fprintf(file, "</svg>\n");
}
