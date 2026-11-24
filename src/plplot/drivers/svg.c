// November 8, 2006
//
// PLplot driver for SVG 1.1 (http://www.w3.org/Graphics/SVG/)
//
// Copyright (C) 2006 Hazen Babcock
//
// This file is part of PLplot.
//
// PLplot is free software; you can redistribute it and/or modify
// it under the terms of the GNU Library General Public License as published
// by the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// PLplot is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Library General Public License for more details.
//
// You should have received a copy of the GNU Library General Public License
// along with PLplot; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//

//---------------------------------------------
// Header files, defines and local variables
// ---------------------------------------------

#include <stdarg.h>
#include <math.h>

// PLplot header files

#include "plplotP.h"
#include "drivers.h"

// constants

#define SVG_Default_X      720
#define SVG_Default_Y      540

#define POINTS_PER_INCH    72

// local variables

PLDLLIMPEXP_DRIVER const char* plD_DEVICE_INFO_svg = "svg:Scalable Vector Graphics (SVG 1.1):1:svg:57:svg\n";

static int    already_warned = 0;

typedef struct
{
    short textClipping;
    int   which_clip;
    int   canvasXSize;
    int   canvasYSize;
    PLFLT scale;
    int   svgIndent;
    FILE  *svgFile;
	int patcount;
    //  char curColor[7];
} SVG;

// Debugging extras

//-----------------------------------------------
// function declarations
// -----------------------------------------------

// Functions for writing XML SVG tags to a file

static void svg_open( SVG *, const char * );
static void svg_open_end( SVG * );
static void svg_attr_value( SVG *, const char *, const char * );
static void svg_attr_values( SVG *, const char *, const char *, ... );
static void svg_close( SVG *, const char * );
static void svg_general( SVG *, const char * );
static void svg_indent( SVG * );
static void svg_stroke_width( PLStream * );
static void svg_stroke_color( PLStream * );
static void svg_fill_pattern( PLStream * , const char*);
static void svg_fill_color( PLStream * );
static void svg_fill_background_color( PLStream * );


// General

static void poly_line( PLStream *, short *, short *, PLINT, short );
static void fill_path(PLStream *);
static void FillPath(PLStream *pls);
static void write_hex( FILE *, unsigned char );

// PLplot interface functions

void plD_dispatch_init_svg( PLDispatchTable *pdt );
void plD_init_svg( PLStream * );
void plD_line_svg( PLStream *, short, short, short, short );
void plD_polyline_svg( PLStream *, short *, short *, PLINT );
void plD_eop_svg( PLStream * );
void plD_bop_svg( PLStream * );
void plD_tidy_svg( PLStream * );
void plD_state_svg( PLStream *, PLINT );
void plD_esc_svg( PLStream *, PLINT, void * );

//define LINE2D, POLYLINE2D
#define LINE2D plD_line_svg
#define POLYLINE2D plD_polyline_svg
#include "plplot3d.h"

//--------------------------------------------------------------------------
// dispatch_init_init()
//
// Initialize device dispatch table
//--------------------------------------------------------------------------

void plD_dispatch_init_svg( PLDispatchTable *pdt )
{
  currDispatchTab = pdt;
  Status3D = 0;
    pdt->pl_MenuStr = "Scalable Vector Graphics (SVG 1.1)";
    pdt->pl_DevName = "svg";
    pdt->pl_type     = plDevType_FileOriented;
    pdt->pl_seq      = 57;
    pdt->pl_init     = (plD_init_fp) plD_init_svg;
    pdt->pl_line     = (plD_line_fp) plD_line_svg;
    pdt->pl_polyline = (plD_polyline_fp) plD_polyline_svg;
    pdt->pl_eop      = (plD_eop_fp) plD_eop_svg;
    pdt->pl_bop      = (plD_bop_fp) plD_bop_svg;
    pdt->pl_tidy     = (plD_tidy_fp) plD_tidy_svg;
    pdt->pl_state    = (plD_state_fp) plD_state_svg;
    pdt->pl_esc      = (plD_esc_fp) plD_esc_svg;
}

//--------------------------------------------------------------------------
// svg_init()
//
// Initialize device
//--------------------------------------------------------------------------

void plD_init_svg( PLStream *pls )
{
    SVG *svgDev;

    pls->termin  = 0;                   // not an interactive device
    pls->color   = 1;                   // supports color
    pls->width   = 1;
    pls->verbose = 1;
    pls->bytecnt = 0;
    //pls->debug = 1;
    pls->page         = 0;
    pls->dev_fill0    = 1;      // driver generates solid fills
    pls->dev_fill1    = 0;      // Use PLplot core fallback for pattern fills
	pls->doPatternedFills = 1;  // can do patterned fills

    if ( ( pls->OutFile = fopen( pls->FileName, "wb+" ) ) == NULL )
            fprintf( stderr, "Can't open %s.\n", pls->FileName ); //plOpenFile( pls );

// Allocate and initialize device-specific data

    if ( pls->dev != NULL )
        free( (void *) pls->dev );

    pls->dev = calloc( 1, (size_t) sizeof ( SVG ) );
    if ( pls->dev == NULL )
        plexit( "plD_init_svg: Out of memory." );

    svgDev = (SVG *) pls->dev;
	svgDev->patcount=0;
    // Set the bounds for plotting in points (unit of 1/72 of an inch).  Default is SVG_Default_X points x SVG_Default_Y points unless otherwise specified by plspage or -geometry option.

    if ( pls->xlength <= 0 || pls->ylength <= 0 )
    {
        svgDev->canvasXSize = SVG_Default_X;
        svgDev->canvasYSize = SVG_Default_Y;
    }
    else
    {
        svgDev->canvasXSize = pls->xlength;
        svgDev->canvasYSize = pls->ylength;
    }
    // Calculate ratio of (larger) internal PLplot coordinates to external
    // coordinates used for svg file.
    if ( svgDev->canvasXSize > svgDev->canvasYSize )
        svgDev->scale = (PLFLT) svgDev->canvasXSize / (PLFLT) ( PIXELS_X - 1 );
    else
        svgDev->scale = (PLFLT) svgDev->canvasYSize / (PLFLT) PIXELS_Y ;
    plP_setphy( (PLINT) 0, (PLINT) ( svgDev->canvasXSize / svgDev->scale ), (PLINT) 0, (PLINT) ( svgDev->canvasYSize / svgDev->scale ) ); // Scaled points.

    plP_setpxl( POINTS_PER_INCH / ( 25.4 * svgDev->scale ), POINTS_PER_INCH / ( 25.4 * svgDev->scale ) );                                           // Scaled points/mm.

    svgDev->svgFile = pls->OutFile;

    svgDev->svgIndent      = 0;
    svg_general( svgDev, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" );
    svg_general( svgDev, "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n" );
    svg_general( svgDev, "        \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" );
}

//--------------------------------------------------------------------------
// svg_bop()
//
// Set up for the next page.
//--------------------------------------------------------------------------

void plD_bop_svg( PLStream *pls )
{
    SVG *svgDev = pls->dev;


    // write opening svg tag for the new page

    svg_open( svgDev, "svg" );
    svg_attr_value( svgDev, "xmlns", "http://www.w3.org/2000/svg" );
    svg_attr_value( svgDev, "xmlns:xlink", "http://www.w3.org/1999/xlink" );
    svg_attr_value( svgDev, "version", "1.1" );
    // svg_attr_values("width", "%dcm", (int)((double)canvasXSize/POINTS_PER_INCH * 2.54));
    // svg_attr_values("height", "%dcm", (int)((double)canvasYSize/POINTS_PER_INCH * 2.54));
    svg_attr_values( svgDev, "width", "%dpt", svgDev->canvasXSize );
    svg_attr_values( svgDev, "height", "%dpt", svgDev->canvasYSize );
    svg_attr_values( svgDev, "viewBox", "%d %d %d %d", 0, 0, svgDev->canvasXSize, svgDev->canvasYSize );
    svg_general( svgDev, ">\n" );

    // set the background by drawing a rectangle that is the size of
    // of the canvas and filling it with the background color.

    svg_open( svgDev, "rect" );
    svg_attr_values( svgDev, "x", "%d", 0 );
    svg_attr_values( svgDev, "y", "%d", 0 );
    svg_attr_values( svgDev, "width", "%d", svgDev->canvasXSize );
    svg_attr_values( svgDev, "height", "%d", svgDev->canvasYSize );
    svg_attr_value( svgDev, "stroke", "none" );
    svg_fill_background_color( pls );
    svg_open_end( svgDev );

    // invert the coordinate system so that PLplot graphs appear right side up

    svg_open( svgDev, "g" );
    svg_attr_values( svgDev, "transform", "matrix(1 0 0 -1 0 %d)", svgDev->canvasYSize );
    svg_general( svgDev, ">\n" );
}

//--------------------------------------------------------------------------
// svg_line()
//
// Draw a line in the current color from (x1,y1) to (x2,y2).
//--------------------------------------------------------------------------

void plD_line_svg( PLStream *pls, short x1a, short y1a, short x2a, short y2a )
{
    SVG *svgDev;

    svgDev = pls->dev;
    svg_open( svgDev, "polyline" );
    svg_stroke_width( pls );
    svg_stroke_color( pls );
    svg_attr_value( svgDev, "fill", "none" );
    // svg_attr_value(svgDev, "shape-rendering", "crispEdges");
    svg_attr_values( svgDev, "points", "%r,%r %r,%r", (double) x1a * svgDev->scale, (double) y1a * svgDev->scale, (double) x2a * svgDev->scale, (double) y2a * svgDev->scale );
    svg_open_end( svgDev );
}

//--------------------------------------------------------------------------
// svg_polyline()
//
// Draw a polyline in the current color.
//--------------------------------------------------------------------------

void plD_polyline_svg( PLStream *pls, short *xa, short *ya, PLINT npts )
{
    poly_line( pls, xa, ya, npts, 0 );
}

//--------------------------------------------------------------------------
// svg_eop()
//
// End of page
//--------------------------------------------------------------------------

void plD_eop_svg( PLStream *pls )
{
    SVG *svgDev;

    svgDev = pls->dev;
    // write the closing svg tag

    svg_close( svgDev, "g" );
    svg_close( svgDev, "svg" );
}

//--------------------------------------------------------------------------
// svg_tidy()
//
// Close graphics file or otherwise clean up.
//--------------------------------------------------------------------------

void plD_tidy_svg( PLStream *PL_UNUSED( pls ) )
{
}

//--------------------------------------------------------------------------
// plD_state_svg()
//
// Handle change in PLStream state (color, pen width, fill attribute, etc).
//
// Nothing is done here because these attributes are aquired from
// PLStream for each element that is drawn.
//--------------------------------------------------------------------------

void plD_state_svg( PLStream *PL_UNUSED( pls ), PLINT PL_UNUSED( op ) )
{
}

//--------------------------------------------------------------------------
// svg_esc()
//
// Escape function.
//--------------------------------------------------------------------------

void plD_esc_svg( PLStream *pls, PLINT op, void *ptr )
{
    switch ( op )
    {
    case PLESC_FILL_POLYGON:      // fill polygon
        if (Status3D == 1) { //enable use everywhere.
          //perform conversion on the fly
          for (PLINT i = 0; i < pls->dev_npts; ++i) {
            int x = pls->dev_x[i];
            int y = pls->dev_y[i];
            // 3D convert, must take into account that y is inverted.
            SelfTransform3D(&x, &y);

            pls->dev_x[i] = x;
            pls->dev_y[i] = y;
          }
        }
        poly_line( pls, pls->dev_x, pls->dev_y, pls->dev_npts, 1 );
        break;
    case PLESC_FILL_PATH:
        fill_path( pls );
        break;
	case PLESC_3D:
    Set3D(ptr);
    break;
  case PLESC_2D:
    UnSet3D();
    break;

    }
}
#define SVG_STYLE_FACTOR 100
void define_pattern(PLStream *pls, char* id) {
    SVG *svgDev;

    svgDev = pls->dev;
	int total = 0;
	for (int i = 0; i < plsc->lineStyleNumberOfElements; i++) {
		total += plsc->mark[i];
		total += plsc->space[i];
	}
	float thick = pls->width;
	float neededLength = (total == 0) ? 10 : (float) total / SVG_STYLE_FACTOR;
	float sep = 10 + thick;
	if (pls->delta[0] > 0) sep = MAX(2, pls->delta[0]*1e-3);
	float rot = (float) plsc->inclin[0] / 10.;
	int ix = neededLength + thick;
	int iy = sep;
	ix = MAX(10 + sep, ix);
	iy = MAX(1 + sep, iy);
	snprintf(id, 31, "hatch_%03.3d", svgDev->patcount++);
	fprintf(svgDev->svgFile, "\n<pattern id=\"%s\" width=\"%d\" height=\"%d\" "
			"patternTransform=\"rotate(%f 0 0)\" stroke-linecap=\"round\" patternUnits=\"userSpaceOnUse\">\n", id, ix, iy, rot);
	if (plsc->lineStyleNumberOfElements == 0) {
		fprintf(svgDev->svgFile, "    <line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" style=\"stroke-width:%f; stroke:#", thick, iy / 2, ix, iy / 2, thick);
		write_hex(svgDev->svgFile, pls->curcolor.r);
		write_hex(svgDev->svgFile, pls->curcolor.g);
		write_hex(svgDev->svgFile, pls->curcolor.b);
		fprintf(svgDev->svgFile, "\"/></pattern>");
	} else {
		int start = thick;
		int end = 0;
		for (int i = 0; i < plsc->lineStyleNumberOfElements; i++) {
			end = start + (float) plsc->mark[i] / SVG_STYLE_FACTOR;
			fprintf(svgDev->svgFile, "    <line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" style=\"stroke-width:%f; stroke:#", start, iy / 2, end, iy / 2, thick);
			write_hex(svgDev->svgFile, pls->curcolor.r);
			write_hex(svgDev->svgFile, pls->curcolor.g);
			write_hex(svgDev->svgFile, pls->curcolor.b);
			fprintf(svgDev->svgFile, "\"/>\n");
			start += (float) (plsc->mark[i] + plsc->space[i]) / SVG_STYLE_FACTOR;
		}
		fprintf(svgDev->svgFile, "</pattern>");
	}
}
//--------------------------------------------------------------------------
// poly_line()
//
// Handles drawing filled and unfilled polygons
//--------------------------------------------------------------------------

void poly_line( PLStream *pls, short *xa, short *ya, PLINT npts, short fill )
{
	char buf[32];
    SVG *svgDev;

    svgDev = pls->dev;
	if (plsc->patt) {
		define_pattern(pls,buf);
	}

	svg_open( svgDev, "polyline" );
    if ( fill )
    {
	//this is a (multiple) FILL. Will add a stroke under certain cases, below.
	if (pls->curcolor.a < 0.99 || plsc->patt|| plsc->patternFill) {
	    // pls->curcolor.a < 0.99: Two adjacent regions will put non-zero width boundary strokes on top
        // of each other on their common boundary.  Thus, a stroke on the boundary
        // of a filled region is generally a bad idea when the fill is partially
        // opaque because the partial opacity of the two boundary strokes which
        // are on top of each other will mutually interfere and produce a
        // bad-looking result.  On the other hand, for completely opaque fills
        // a boundary stroke is a good idea since if it is of sufficient width
        // it will keep the background from leaking through at the anti-aliased
        // edges of filled regions that have a common boundary with other
        // filled regions.
		svg_attr_value(svgDev, "stroke", "none");
	} else {
		svg_stroke_width(pls);
		svg_stroke_color(pls);
	}
	if (plsc->patt) {
		svg_fill_pattern(pls, buf);
	} else if (plsc->patternFill) {
		svg_fill_pattern(pls, pls->thePatternBase64);
	} else {
		svg_fill_color(pls);
	}
	if (pls->patt && pls->dev_eofill)
		svg_attr_value(svgDev, "fill-rule", "evenodd");
	else
		svg_attr_value(svgDev, "fill-rule", "nonzero");
    }
    else
    {
        svg_stroke_width( pls );
        svg_stroke_color( pls );
        svg_attr_value( svgDev, "fill", "none" );
    }
    //svg_attr_value(svgDev, "shape-rendering", "crispEdges");
    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "points=\"" );
    for ( int i = 0; i < npts; i++ )
    {
        fprintf( svgDev->svgFile, "%.2f,%.2f ", (double) xa[i] * svgDev->scale, (double) ya[i] * svgDev->scale );
        if ( ( ( i + 1 ) % 10 ) == 0 )
        {
            fprintf( svgDev->svgFile, "\n" );
            svg_indent( svgDev );
        }
    }
    fprintf( svgDev->svgFile, "\"/>\n" );
    svgDev->svgIndent -= 2;
}

//--------------------------------------------------------------------------
//  static void fill_path( PLStream *pls )
//
//  Fill special arrangement of values defining multiple path at once
//--------------------------------------------------------------------------
static void fill_path( PLStream *pls) {

  if (Status3D == 1) { //enable use everywhere.
    //perform conversion on the fly
    for (PLINT i = 0; i < pls->dev_npath; ++i) {
      PLINT *x = pls->dev_pathx[i];
      PLINT *y = pls->dev_pathy[i];
      for (PLINT j = 0; j < pls->dev_pathnxy[i]; ++j) {
        // 3D convert, must take into account that y is inverted.
        int ix=x[j];
        int iy=y[j];
        if (ix >= 0) { //avoid transforming the negative codes...
          SelfTransform3D(&ix, &iy);
        x[j]=ix;
        y[j]=iy;
        }
      }
    }
  }
  FillPath(pls);
}
void FillPath(PLStream *pls) {
    SVG *svgDev;

	char buf[32];
    svgDev = pls->dev;
	if (plsc->patt) {
		define_pattern(pls,buf);
	} 
    fprintf( svgDev->svgFile, "\n<path d=\"");
    for (int i = 0; i < pls->dev_npath; ++i) {
    PLINT* x = pls->dev_pathx[i];
    PLINT* y = pls->dev_pathy[i];
	fprintf( svgDev->svgFile, "M %.2f,%.2f ", (double) x[0] * svgDev->scale, (double) y[0] * svgDev->scale );
    for (int j = 1; j < pls->dev_pathnxy[i]; ++j) {
      switch (x[j]) {
        case -1: //line
          fprintf( svgDev->svgFile, "L %.2f,%.2f ", (double) x[j+1] * svgDev->scale, (double) y[j+1] * svgDev->scale );
          j++;
          break;
        case -2:
         fprintf( svgDev->svgFile, "Q %.2f,%.2f %.2f,%.2f ", (double) x[j+1] * svgDev->scale, (double) y[j+1] * svgDev->scale, (double) x[j+2] * svgDev->scale, (double) y[j+2] * svgDev->scale);
          j+=2;
          break;
        case -3:
         fprintf( svgDev->svgFile, "C %.2f,%.2f %.2f,%.2f %.2f,%.2f ", (double) x[j+1] * svgDev->scale, (double) y[j+1] * svgDev->scale, (double) x[j+2] * svgDev->scale, (double) y[j+2] * svgDev->scale, (double) x[j+3] * svgDev->scale, (double) y[j+3] * svgDev->scale );
          j+=3;
          break;
        default:
          printf("should not happen in FillPolygons(%d), please report!\n",x[j]);
      }
    }
    fprintf( svgDev->svgFile, "Z ");
  }
	fprintf( svgDev->svgFile, "\"\n");
	//this is a (multiple) FILL. Will add a stroke under certain cases, below.
	if (pls->curcolor.a < 0.99 || plsc->patt || plsc->patternFill) {
	    // pls->curcolor.a < 0.99: Two adjacent regions will put non-zero width boundary strokes on top
        // of each other on their common boundary.  Thus, a stroke on the boundary
        // of a filled region is generally a bad idea when the fill is partially
        // opaque because the partial opacity of the two boundary strokes which
        // are on top of each other will mutually interfere and produce a
        // bad-looking result.  On the other hand, for completely opaque fills
        // a boundary stroke is a good idea since if it is of sufficient width
        // it will keep the background from leaking through at the anti-aliased
        // edges of filled regions that have a common boundary with other
        // filled regions.
		svg_attr_value(svgDev, "stroke", "none");
	} else {
		svg_stroke_width(pls);
		svg_stroke_color(pls);
	}
	if (plsc->patt) {
		svg_fill_pattern(pls, buf);
	} else if (plsc->patternFill) {
		svg_fill_pattern(pls, pls->thePatternBase64);
	} else 	{
		svg_fill_color(pls);
	}
	if (pls->patt && pls->dev_eofill)
		svg_attr_value(svgDev, "fill-rule", "evenodd");
	else
		svg_attr_value(svgDev, "fill-rule", "nonzero");

	fprintf( svgDev->svgFile, "/>"); //end of <path
	}


//--------------------------------------------------------------------------
// svg_open ()
//
// Used to open a new XML expression, sets the indent level appropriately
//--------------------------------------------------------------------------

void svg_open( SVG *svgDev, const char *tag )
{
    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "<%s\n", tag );
    svgDev->svgIndent += 2;
}

//--------------------------------------------------------------------------
// svg_open_end ()
//
// Used to end the opening of a new XML expression i.e. add
// the final ">".
//--------------------------------------------------------------------------

void svg_open_end( SVG *svgDev )
{
    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "/>\n" );
    svgDev->svgIndent -= 2;
}

//--------------------------------------------------------------------------
// svg_attr_value ()
//
// Prints two strings to svgFile as a XML attribute value pair
// i.e. foo="bar"
//--------------------------------------------------------------------------

void svg_attr_value( SVG *svgDev, const char *attribute, const char *value )
{
    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "%s=\"%s\"\n", attribute, value );
}

//--------------------------------------------------------------------------
// svg_attr_values ()
//
// Prints a string and a bunch of numbers / strings as a XML attribute
// value pair i.e. foo="0 10 1.0 5.3 bar"
//
// This function is derived from an example in
// "The C Programming Language" by Kernighan and Ritchie.
//
//--------------------------------------------------------------------------

void svg_attr_values( SVG *svgDev, const char *attribute, const char *format, ... )
{
    va_list    ap;
    const char *p, *sval;
    int        ival;
    double     dval;

    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "%s=\"", attribute );
    va_start( ap, format );
    for ( p = format; *p; p++ )
    {
        if ( *p != '%' )
        {
            fprintf( svgDev->svgFile, "%c", *p );
            continue;
        }
        switch ( *++p )
        {
        case 'd':
            ival = va_arg( ap, int );
            fprintf( svgDev->svgFile, "%d", ival );
            break;
        case 'f':
            dval = va_arg( ap, double );
            fprintf( svgDev->svgFile, "%f", dval );
            break;
        case 'r':
            // r is non-standard, but use it here to format rounded value
            dval = va_arg( ap, double );
            fprintf( svgDev->svgFile, "%.2f", dval );
            break;
        case 's':
            sval = va_arg( ap, char * );
            fprintf( svgDev->svgFile, "%s", sval );
            break;
        default:
            fprintf( svgDev->svgFile, "%c", *p );
            break;
        }
    }
    fprintf( svgDev->svgFile, "\"\n" );
    va_end( ap );
}

//--------------------------------------------------------------------------
// svg_close ()
//
// Used to close a XML expression, sets the indent level appropriately
//--------------------------------------------------------------------------

void svg_close( SVG *svgDev, const char *tag )
{
    svgDev->svgIndent -= 2;
    svg_indent( svgDev );
    if ( strlen( tag ) > 0 )
    {
        fprintf( svgDev->svgFile, "</%s>\n", tag );
    }
    else
    {
        fprintf( svgDev->svgFile, "/>\n" );
    }
}

//--------------------------------------------------------------------------
// svg_general ()
//
// Used to print any text into the svgFile
//--------------------------------------------------------------------------

void svg_general( SVG *svgDev, const char *text )
{
    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "%s", text );
}

//--------------------------------------------------------------------------
// svg_indent ()
//
// Indents properly based on the current indent level
//--------------------------------------------------------------------------

void svg_indent( SVG *svgDev )
{
    short i;
    for ( i = 0; i < svgDev->svgIndent; i++ )
    {
        fprintf( svgDev->svgFile, " " );
    }
}

//--------------------------------------------------------------------------
// svg_stroke_width ()
//
// sets the stroke width based on the current width
//--------------------------------------------------------------------------

void svg_stroke_width( PLStream *pls )
{
    SVG *svgDev;

    svgDev = pls->dev;
    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "stroke-width=\"%e\"\n", pls->width );
}

//--------------------------------------------------------------------------
// svg_stroke_color ()
//
// sets the stroke color based on the current color
//--------------------------------------------------------------------------

void svg_stroke_color( PLStream *pls )
{
    SVG *svgDev;

    svgDev = pls->dev;
    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "stroke=\"#" );
    write_hex( svgDev->svgFile, pls->curcolor.r );
    write_hex( svgDev->svgFile, pls->curcolor.g );
    write_hex( svgDev->svgFile, pls->curcolor.b );
    fprintf( svgDev->svgFile, "\"\n" );
    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "stroke-opacity=\"%f\"\n", pls->curcolor.a );
}

void svg_fill_pattern( PLStream *pls , const char *patname)
{
    SVG *svgDev;

    svgDev = pls->dev;
    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "fill=\"url(#%s)\"",patname );
    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "fill-opacity=\"%f\"\n", pls->curcolor.a );
}
//--------------------------------------------------------------------------
// svg_fill_color ()
//
// sets the fill color based on the current color
//--------------------------------------------------------------------------

void svg_fill_color( PLStream *pls )
{
    SVG *svgDev;

    svgDev = pls->dev;
    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "fill=\"#" );
    write_hex( svgDev->svgFile, pls->curcolor.r );
    write_hex( svgDev->svgFile, pls->curcolor.g );
    write_hex( svgDev->svgFile, pls->curcolor.b );
    fprintf( svgDev->svgFile, "\"\n" );
    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "fill-opacity=\"%f\"\n", pls->curcolor.a );
}

//--------------------------------------------------------------------------
// svg_fill_background_color ()
//
// sets the background fill color based on the current background color
//--------------------------------------------------------------------------

void svg_fill_background_color( PLStream *pls )
{
    SVG *svgDev;

    svgDev = pls->dev;
    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "fill=\"#" );
    write_hex( svgDev->svgFile, pls->cmap0[0].r );
    write_hex( svgDev->svgFile, pls->cmap0[0].g );
    write_hex( svgDev->svgFile, pls->cmap0[0].b );
    fprintf( svgDev->svgFile, "\"\n" );
    svg_indent( svgDev );
    fprintf( svgDev->svgFile, "fill-opacity=\"%f\"\n", pls->cmap0[0].a );
}

//--------------------------------------------------------------------------
// write_hex ()
//
// writes a unsigned char as an appropriately formatted hex value
//--------------------------------------------------------------------------

void write_hex( FILE *svgFile, unsigned char val )
{
    if ( val < 16 )
    {
        fprintf( svgFile, "0%X", val );
    }
    else
    {
        fprintf( svgFile, "%X", val );
    }
}


