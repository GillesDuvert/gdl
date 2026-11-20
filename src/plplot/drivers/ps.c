// November 8, 2006
//
// PLplot driver for PS 1.1 (http://www.w3.org/Graphics/PS/) copy, used for postScript.
// This is a new version hacked from the original 'ps.c' by GD.
// part of the 'ps.c' code is then
// Copyright (C) 1992-2001 Geoffrey Furnish
// Copyright (C) 1992-2001 Maurice LeBrun
// Copyright (C) 2000-2018 Alan W. Irwin
// Copyright (C) 2001-2002 Joao Cardoso
// Copyright (C) 2001-2004 Rafael Laboissiere
// Copyright (C) 2004-2005 Thomas J. Duck
// Copyright (C) 2005 Andrew Ross
// but this code follows the simple and beautiful 'svg.c' structure proposed by
// Copyright (C) 2006 Hazen Babcock
//
// This file is not part of the official PLplot, but the following applies:
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
#include <time.h>
// PLplot header files

#include "plplotP.h"
#include "drivers.h"

// constants

#define LINELENGTH    78
#define COPIES 1
#define PS_Default_X      720
#define PS_Default_Y      540

#define POINTS_PER_INCH    72
#define OF            pls->OutFile
#define MIN_WIDTH     0.  // Minimum pen width in default pen width (dots)
#define MAX_WIDTH     50 // Maximum pen width 
#define DEF_WIDTH     1.  // Default pen width

// A coordinate value that should never occur

#define PL_UNDEFINED    -9999999

// local variables

PLDLLIMPEXP_DRIVER const char* plD_DEVICE_INFO_ps = "ps:PostScript File (monochrome):0:ps:29:ps\n";

static int    already_warned = 0;

typedef struct
{
	int epsf;
    int   which_clip;
    int   canvasXSize;
    int   canvasYSize;
    int   xOffset;
    int   yOffset;
    PLFLT scale;
    int   psIndent;
    FILE  *psFile;
    PLINT xold;
	PLINT yold;
	int llx, lly, urx, ury;
} PSDev;

// Debugging extras

//-----------------------------------------------
// function declarations
// -----------------------------------------------

static void fill_path(PLStream *pls);
static void fill_polygon(PLStream *pls);

// PLplot interface functions

void plD_dispatch_init_ps( PLDispatchTable *pdt );
void plD_init_ps( PLStream * );
void plD_line_ps( PLStream *, short, short, short, short );
void plD_polyline_ps( PLStream *, short *, short *, PLINT );
void plD_eop_ps( PLStream * );
void plD_bop_ps( PLStream * );
void plD_tidy_ps( PLStream * );
void plD_state_ps( PLStream *, PLINT );
void plD_esc_ps( PLStream *, PLINT, void * );

//define LINE2D, POLYLINE2D
#define LINE2D plD_line_ps
#define POLYLINE2D plD_polyline_ps
#include "plplot3d.h"

//--------------------------------------------------------------------------
// dispatch_init_init()
//
// Initialize device dispatch table
//--------------------------------------------------------------------------

void plD_dispatch_init_ps( PLDispatchTable *pdt )
{
  currDispatchTab = pdt;
  Status3D = 0;
    pdt->pl_MenuStr = "Scalable Vector Graphics (PS 1.1)";
    pdt->pl_DevName = "ps";
    pdt->pl_type     = plDevType_FileOriented;
    pdt->pl_seq      = 57;
    pdt->pl_init     = (plD_init_fp) plD_init_ps;
    pdt->pl_line     = (plD_line_fp) plD_line_ps;
    pdt->pl_polyline = (plD_polyline_fp) plD_polyline_ps;
    pdt->pl_eop      = (plD_eop_fp) plD_eop_ps;
    pdt->pl_bop      = (plD_bop_fp) plD_bop_ps;
    pdt->pl_tidy     = (plD_tidy_fp) plD_tidy_ps;
    pdt->pl_state    = (plD_state_fp) plD_state_ps;
    pdt->pl_esc      = (plD_esc_fp) plD_esc_ps;
}
static const char *
ps_getdate( void )
{
    int    len;
    time_t t;
    char   *p;

    t   = time( (time_t *) 0 );
    p   = ctime( &t );
    len = (int) strlen( p );
    *( p + len - 1 ) = '\0';      // zap the newline character
    return p;
}

//--------------------------------------------------------------------------
// ps_init()
//
// Initialize device
//--------------------------------------------------------------------------

void plD_init_ps( PLStream *pls )
{
    PSDev *dev;
	
    pls->termin  = 0;                   // not an interactive device
    pls->color   = 1;                   // a color device by default
    pls->width   = 1;
    pls->verbose = 1;
    pls->bytecnt = 0;
    //pls->debug = 1;
    pls->page         = 0;
    pls->dev_fill0    = 1;      // driver generates solid fills
    pls->dev_fill1    = 0;      // Use PLplot core fallback for pattern fills

    if ( ( pls->OutFile = fopen( pls->FileName, "wb+" ) ) == NULL )
            fprintf( stderr, "Can't open %s.\n", pls->FileName ); //plOpenFile( pls );

	// Allocate and initialize device-specific data

    if ( pls->dev != NULL )
        free( (void *) pls->dev );

    pls->dev = calloc( 1, (size_t) sizeof ( PSDev ) );
    if ( pls->dev == NULL )
        plexit( "plD_init_ps: Out of memory." );

    dev = (PSDev *) pls->dev;

    // Set the bounds for plotting in points (unit of 1/72 of an inch).  Default is PS_Default_X points x PS_Default_Y points unless otherwise specified by plspage or -geometry option.

    if ( pls->xlength <= 0 || pls->ylength <= 0 )
    {
        dev->canvasXSize = PS_Default_X;
        dev->canvasYSize = PS_Default_Y;
    }
    else
    {
        dev->canvasXSize = pls->xlength;
        dev->canvasYSize = pls->ylength;
    }
	dev->xOffset = pls->xoffset;
	dev->yOffset = pls->yoffset;
    // Calculate ratio of (larger) internal PLplot coordinates to external
    // coordinates used for svg file.
    if ( dev->canvasXSize > dev->canvasYSize )
        dev->scale = (PLFLT) dev->canvasXSize / (PLFLT) ( PIXELS_X - 1 );
    else
        dev->scale = (PLFLT) dev->canvasYSize / (PLFLT) PIXELS_Y ;
    plP_setphy( (PLINT) 0, (PLINT) ( dev->canvasXSize / dev->scale ), (PLINT) 0, (PLINT) ( dev->canvasYSize / dev->scale ) ); // Scaled points.

    plP_setpxl( POINTS_PER_INCH / ( 25.4 * dev->scale ), POINTS_PER_INCH / ( 25.4 * dev->scale ) );                                           // Scaled points/mm.

    dev->psFile = pls->OutFile;

    dev->psIndent      = 0;
	// start is not epsf. it will become at end if needed.
    fprintf( OF, "%%!PS-Adobe-3.0\n" );
    fprintf( OF, "%%%%BoundingBox: \n",0, (PLINT) ( dev->canvasXSize / dev->scale ),0,(PLINT) ( dev->canvasYSize / dev->scale ));
    fprintf( OF, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n" );

    fprintf( OF, "%%%%Title: PLplot Graph\n" );
    fprintf( OF, "%%%%Creator: PLplot Version %s\n", PLPLOT_VERSION );
    fprintf( OF, "%%%%CreationDate: %s\n", ps_getdate() );
    fprintf( OF, "%%%%Pages: (atend)\n" );
    fprintf( OF, "%%%%EndComments\n%%%%BeginProlog\nsave\n" );

// Macro definitions of common instructions, to keep output small

    fprintf( OF, "/M {moveto} def\n" );
    fprintf( OF, "/R {rlineto} def\n" );
    fprintf( OF, "/D {lineto} def\n" );
    fprintf( OF, "/A {0.5 0 360 arc} def\n" );
    fprintf( OF, "/S {stroke} def\n" );
    fprintf( OF, "/Z {newpath} def\n" );
	fprintf( OF, "/E {closepath gsave eofill  0.1 setlinewidth stroke grestore} def \n" );
	fprintf( OF, "/F {closepath gsave fill 0.1 setlinewidth stroke grestore } def \n" );
	fprintf( OF, "/N {newpath} def\n" );
    fprintf( OF, "/C {setrgbcolor} def\n" );
    fprintf( OF, "/H {rcurveto} def\n" );
    fprintf( OF, "/G {setgray} def\n" );
// try to make linewidth more like IDL
    fprintf( OF, "/W { %f div 1.414 div setlinewidth} def %% note: IDL scale is fixed to 0.028346 \n", dev->scale);
    fprintf( OF, "/RO {rotate} def\n" );
    fprintf( OF, "/SW {stringwidth 2 index mul exch 2 index mul exch rmoveto pop} bind def\n" );
    fprintf( OF, "/B {Z %d %d M %d %d D %d %d D %d %d D %d %d closepath} def\n",
        0, 0, 0, dev->canvasYSize, dev->canvasXSize, dev->canvasYSize, dev->canvasXSize, 0, 0, 0 );
    fprintf( OF, "/CL {newpath M D D D closepath clip} def\n" );
    fprintf( OF, "0 setlinecap 0 setlinejoin 2.5 setmiterlimit\n");

	fprintf( OF, "%%%%EndProlog\n" );
//	if (pls->portrait) fprintf( OF, "%d %d translate %f dup scale\n",dev->xOffset, dev->yOffset, dev->scale);
	if (pls->portrait) fprintf( OF, "%f dup scale\n",dev->scale);
// we should do this :	else fprintf( OF, "%d 0 translate %f dup scale 270 rotate %d 0 translate\n",dev->xOffset, dev->scale, -PIXELS_X);
// but IDL does this: !!!
	else fprintf( OF, "0 29.7 2 div %f div translate %f dup scale 270 rotate\n",dev->scale, dev->scale); //IDL just translates by 21cm
	fprintf( OF, "gsave\n");
}

//--------------------------------------------------------------------------
// ps_bop()
//
// Set up for the next page.
//--------------------------------------------------------------------------

void plD_bop_ps( PLStream *pls )
{
     PSDev *dev = (PSDev *) pls->dev;

    dev->xold = PL_UNDEFINED;
    dev->yold = PL_UNDEFINED;

    pls->page++;


    // write opening ps tag for the new page
    fprintf( OF, "%%%%Page: %d %d\n", (int) pls->page, (int) pls->page );

    if ( pls->portrait ) fprintf( OF, "%%%%PageOrientation: Portrait\n"); else fprintf( OF, "%%%%PageOrientation: Landscape\n");

    fprintf( OF, "gsave\n" );
    
	if ( pls->color )
    {
        PLFLT r, g, b;
        if ( pls->cmap0[0].r != 0xFF ||
             pls->cmap0[0].g != 0xFF ||
             pls->cmap0[0].b != 0xFF )
        {
            r = ( (PLFLT) pls->cmap0[0].r ) / 255.;
            g = ( (PLFLT) pls->cmap0[0].g ) / 255.;
            b = ( (PLFLT) pls->cmap0[0].b ) / 255.;

            fprintf( OF, "B %.4f %.4f %.4f C F\n", r, g, b );
        }
    }
    pls->linepos = 0;

// This ensures the color and line width are set correctly at the beginning of
// each page

    plD_state_ps( pls, PLSTATE_COLOR0 );
    plD_state_ps( pls, PLSTATE_WIDTH );

/*
    // invert the coordinate system so that PLplot graphs appear right side up

    ps_open( dev, "g" );
    ps_attr_values( dev, "transform", "matrix(1 0 0 -1 0 %d)", dev->canvasYSize );
    ps_general( dev, ">\n" );
*/
}

//--------------------------------------------------------------------------
// ps_line()
//
// Draw a line in the current color from (x1,y1) to (x2,y2).
//--------------------------------------------------------------------------

void plD_line_ps( PLStream *pls, short x1a, short y1a, short x2a, short y2a )
{
    PSDev *dev = (PSDev *) pls->dev;
	PLINT x1 = x1a, y1 = y1a, x2 = x2a, y2 = y2a;
	if (pls->linepos > (LINELENGTH)) {
		fputc('\n', OF);
		pls->linepos = 0;
	} 
	if (x1 == x2 && y1 == y2) // must be a single dot, draw a circle
		pls->linepos +=fprintf(OF, "%d %d A S ", x1, y1);
	else
		pls->linepos +=fprintf(OF, "%d %d M %d %d D S ", x1, y1, x2, y2);
	dev->llx = MIN(dev->llx, x1);
	dev->lly = MIN(dev->lly, y1);
	dev->urx = MAX(dev->urx, x1);
	dev->ury = MAX(dev->ury, y1);
	dev->llx = MIN(dev->llx, x2);
	dev->lly = MIN(dev->lly, y2);
	dev->urx = MAX(dev->urx, x2);
	dev->ury = MAX(dev->ury, y2);
	dev->xold = PL_UNDEFINED;
	dev->yold = PL_UNDEFINED;
	if (pls->linepos > (LINELENGTH)) {
		fputc('\n', OF);
		pls->linepos = 0;
	}
}

//--------------------------------------------------------------------------
// ps_polyline()
//
// Draw a polyline in the current color.
//--------------------------------------------------------------------------

void plD_polyline_ps( PLStream *pls, short *xa, short *ya, PLINT npts )
{
	PLINT i;
	PSDev * dev = (PSDev *) pls->dev;
	PLINT x = xa[0], y = ya[0];

	if (pls->linepos > LINELENGTH) {
		fputc('\n', OF);
		pls->linepos = 0;
	}

	pls->linepos +=fprintf(OF, "%d %d M ", x, y);
	dev->llx = MIN(dev->llx, x);
	dev->lly = MIN(dev->lly, y);
	dev->urx = MAX(dev->urx, x);
	dev->ury = MAX(dev->ury, y);
	dev->xold = x;
	dev->yold = y;

	for (i = 1; i < npts ; i++) {
		x = xa[i];
		y = ya[i];
		pls->linepos +=fprintf(OF, "%i %i R ", x - dev->xold, y - dev->yold);
		dev->llx = MIN(dev->llx, x);
		dev->lly = MIN(dev->lly, y);
		dev->urx = MAX(dev->urx, x);
		dev->ury = MAX(dev->ury, y);
		dev->xold = x;
		dev->yold = y;
		if (pls->linepos > (LINELENGTH)) {
			pls->linepos +=fputc('\n', OF);
			pls->linepos = 0;
		}
	}
	dev->xold = PL_UNDEFINED;
	dev->yold = PL_UNDEFINED;	
	pls->linepos +=fprintf(OF, "S ");
}

//--------------------------------------------------------------------------
// ps_eop()
//
// End of page
//--------------------------------------------------------------------------

void plD_eop_ps( PLStream *pls )
{
   fprintf( OF, "\nshowpage grestore\n" );
}

//--------------------------------------------------------------------------
// ps_tidy()
//
// Close graphics file or otherwise clean up.
//--------------------------------------------------------------------------

void plD_tidy_ps( PLStream *pls )
{
    PSDev *dev = (PSDev *) pls->dev;

    fprintf( OF, "\n%%%%Trailer\n" );

    dev->llx *= dev->scale;
    dev->lly *= dev->scale;
    dev->urx *= dev->scale;
    dev->ury *= dev->scale;
/*
    dev->llx += dev->xOffset;
    dev->lly += dev->yOffset;
    dev->urx += dev->xOffset;
    dev->ury += dev->yOffset;
*/

// changed for correct Bounding boundaries Jan Thorbecke  okt 1993
// occurs from the integer truncation -- postscript uses fp arithmetic

    dev->urx += 1;
    dev->ury += 1;

    fprintf( OF, "%%%%Pages: %d\n", (int) pls->page );

    fprintf( OF, "end\n" );
    fprintf( OF, "%%%%EOF\n" );

	// Backtrack to write the BoundingBox at the beginning
// Some applications don't like it atend
    rewind( OF );
    if (dev->epsf == 1) fprintf( OF, "%%!PS-Adobe-3.0 EPSF-2.0\n" ); else fprintf( OF, "%%!PS-Adobe-3.0\n" );
    if (dev->epsf == 1) {
		if ( pls->portrait ) fprintf( OF, "%%%%BoundingBox: %d %d %d %d\n",dev->llx, dev->lly, dev->urx, dev->ury );
		else {
			fprintf( OF, "%%%%BoundingBox: %d %d %d %d\n",dev->llx, dev->lly, dev->ury, dev->urx );
		}
	} else {
		if ( pls->portrait ) fprintf( OF, "%%%%BoundingBox: %d %d %d %d\n",pls->wpxoff,pls->wpyoff,pls->xlength, pls->ylength );
	    else  fprintf( OF, "%%%%BoundingBox: %d %d %d %d\n",pls->wpyoff,pls->wpxoff,pls->ylength, pls->xlength );
	}
    plCloseFile( pls );

}

//--------------------------------------------------------------------------
// plD_state_ps()
//
// Handle change in PLStream state (color, pen width, fill attribute, etc).
//
//--------------------------------------------------------------------------

void plD_state_ps(PLStream *pls, PLINT op) {
	PSDev *dev = (PSDev *) pls->dev;

	switch (op) {
		case PLSTATE_WIDTH:
		{
			// Set line width
			float width = pls->width*DEF_WIDTH;
			if (width < MIN_WIDTH) width = MIN_WIDTH;
			if (width > MAX_WIDTH) width = MAX_WIDTH;
			pls->linepos += fprintf(OF, " %f W ", width);

			dev->xold = PL_UNDEFINED;
			dev->yold = PL_UNDEFINED;
			break;
		}
		case PLSTATE_COLOR0:
			if (!pls->color) {
				pls->linepos += fprintf(OF, " %.4f G ", (pls->icol0 ? 0.0 : 1.0));
				// Reinitialize current point location.
				if (dev->xold != PL_UNDEFINED && dev->yold != PL_UNDEFINED)
					pls->linepos += fprintf(OF, " %d %d M \n", (int) dev->xold, (int) dev->yold);
				break;
			}
			// else fallthrough
		case PLSTATE_COLOR1:
			if (pls->color) {
				PLFLT r = ((PLFLT) pls->curcolor.r) / 255.0;
				PLFLT g = ((PLFLT) pls->curcolor.g) / 255.0;
				PLFLT b = ((PLFLT) pls->curcolor.b) / 255.0;

				pls->linepos += fprintf(OF, " %.4f %.4f %.4f C ", r, g, b);
			} else {
				PLFLT r = ((PLFLT) pls->curcolor.r) / 255.0;
				pls->linepos += fprintf(OF, " %.4f G ", 1.0 - r);
			}
			// Reinitialize current point location.
			if (dev->xold != PL_UNDEFINED && dev->yold != PL_UNDEFINED)
				pls->linepos += fprintf(OF, " %d %d M \n", (int) dev->xold, (int) dev->yold);
			break;
	}
}

//--------------------------------------------------------------------------
// ps_esc()
//
// Escape function.
//--------------------------------------------------------------------------

void plD_esc_ps( PLStream *pls, PLINT op, void *ptr )
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
		fill_polygon(pls);
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
//--------------------------------------------------------------------------
// FillPath()
//
// see fill_path
//--------------------------------------------------------------------------

void fill_path(PLStream *pls) {
	PSDev *dev = (PSDev *) pls->dev;
	pls->linepos = 0;
	fprintf(OF, " Z\n"); //stroke newpath
	PLINT x, y;
	PLINT x1, y1;
	PLINT x2, y2;
	for (int i = 0; i < pls->dev_npath; ++i) {
		PLINT* xx = pls->dev_pathx[i];
		PLINT* yy = pls->dev_pathy[i];
		x = xx[0];
		y = yy[0];

		// start path with a x y moveto

		pls->linepos += fprintf(OF, "%d %d M ", x, y); //xy moveto : define new path
		dev->xold = x;
		dev->yold = y;
		dev->llx = MIN(dev->llx, x);
		dev->lly = MIN(dev->lly, y);
		dev->urx = MAX(dev->urx, x);
		dev->ury = MAX(dev->ury, y);

		if (pls->linepos > LINELENGTH) {
			fputc('\n', OF);
			pls->linepos = 0;
		}
		for (int j = 1; j < pls->dev_pathnxy[i]; ++j) {
			switch (xx[j]) {
				case -1: //line
					x = xx[j + 1];
					y = yy[j + 1];
					pls->linepos += fprintf(OF, "%d %d R ", x - dev->xold, y - dev->yold); //x y lineto
					dev->llx = MIN(dev->llx, x);
					dev->lly = MIN(dev->lly, y);
					dev->urx = MAX(dev->urx, x);
					dev->ury = MAX(dev->ury, y);
					dev->xold = x;
					dev->yold = y;
					j++;
					break;
				case -2:
					x = xx[j + 1];
					y = yy[j + 1];
					x1 = xx[j + 2];
					y1 = yy[j + 2];
					pls->linepos += fprintf(OF, "%d %d %d %d %d %d H ", x - dev->xold, y - dev->yold, x1 - dev->xold, y1 - dev->yold, x1 - dev->xold, y1 - dev->yold); //x1 y1 x2 y2 x3 y3 curveto
					dev->llx = MIN(dev->llx, x);
					dev->lly = MIN(dev->lly, y);
					dev->urx = MAX(dev->urx, x);
					dev->ury = MAX(dev->ury, y);
					dev->llx = MIN(dev->llx, x1);
					dev->lly = MIN(dev->lly, y1);
					dev->urx = MAX(dev->urx, x1);
					dev->ury = MAX(dev->ury, y1);
					j += 2;
					dev->xold = x1;
					dev->yold = y1;
					break;
				case -3:
					x = xx[j + 1];
					y = yy[j + 1];
					x1 = xx[j + 2];
					y1 = yy[j + 2];
					x2 = xx[j + 3];
					y2 = yy[j + 3];
					pls->linepos += fprintf(OF, "%d %d %d %d %d %d H ", x - dev->xold, y - dev->yold, x1 - dev->xold, y1 - dev->yold, x2 - dev->xold, y2 - dev->yold); //x1 y1 x2 y2 x3 y3 curveto
					dev->llx = MIN(dev->llx, x);
					dev->lly = MIN(dev->lly, y);
					dev->urx = MAX(dev->urx, x);
					dev->ury = MAX(dev->ury, y);
					dev->llx = MIN(dev->llx, x1);
					dev->lly = MIN(dev->lly, y1);
					dev->urx = MAX(dev->urx, x1);
					dev->ury = MAX(dev->ury, y1);
					dev->llx = MIN(dev->llx, x2);
					dev->lly = MIN(dev->lly, y2);
					dev->urx = MAX(dev->urx, x2);
					dev->ury = MAX(dev->ury, y2);
					j += 3;
					dev->xold = x2;
					dev->yold = y2;
					break;
					break;
				default:
					printf("should not happen in FillPolygons(%d), please report!\n", xx[j]);
			}
			if (pls->linepos > LINELENGTH) {
				fputc('\n', OF);
				pls->linepos = 0;
			}
		}
	}
	dev->xold = PL_UNDEFINED;
	dev->yold = PL_UNDEFINED;
	if (pls->dev_eofill)
		pls->linepos += fprintf(OF, " E ");
	else
		pls->linepos += fprintf(OF, " F ");
	if (pls->linepos > LINELENGTH) {
		putc('\n', OF);
		pls->linepos = 0;
	}
}

//--------------------------------------------------------------------------
// fill_polygon()
//
// Fill polygon described in points pls->dev_x[] and pls->dev_y[].
// Only solid color fill supported.
//--------------------------------------------------------------------------

static void
fill_polygon(PLStream *pls) {
	PSDev *dev = (PSDev *) pls->dev;
	PLINT n, ix, iy;
	PLINT x, y;

	pls->linepos += fprintf(OF, " Z "); //newpath
	// First time through start with a x y moveto
	x = pls->dev_x[0];
	y = pls->dev_y[0];
	pls->linepos += fprintf(OF, "N %d %d M ", x, y);
	dev->llx = MIN(dev->llx, x);
	dev->lly = MIN(dev->lly, y);
	dev->urx = MAX(dev->urx, x);
	dev->ury = MAX(dev->ury, y);
	dev->xold = x;
	dev->yold = y;

	for (n = 1, ix = 1, iy = 1; n < pls->dev_npts; n++) {
		if (pls->linepos > LINELENGTH) {
			fputc('\n', OF);
			pls->linepos = 0;
		}
		x = pls->dev_x[ix++];
		y = pls->dev_y[iy++];
		pls->linepos += fprintf(OF, "%i %i R ", x - dev->xold, y - dev->yold);
		dev->llx = MIN(dev->llx, x);
		dev->lly = MIN(dev->lly, y);
		dev->urx = MAX(dev->urx, x);
		dev->ury = MAX(dev->ury, y);
		dev->xold = x;
		dev->yold = y;
	}
	dev->xold = PL_UNDEFINED;
	dev->yold = PL_UNDEFINED;
	pls->linepos += fprintf(OF, " F ");
	if (pls->linepos > LINELENGTH) {
		putc('\n', OF);
		pls->linepos = 0;
	}
}
