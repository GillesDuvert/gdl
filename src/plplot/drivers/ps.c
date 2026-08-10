//      PLplot PostScript device driver.
//
// Copyright (C) 1992-2001 Geoffrey Furnish
// Copyright (C) 1992-2001 Maurice LeBrun
// Copyright (C) 2000-2018 Alan W. Irwin
// Copyright (C) 2001-2002 Joao Cardoso
// Copyright (C) 2001-2004 Rafael Laboissiere
// Copyright (C) 2004-2005 Thomas J. Duck
// Copyright (C) 2005 Andrew Ross
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

#include "plDevs.h"

#define DEBUG

#define NEED_PLDEBUG
#include "plplotP.h"
#include "drivers.h"
#include "ps.h"

#include <string.h>
#include <time.h>
#include "plunicode-type1.h"
#include "plfci-type1.h"

// Define macro to truncate small values to zero - prevents
// printf printing -0.000
#define TRMFLT( a )    ( ( fabs( a ) < 5.0e-4 ) ? 0.0 : ( a ) )

// Device info

PLDLLIMPEXP_DRIVER const char* plD_DEVICE_INFO_ps =
#ifdef PLD_ps
    "ps:PostScript File (monochrome):0:ps:29:ps\n"
#endif
#ifdef PLD_psc
    "psc:PostScript File (color):0:ps:30:psc\n"
#endif
;

// Prototypes for functions in this file.

#ifdef PLD_ps
void plD_dispatch_init_ps( PLDispatchTable *pdt );
#endif
#ifdef PLD_psc
void plD_dispatch_init_psc( PLDispatchTable *pdt );
#endif

static char *ps_getdate( void );
static void ps_init( PLStream * );
static void fill_polygon( PLStream *pls );
static void proc_str( PLStream *, EscText * );
static void esc_purge( unsigned char *, unsigned char * );
static void ps_dispatch_init_helper( PLDispatchTable *pdt,
                                     const char *menustr, const char *devnam,
                                     int type, int seq, plD_init_fp init );
#define OUTBUF_LEN    128
static char   outbuf[OUTBUF_LEN];
static int    text = 1;
static int    color;
static int    hrshsym = 1;
static int    epsf = 1;

static DrvOpt ps_options[] = { { "text",    DRV_INT, &text,    "Use Postscript text (text=0|1)"       },
                               { "epsf",    DRV_INT, &epsf,    "EncapsulatedPostScript (epsf=0|1)"    },
                               { "color",   DRV_INT, &color,   "Use color (color=0|1)"                },
                               { "hrshsym", DRV_INT, &hrshsym, "Use Hershey symbol set (hrshsym=0|1)" },
                               { NULL,      DRV_INT, NULL,     NULL                                   } };

static unsigned char
plunicode2type1( const PLUNICODE index,
                 const Unicode_to_Type1_table lookup[],
                 const int number_of_entries );

static const char *
get_font(  PLUNICODE fci );

// text > 0 uses some postscript tricks, namely a transformation matrix
// that scales, rotates (with slanting) and offsets text strings.
// It has yet some bugs for 3d plots.

//We have a special processing, default one does not work (meaning the driver is not well written)
void plD_line_ps_3D( PLStream *, short, short, short, short );
void plD_polyline_ps_3D( PLStream *, short *, short *, PLINT );

#define SPECIFIC_3D
#define LINE3D_FUNCTION plD_line_ps_3D
#define POLYLINE3D_FUNCTION plD_polyline_ps_3D

#define LINE2D plD_line_ps
#define POLYLINE2D plD_polyline_ps
#include "plplot3d.h"

static void ps_dispatch_init_helper( PLDispatchTable *pdt,
                                     const char *menustr, const char *devnam,
                                     int type, int seq, plD_init_fp init )
{
  currDispatchTab = pdt;
  Status3D = 0;

    pdt->pl_MenuStr = (char *) menustr;
    pdt->pl_DevName = (char *) devnam;
    pdt->pl_type     = type;
    pdt->pl_seq      = seq;
    pdt->pl_init     = init;
    pdt->pl_line     = (plD_line_fp) plD_line_ps;
    pdt->pl_polyline = (plD_polyline_fp) plD_polyline_ps;
    pdt->pl_eop      = (plD_eop_fp) plD_eop_ps;
    pdt->pl_bop      = (plD_bop_fp) plD_bop_ps;
    pdt->pl_tidy     = (plD_tidy_fp) plD_tidy_ps;
    pdt->pl_state    = (plD_state_fp) plD_state_ps;
    pdt->pl_esc      = (plD_esc_fp) plD_esc_ps;
}

#ifdef PLD_ps
void plD_dispatch_init_ps( PLDispatchTable *pdt )
{
    ps_dispatch_init_helper( pdt,
        "PostScript File (monochrome)", "ps",
        plDevType_FileOriented, 29,
        (plD_init_fp) plD_init_ps );
}

//--------------------------------------------------------------------------
// plD_init_ps()
//
// Initialize device.
//--------------------------------------------------------------------------

void
plD_init_ps( PLStream *pls )
{
    color      = 0;
    pls->color = 0;             // Not a color device
    
    plParseDrvOpts( ps_options );
    if ( color )
        pls->color = 1;         // But user wants color
    ps_init( pls );
}
#endif //#ifdef PLD_ps

#ifdef PLD_psc
void plD_dispatch_init_psc( PLDispatchTable *pdt )
{
    ps_dispatch_init_helper( pdt,
        "PostScript File (color)", "psc",
        plDevType_FileOriented, 30,
        (plD_init_fp) plD_init_psc );
}

void
plD_init_psc( PLStream *pls )
{
    color      = 1;
    pls->color = 1;             // Is a color device
    plParseDrvOpts( ps_options );

    if ( !color )
        pls->color = 0;         // But user does not want color
    ps_init( pls );
}
#endif //#ifdef PLD_psc

static void
ps_init( PLStream *pls )
{
    PSDev *dev;

    PLFLT pxlx, pxly;

    // Set default values - 7.5 x 10 [inches] (72 points = 1 inch)
    if ( pls->xlength <= 0 || pls->ylength <= 0 )
    {
        pls->xlength = 540;
        pls->ylength = 720;
        pls->xoffset = 32;
        pls->yoffset = 32;
    }
    if ( pls->xdpi <= 0 )
        pls->xdpi = 72.;
    if ( pls->ydpi <= 0 )
        pls->ydpi = 72.;

    pxlx = YPSSIZE / LPAGE_X;
    pxly = XPSSIZE / LPAGE_Y;

    if ( text )
    {
        pls->dev_text    = 1;                // want to draw text
        pls->dev_unicode = 1;                // want unicode
        if ( hrshsym )
            pls->dev_hrshsym = 1;            // want Hershey symbols
    }

    pls->dev_fill0 = 1;         // Can do solid fills

// Initialize family file info

    plFamInit( pls );

// Prompt for a file name if not already set

    plOpenFile( pls );

// Allocate and initialize device-specific data

    if ( pls->dev != NULL )
        free( (void *) pls->dev );

    pls->dev = calloc( 1, (size_t) sizeof ( PSDev ) );
    if ( pls->dev == NULL )
        plexit( "ps_init: Out of memory." );

    dev = (PSDev *) pls->dev;

    dev->xold = PL_UNDEFINED;
    dev->yold = PL_UNDEFINED;

    plP_setpxl( pxlx, pxly );

    dev->llx   = XPSSIZE;
    dev->lly   = YPSSIZE;
    dev->urx   = 0;
    dev->ury   = 0;
    dev->ptcnt = 0;

// Rotate by 90 degrees since portrait mode addressing is used

    dev->xmin = 0;
    dev->ymin = 0;
    dev->xmax = PSY;
    dev->ymax = PSX;
    dev->xlen = dev->xmax - dev->xmin;
    dev->ylen = dev->ymax - dev->ymin;

    plP_setphy( dev->xmin, dev->xmax, dev->ymin, dev->ymax );

// If portrait mode is specified, then set up an additional rotation
// transformation with aspect ratio allowed to adjust via freeaspect.
// Default orientation is landscape (ORIENTATION == 3 or 90 deg rotation
// counter-clockwise from portrait).  (Legacy PLplot used seascape
// which was equivalent to ORIENTATION == 1 or 90 deg clockwise rotation
// from portrait.)

    if ( pls->portrait )
    {
        plsdiori( (PLFLT) ( 4 - ORIENTATION ) );
        pls->freeaspect = 1;
    }

// Header comments into PostScript file

    if (epsf == 1) fprintf( OF, "%%!PS-Adobe-3.0 EPSF-2.0\n" ); else fprintf( OF, "%%!PS-Adobe-3.0\n" );
    fprintf( OF, "%%%%BoundingBox:         \n" );
    fprintf( OF, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n" );

    fprintf( OF, "%%%%Title: PLplot Graph\n" );
    fprintf( OF, "%%%%Creator: PLplot Version %s\n", PLPLOT_VERSION );
    fprintf( OF, "%%%%CreationDate: %s\n", ps_getdate() );
    fprintf( OF, "%%%%Pages: (atend)\n" );
    fprintf( OF, "%%%%EndComments\n%%%%BeginProlog\nsave\n" );

// Definitions
// Save VM state

    fprintf( OF, "/PSSave save def\n" );

// Define a dictionary and start using it

    fprintf( OF, "/PSDict 200 dict def\n" );
    fprintf( OF, "PSDict begin\n" );

    fprintf( OF, "/@restore /restore load def\n" );
    fprintf( OF, "/restore\n" );
    fprintf( OF, "   {vmstatus pop\n" );
    fprintf( OF, "    dup @VMused lt {pop @VMused} if\n" );
    fprintf( OF, "    exch pop exch @restore /@VMused exch def\n" );
    fprintf( OF, "   } def\n" );
    fprintf( OF, "/@pri\n" );
    fprintf( OF, "   {\n" );
    fprintf( OF, "    ( ) print\n" );
    fprintf( OF, "    (                                       ) cvs print\n" );
    fprintf( OF, "   } def\n" );

// n @copies -

    fprintf( OF, "/@copies\n" );
    fprintf( OF, "   {\n" );
    fprintf( OF, "    /#copies exch def\n" );
    fprintf( OF, "   } def\n" );

// - @start -  -- start everything

    fprintf( OF, "/@start\n" );
    fprintf( OF, "   {\n" );
    fprintf( OF, "    vmstatus pop /@VMused exch def pop\n" );
    fprintf( OF, "   } def\n" );

// - @end -  -- finished

    fprintf( OF, "/@end\n" );
    fprintf( OF, "   {flush\n" );
    fprintf( OF, "    end\n" );
    fprintf( OF, "    PSSave restore\n" );
    fprintf( OF, "   } def\n" );

// bop -  -- begin a new page
// Only fill background if we are using color and if the bg isn't white

    fprintf( OF, "/bop\n" );
    fprintf( OF, "   {\n" );
    fprintf( OF, "    /SaveImage save def\n" );
    fprintf( OF, "   } def\n" );

// - eop -  -- end a page

    fprintf( OF, "/eop\n" );
    fprintf( OF, "   {\n" );
    fprintf( OF, "    showpage\n" );
    fprintf( OF, "    SaveImage restore\n" );
    fprintf( OF, "   } def\n" );

// Set line parameters

    fprintf( OF, "/@line\n" );
    fprintf( OF, "   {0 setlinecap\n" );
    fprintf( OF, "    0 setlinejoin\n" );
    fprintf( OF, "    2.5 setmiterlimit\n" );
    fprintf( OF, "   } def\n" );

// d @hsize -  horizontal clipping dimension

    fprintf( OF, "/@hsize   {/hs exch def} def\n" );
    fprintf( OF, "/@vsize   {/vs exch def} def\n" );

// d @hoffset - shift for the plots

    fprintf( OF, "/@hoffset {/ho exch def} def\n" );
    fprintf( OF, "/@voffset {/vo exch def} def\n" );

// Setup user specified offsets, scales, sizes for clipping

    fprintf( OF, "/@SetPlot\n" );
    fprintf( OF, "   {\n" );
    fprintf( OF, "    ho vo translate\n" );
    fprintf( OF, "    XScale YScale scale  \n" );
    fprintf( OF, "   } def\n" );

// Setup x & y scales

    fprintf( OF, "/XScale\n" );
    fprintf( OF, "   {hs %d div} def\n", YPSSIZE );
    fprintf( OF, "/YScale\n" );
    fprintf( OF, "   {vs %d div} def\n", XPSSIZE );

// Macro definitions of common instructions, to keep output small

    fprintf( OF, "/M {moveto} def\n" );
    fprintf( OF, "/D {lineto} def\n" );
    fprintf( OF, "/A {0.5 0 360 arc} def\n" );
    fprintf( OF, "/S {stroke} def\n" );
    fprintf( OF, "/Z {stroke newpath} def\n" );
    // Modify to use fill and stroke for better output with
    // anti-aliasing
    //fprintf(OF, "/F {fill} def\n");
    if ( pls->dev_eofill )
        fprintf( OF, "/F {closepath gsave eofill grestore stroke} def \n" );
    else
        fprintf( OF, "/F {closepath gsave fill grestore stroke} def \n" );
    fprintf( OF, "/N {newpath} def\n" );
    fprintf( OF, "/C {setrgbcolor} def\n" );
    fprintf( OF, "/G {setgray} def\n" );
// try to make linewidth more like IDL
    fprintf( OF, "/W { XScale YScale add 2 div div 2 div setlinewidth} def %% note: IDL scale is fixed to 0.028346 \n" );
    fprintf( OF, "/SF {selectfont} def\n" );
    fprintf( OF, "/R {rotate} def\n" );
    fprintf( OF, "/SW {stringwidth 2 index mul exch 2 index mul exch rmoveto pop} bind def\n" );
    fprintf( OF, "/B {Z %d %d M %d %d D %d %d D %d %d D %d %d closepath} def\n",
        0, 0, 0, PSY, PSX, PSY, PSX, 0, 0, 0 );
    fprintf( OF, "/CL {newpath M D D D closepath clip} def\n" );

// End of dictionary definition

    fprintf( OF, "end\n\n" );

// Set up the plots

    fprintf( OF, "PSDict begin\n" );
    fprintf( OF, "@start\n" );
    fprintf( OF, "%d @copies\n", COPIES );
    fprintf( OF, "%d @hsize\n", YSIZE );
    fprintf( OF, "%d @vsize\n", XSIZE );
    fprintf( OF, "%d @hoffset\n", YOFFSET );
    fprintf( OF, "%d @voffset\n", XOFFSET );
    fprintf( OF, "@line\n" );
    fprintf( OF, "@SetPlot\n%%%%EndProlog\n" );
}

//--------------------------------------------------------------------------
// plD_line_ps()
//
// Draw a line in the current color from (x1,y1) to (x2,y2).
//--------------------------------------------------------------------------

void
plD_line_ps( PLStream *pls, short x1a, short y1a, short x2a, short y2a )
{
   PSDev *dev = (PSDev *) pls->dev;
    PLINT x1   = x1a, y1 = y1a, x2 = x2a, y2 = y2a;

// Rotate by 90 degrees

    plRotPhy( ORIENTATION, dev->xmin, dev->ymin, dev->xmax, dev->ymax, &x1, &y1 );
    plRotPhy( ORIENTATION, dev->xmin, dev->ymin, dev->xmax, dev->ymax, &x2, &y2 );

    if ( x1 == dev->xold && y1 == dev->yold && dev->ptcnt < 40 )
    {
        if ( pls->linepos + 12 > LINELENGTH )
        {
            putc( '\n', OF );
            pls->linepos = 0;
        }
        else
            putc( ' ', OF );

        snprintf( outbuf, OUTBUF_LEN, "%d %d D", x2, y2 );
        dev->ptcnt++;
        pls->linepos += 12;
    }
    else
    {
        fprintf( OF, " Z\n" );
        pls->linepos = 0;

        if ( x1 == x2 && y1 == y2 ) // must be a single dot, draw a circle
            snprintf( outbuf, OUTBUF_LEN, "%d %d A", x1, y1 );
        else
            snprintf( outbuf, OUTBUF_LEN, "%d %d M %d %d D", x1, y1, x2, y2 );
        dev->llx      = MIN( dev->llx, x1 );
        dev->lly      = MIN( dev->lly, y1 );
        dev->urx      = MAX( dev->urx, x1 );
        dev->ury      = MAX( dev->ury, y1 );
        dev->ptcnt    = 1;
        pls->linepos += 24;
    }
    dev->llx = MIN( dev->llx, x2 );
    dev->lly = MIN( dev->lly, y2 );
    dev->urx = MAX( dev->urx, x2 );
    dev->ury = MAX( dev->ury, y2 );

    fprintf( OF, "%s", outbuf );
    pls->bytecnt += 1 + (PLINT) strlen( outbuf );
    dev->xold     = x2;
    dev->yold     = y2;
}
//--------------------------------------------------------------------------
// same as above ut with 3D enabled
//--------------------------------------------------------------------------

//special 3D transform for PORTAIT mode
static void SelfTransform3DPSP(int *xs, int *ys)
{
  if (Status3D == 1) { //enable use everywhere.
    PLFLT x = *xs, y = *ys, z=Data3d.zValue;
    // x and Y are in raw device coordinates.
    // convert to NORM, here X and Y are inverted if PORTRAIT
    //  x = my_plP_pcdcx(x);
    //  y = my_plP_pcdcy(y);
    x = (x - plsc->phyymi) / (double) plsc->phyylen;
    y = (y - plsc->phyxmi) / (double) plsc->phyxlen;
    //here it is !P.T not a c/c++ transposed matrix
    PLFLT xx, yy, ww;
    xx = x * Data3d.T[0] + y * Data3d.T[1] + z * Data3d.T[2] + Data3d.T[3];
    yy = x * Data3d.T[4] + y * Data3d.T[5] + z * Data3d.T[6] + Data3d.T[7];
    ww = x * Data3d.T[12] + y * Data3d.T[13] + z * Data3d.T[14] + Data3d.T[15];
    xx /= ww;
    yy /= ww;
     // convert to device again
    //  *xs = (int) (my_plP_dcpcx(xx));
    //  *ys = (int) (my_plP_dcpcy(yy));
    *xs = (int) (plsc->phyymi + plsc->phyylen * xx);
    *ys = (int) (plsc->phyxmi + plsc->phyxlen * yy);
  }
}
//special 3D transform for LANDCSAPE mode
static void SelfTransform3DPSL(int *xs, int *ys)
{
  if (Status3D == 1) { //enable use everywhere.
    PLFLT x = *xs, y = *ys;
    PLFLT z=(1-Data3d.zValue); //this displacement is needed
    // x and Y are in raw device coordinates.
    // convert to NORM
    //  x = my_plP_pcdcx(x);
    //  y = my_plP_pcdcy(y);
    x = (x - plsc->phyxmi) / (double) plsc->phyxlen;
    y = (y - plsc->phyymi) / (double) plsc->phyylen;
    //here it is !P.T not a c/c++ transposed matrix
    PLFLT xx, yy, ww;
    xx = x * Data3d.T[0] + y * Data3d.T[1] + z * Data3d.T[2] + Data3d.T[3];
    yy = x * Data3d.T[4] + y * Data3d.T[5] + z * Data3d.T[6] + Data3d.T[7];
    ww = x * Data3d.T[12] + y * Data3d.T[13] + z * Data3d.T[14] + Data3d.T[15];
    xx /= ww;
    yy /= ww;
    // convert to device again
    //  *xs = (int) (my_plP_dcpcx(xx));
    //  *ys = (int) (my_plP_dcpcy(yy));
    *xs = (int) (plsc->phyxmi + plsc->phyxlen * xx);
    *ys = (int) (plsc->phyymi + plsc->phyylen * yy);
  }
}
void
plD_line_ps_3D(PLStream *pls, short x1a, short y1a, short x2a, short y2a)
{
  PSDev *dev = (PSDev *) pls->dev;
  PLINT x1 = x1a, y1 = y1a, x2 = x2a, y2 = y2a;

if ( !pls->portrait )
{
    // 3D convert on normalized values
    
    SelfTransform3DPSL(&x1, &y1);
    SelfTransform3DPSL(&x2, &y2);
}
  
  // Rotate by 90 degrees

  plRotPhy(ORIENTATION, dev->xmin, dev->ymin, dev->xmax, dev->ymax, &x1, &y1);
  plRotPhy(ORIENTATION, dev->xmin, dev->ymin, dev->xmax, dev->ymax, &x2, &y2);

if ( pls->portrait )
{
  // 3D convert on normalized values
  SelfTransform3DPSP(&x1, &y1);
  SelfTransform3DPSP(&x2, &y2);
}
  if (x1 == dev->xold && y1 == dev->yold && dev->ptcnt < 40) {
    if (pls->linepos > (LINELENGTH-12)) {
      putc('\n', OF);
      pls->linepos = 0;
    } else
      putc(' ', OF);

    snprintf(outbuf, OUTBUF_LEN, "%d %d D", x2, y2);
    dev->ptcnt++;
    pls->linepos += 12;
  } else {
    fprintf(OF, " Z\n");
    pls->linepos = 0;

    if (x1 == x2 && y1 == y2) // must be a single dot, draw a circle
      snprintf(outbuf, OUTBUF_LEN, "%d %d A", x1, y1);
    else
      snprintf(outbuf, OUTBUF_LEN, "%d %d M %d %d D", x1, y1, x2, y2);
    dev->llx = MIN(dev->llx, x1);
    dev->lly = MIN(dev->lly, y1);
    dev->urx = MAX(dev->urx, x1);
    dev->ury = MAX(dev->ury, y1);
    dev->ptcnt = 1;
    pls->linepos += 24;
  }
  dev->llx = MIN(dev->llx, x2);
  dev->lly = MIN(dev->lly, y2);
  dev->urx = MAX(dev->urx, x2);
  dev->ury = MAX(dev->ury, y2);

  fprintf(OF, "%s", outbuf);
  pls->bytecnt += 1 + (PLINT) strlen(outbuf);
  dev->xold = x2;
  dev->yold = y2;
}
//--------------------------------------------------------------------------
// plD_polyline_ps()
//
// Draw a polyline in the current color.
//--------------------------------------------------------------------------

void
plD_polyline_ps( PLStream *pls, short *xa, short *ya, PLINT npts )
{
   PLINT i;

    for ( i = 0; i < npts - 1; i++ )
        plD_line_ps( pls, xa[i], ya[i], xa[i + 1], ya[i + 1] );
}
//--------------------------------------------------------------------------
// Same as above for 3D
//--------------------------------------------------------------------------

void
plD_polyline_ps_3D(PLStream *pls, short *xa, short *ya, PLINT npts)
{
 PLINT i;

  for (i = 0; i < npts - 1; i++)
    plD_line_ps_3D(pls, xa[i], ya[i], xa[i + 1], ya[i + 1]);
}

//--------------------------------------------------------------------------
// plD_eop_ps()
//
// End of page.
//--------------------------------------------------------------------------

void
plD_eop_ps( PLStream *pls )
{
    fprintf( OF, " S\neop\n" );
}

//--------------------------------------------------------------------------
// plD_bop_ps()
//
// Set up for the next page.
// Advance to next family file if necessary (file output).
//--------------------------------------------------------------------------

void
plD_bop_ps( PLStream *pls )
{
    PSDev *dev = (PSDev *) pls->dev;

    dev->xold = PL_UNDEFINED;
    dev->yold = PL_UNDEFINED;

    if ( !pls->termin )
        plGetFam( pls );

    pls->page++;

    if ( pls->family )
        fprintf( OF, "%%%%Page: %d %d\n", (int) pls->page, 1 );
    else
        fprintf( OF, "%%%%Page: %d %d\n", (int) pls->page, (int) pls->page );

    if ( !pls->portrait ) fprintf( OF, "%%%%PageOrientation: Landscape\n"); else fprintf( OF, "%%%%PageOrientation: Portrait\n");

    fprintf( OF, "bop\n" );
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
}

//--------------------------------------------------------------------------
// plD_tidy_ps()
//
// Close graphics file or otherwise clean up.
//--------------------------------------------------------------------------

void
plD_tidy_ps( PLStream *pls )
{
    PSDev *dev = (PSDev *) pls->dev;

    fprintf( OF, "\n%%%%Trailer\n" );

    dev->llx /= ENLARGE;
    dev->lly /= ENLARGE;
    dev->urx /= ENLARGE;
    dev->ury /= ENLARGE;
    dev->llx += YOFFSET;
    dev->lly += XOFFSET;
    dev->urx += YOFFSET;
    dev->ury += XOFFSET;

// changed for correct Bounding boundaries Jan Thorbecke  okt 1993
// occurs from the integer truncation -- postscript uses fp arithmetic

    dev->urx += 1;
    dev->ury += 1;

    if ( pls->family )
        fprintf( OF, "%%%%Pages: %d\n", (int) 1 );
    else
        fprintf( OF, "%%%%Pages: %d\n", (int) pls->page );

    fprintf( OF, "@end\n" );
    fprintf( OF, "%%%%EOF\n" );

// Backtrack to write the BoundingBox at the beginning
// Some applications don't like it atend

    rewind( OF );
    if (epsf == 1) fprintf( OF, "%%!PS-Adobe-3.0 EPSF-2.0\n" ); else fprintf( OF, "%%!PS-Adobe-3.0\n" );
    fprintf( OF, "%%%%BoundingBox: %d %d %d %d\n",
        dev->llx, dev->lly, dev->urx, dev->ury );
    plCloseFile( pls );
}

//--------------------------------------------------------------------------
// plD_state_ps()
//
// Handle change in PLStream state (color, pen width, fill attribute, etc).
//--------------------------------------------------------------------------

void
plD_state_ps( PLStream *pls, PLINT op )
{
    PSDev *dev = (PSDev *) pls->dev;

    switch ( op )
    {
    case PLSTATE_WIDTH:
  {
    // Set line width
    float width = pls->width*DEF_WIDTH;
    if (width < MIN_WIDTH) width = MIN_WIDTH;
    if (width > MAX_WIDTH) width = MAX_WIDTH;
        fprintf( OF, " S\n%f W", width );

        dev->xold = PL_UNDEFINED;
        dev->yold = PL_UNDEFINED;
        break;
    }
    case PLSTATE_COLOR0:
        if ( !pls->color )
        {
            fprintf( OF, " S\n%.4f G", ( pls->icol0 ? 0.0 : 1.0 ) );
            // Reinitialize current point location.
            if ( dev->xold != PL_UNDEFINED && dev->yold != PL_UNDEFINED )
                fprintf( OF, " %d %d M \n", (int) dev->xold, (int) dev->yold );
            break;
        }
    // else fallthrough
    case PLSTATE_COLOR1:
        if ( pls->color )
        {
            PLFLT r = ( (PLFLT) pls->curcolor.r ) / 255.0;
            PLFLT g = ( (PLFLT) pls->curcolor.g ) / 255.0;
            PLFLT b = ( (PLFLT) pls->curcolor.b ) / 255.0;

            fprintf( OF, " S\n%.4f %.4f %.4f C", r, g, b );
        }
        else
        {
            PLFLT r = ( (PLFLT) pls->curcolor.r ) / 255.0;
            fprintf( OF, " S\n%.4f G", 1.0 - r );
        }
        // Reinitialize current point location.
        if ( dev->xold != PL_UNDEFINED && dev->yold != PL_UNDEFINED )
            fprintf( OF, " %d %d M \n", (int) dev->xold, (int) dev->yold );
        break;
    }
}

//--------------------------------------------------------------------------
// plD_esc_ps()
//
// Escape function.
//--------------------------------------------------------------------------

void
plD_esc_ps(PLStream *pls, PLINT op, void *ptr)
{
    switch ( op )
    {
	case PLESC_FILL:
        fill_polygon(pls);
        break;
      case PLESC_HAS_TEXT:
        proc_str(pls, (EscText *) ptr);
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
// fill_polygon()
//
// Fill polygon described in points pls->dev_x[] and pls->dev_y[].
// Only solid color fill supported.
//--------------------------------------------------------------------------

static void
fill_polygon( PLStream *pls )
{
    PSDev *dev = (PSDev *) pls->dev;
    PLINT n, ix = 0, iy = 0;
    PLINT x, y;

    fprintf( OF, " Z\n" );
    
    for ( n = 0; n < pls->dev_npts; n++ )
    {
        x = pls->dev_x[ix++];
        y = pls->dev_y[iy++];
        
    if ( Status3D == 1 && !pls->portrait ) { // 3D convert on normalized values
          SelfTransform3DPSL(&x, &y);
    }
// Rotate by 90 degrees

        plRotPhy( ORIENTATION, dev->xmin, dev->ymin, dev->xmax, dev->ymax, &x, &y );

    if ( Status3D == 1 && pls->portrait ) { // 3D convert on normalized values
          SelfTransform3DPSP(&x, &y);
    }

// First time through start with a x y moveto

        if ( n == 0 )
        {
            snprintf( outbuf, OUTBUF_LEN, "N %d %d M", x, y );
            dev->llx = MIN( dev->llx, x );
            dev->lly = MIN( dev->lly, y );
            dev->urx = MAX( dev->urx, x );
            dev->ury = MAX( dev->ury, y );
            fprintf( OF, "%s", outbuf );
            pls->bytecnt += (PLINT) strlen( outbuf );
            continue;
        }

        if ( pls->linepos > (LINELENGTH-21) )
        {
            putc( '\n', OF );
            pls->linepos = 0;
        }
        else
            putc( ' ', OF );

        pls->bytecnt++;

        snprintf( outbuf, OUTBUF_LEN, "%d %d D", x, y );
        dev->llx = MIN( dev->llx, x );
        dev->lly = MIN( dev->lly, y );
        dev->urx = MAX( dev->urx, x );
        dev->ury = MAX( dev->ury, y );

        fprintf( OF, "%s", outbuf );
        pls->bytecnt += (PLINT) strlen( outbuf );
        pls->linepos += 21;
    }
    dev->xold = PL_UNDEFINED;
    dev->yold = PL_UNDEFINED;
    fprintf( OF, " F " );
}

//--------------------------------------------------------------------------
// ps_getdate()
//
// Get the date and time
//--------------------------------------------------------------------------

static char *
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


// 0.8 should mimic the offset of first superscript/subscript level
// implemented in plstr (plsym.c) for Hershey fonts.  However, when
// comparing with -dev xwin and -dev xcairo results changing this
// factor to 0.6 appears to offset the centers of the letters
// appropriately while 0.8 gives much poorer agreement with the
// other devices.
# define RISE_FACTOR    0.6

//--------------------------------------------------------------------------
// proc_str()
//
// Prints postscript strings.
// N.B. Now unicode only, no string access!
//
//--------------------------------------------------------------------------

void
proc_str( PLStream *pls, EscText *args ) {
	const PLUNICODE *ucs4 = args->unicode_array;
	const short ucs4Len = (short) args->unicode_array_len;
    PLFLT      *t = args->xform, tt[4];           // Transform matrices
    PLFLT      rotation, shear, stride;              // Rotation angle and shear from the matrix
    PLFLT      ft_ht, offset;                     // Font height and offset
    PLFLT      cs, sn;
    PSDev      *dev = (PSDev *) pls->dev;
    const char *font;
    char       esc;
    // Be generous.  Used to store lots of font changes
  #define PROC_STR_STRING_LENGTH    1000
    static unsigned char string_to_print[PROC_STR_STRING_LENGTH];
    float         font_factor = 1.4f;

    PLFLT         scale = 1.;            // Font scaling and shifting parameters

    int           i = 0;                          // String index

    // check that we got unicode
    if ( ucs4Len == 0 )
    {
        printf( "Non unicode string passed to PS driver, ignoring\n" );
        return;
    }

	int        j, s, f;
	PLUNICODE  fci, fci_save;

	plgesc( &esc );
	plgfci( &fci );
	fci_save = fci;
	font     = get_font(-1);
	ucs4 = args->unicode_array;

	// finish previous polyline

	dev->xold = PL_UNDEFINED;
	dev->yold = PL_UNDEFINED;

	// Determine the font height
	ft_ht = pls->chrht * 72.0 / 25.4; // ft_ht in points, ht is in mm

//	Project3DToPlplotFormMatrix(t);
	// The transform matrix has only rotations and shears; extract them
	plRotationShear( t, &rotation, &shear, &stride );
	printf("rotation=%f, shear=%f, stride=%f ft=%f offset(scale)=%f\n", rotation, shear, stride, ft_ht, offset);
	cs    = cos( -rotation );
	sn    = sin( -rotation );
	tt[0] = t[0] * cs + t[2] * sn;
	tt[1] = t[1] * cs + t[3] * sn;
	tt[2] = -t[0] * sn + t[2] * cs;
	tt[3] = -t[1] * sn + t[3] * cs;

//
	// Reference point conventions:
	//   If base = 0, it is aligned with the center of the text box
	//   If base = 1, it is aligned with the baseline of the text box
	//   If base = 2, it is aligned with the top of the text box
	//
	// Currently plplot only uses base=0
	// Postscript uses base=1
	//
	// We must calculate the difference between the two and apply the offset.
	//

	if ( args->base == 2 )             // not supported by plplot
		offset = ENLARGE * ft_ht / 2.; // half font height
	else if ( args->base == 1 )
		offset = 0.;
	else
		offset = -ENLARGE * ft_ht / 2.;

	// Determine the adjustment for page orientation
        rotation   -= PI / 2. * pls->diorot;
        args->y += (PLINT) ( offset * cos( rotation ) );
        args->x -= (PLINT) ( offset * sin( rotation ) );

if ( ! pls->portrait )
{
// 3D convert on normalized values
SelfTransform3DPSL(&(args->x), &(args->y));
}
	// ps driver is rotated by default
plRotPhy( ORIENTATION, dev->xmin, dev->ymin, dev->xmax, dev->ymax,
		&( args->x ), &( args->y ) );
if ( pls->portrait )
{
// 3D convert on normalized values
SelfTransform3DPSP(&(args->x), &(args->y));
}

	// Correct for the fact ps driver uses landscape by default
       rotation += PI / 2.;

        // Output
        // Set clipping
/*
        clipx[0] = pls->clpxmi;
        clipx[2] = pls->clpxma;
        clipy[0] = pls->clpymi;
        clipy[2] = pls->clpyma;
        clipx[1] = clipx[2];
        clipy[1] = clipy[0];
        clipx[3] = clipx[0];
        clipy[3] = clipy[2];
        difilt( clipx, clipy, 4, &clxmin, &clxmax, &clymin, &clymax );
        plRotPhy( ORIENTATION, dev->xmin, dev->ymin, dev->xmax, dev->ymax,
            &clipx[0], &clipy[0] );
        plRotPhy( ORIENTATION, dev->xmin, dev->ymin, dev->xmax, dev->ymax,
            &clipx[1], &clipy[1] );
        plRotPhy( ORIENTATION, dev->xmin, dev->ymin, dev->xmax, dev->ymax,
            &clipx[2], &clipy[2] );
        plRotPhy( ORIENTATION, dev->xmin, dev->ymin, dev->xmax, dev->ymax,
            &clipx[3], &clipy[3] );
        fprintf( OF, " gsave %d %d %d %d %d %d %d %d CL\n", clipx[0], clipy[0], clipx[1], clipy[1], clipx[2], clipy[2], clipx[3], clipy[3] );
*/

	// move to string reference point
	fprintf( OF, " %d %d M\n", args->x, args->y );

	// Save the current position and set the string rotation
	fprintf( OF, "gsave %.3f R\n", TRMFLT( rotation * 180. / PI ) );

	int ucs4Pos = 0;
	int stringLen=0;
	fprintf( OF, "/%s %.3f SF\n", font , TRMFLT( font_factor * ENLARGE * ft_ht ) );
	while (ucs4Pos < ucs4Len) {
		if (ucs4[ucs4Pos] < PRIVATE_UNICODE_PLANE) // not a font change
		{
			if ((ucs4[ucs4Pos] & 0xffff80) == 0) string_to_print[stringLen] = (unsigned char) ucs4[ucs4Pos]; else string_to_print[stringLen] = 32; // single byte
				if (string_to_print[stringLen] == '(' || string_to_print[stringLen] == ')' || string_to_print[stringLen] == '\\') {
				unsigned char tmp = string_to_print[stringLen];
				string_to_print[stringLen++] = '\\';
				string_to_print[stringLen] = tmp;
			}
			stringLen++;ucs4Pos++;
		} else // a font change
		{
			fci = ucs4[ucs4Pos] - PRIVATE_UNICODE_PLANE;
			//write so far
			if (stringLen > 0) {
				string_to_print[stringLen] = '\0';
				// Apply the scaling and the shear
				fprintf(OF, "/%s [%.3f %.3f %.3f %.3f 0 0] SF\n",
						font,
						TRMFLT(tt[0] * font_factor * ENLARGE * ft_ht * scale),
						TRMFLT(tt[2] * font_factor * ENLARGE * ft_ht * scale),
						TRMFLT(tt[1] * font_factor * ENLARGE * ft_ht * scale),
						TRMFLT(tt[3] * font_factor * ENLARGE * ft_ht * scale));

				// print the string
				fprintf(OF, "(%s) show\n", string_to_print);
				printf("(%s) show\n", string_to_print);

				//????					fprintf( OF, "%.3f (", TRMFLT( -args->just ) );
			}
			font = get_font(fci);
			fprintf(OF, "/%s %.3f SF\n", font, TRMFLT(font_factor * ENLARGE * ft_ht));
			ucs4Pos++;
			stringLen = 0;
			string_to_print[stringLen] = '\0';
		}
	}
	if (stringLen > 0) {
		string_to_print[stringLen]='\0';
		// Apply the scaling and the shear
		fprintf( OF, "/%s [%.3f %.3f %.3f %.3f 0 0] SF\n",
		font,
		TRMFLT( tt[0] * font_factor * ENLARGE * ft_ht * scale ),
		TRMFLT( tt[2] * font_factor * ENLARGE * ft_ht * scale ),
		TRMFLT( tt[1] * font_factor * ENLARGE * ft_ht * scale ),
		TRMFLT( tt[3] * font_factor * ENLARGE * ft_ht * scale ) );

		// print the string
		fprintf( OF, "(%s) show\n", string_to_print );
	}
/*
	// Output string, while escaping the '(', ')' and '\' characters.
	// this string is output for measurement purposes only.
	//
	fprintf( OF, "%.3f (", TRMFLT( -args->just ) );
	while ( string_to_print[i] != '\0' )
	{
		if ( string_to_print[i] == '(' || string_to_print[i] == ')' || string_to_print[i] == '\\' )
			fprintf( OF, "\\%c", string_to_print[i] );
		else
			fprintf( OF, "%c", string_to_print[i] );
		i++;
	}
	fprintf( OF, ") SW\n" );


	// Parse string for PLplot escape sequences and print everything out

	cur_strp = cur_str;
	f        = 0;
	do
	{
		strp = string_to_print;

        if( *cur_strp > PRIVATE_UNICODE_PLANE ) {
			   // get new font
            fci = *cur_strp-PRIVATE_UNICODE_PLANE;
            PSSetFont( fci );

		// copy from current to next token, adding a postscript escape
		// char '\' if necessary
		//
		while ( *cur_strp && *cur_strp != esc )
		{
			if ( *cur_strp == '(' || *cur_strp == ')' || *cur_strp == '\\' )
				*strp++ = '\\';
			*strp++ = *cur_strp++;
		}
		*strp = '\0';

		// Apply the scaling and the shear
		fprintf( OF, "/%s [%.3f %.3f %.3f %.3f 0 0] SF\n",
			font,
			TRMFLT( tt[0] * font_factor * ENLARGE * ft_ht * scale ),
			TRMFLT( tt[2] * font_factor * ENLARGE * ft_ht * scale ),
			TRMFLT( tt[1] * font_factor * ENLARGE * ft_ht * scale ),
			TRMFLT( tt[3] * font_factor * ENLARGE * ft_ht * scale ) );

		// print the string
		fprintf( OF, "(%s) show\n", string_to_print );

	} while ( *cur_strp );
*/

	fprintf( OF, "grestore\n" );
	fprintf( OF, "grestore\n" );

/*
	//
	// keep driver happy -- needed for background and orientation.
	// arghhh! can't calculate it, as I only have the string reference
	// point, not its extent!
	// Still a hack - but at least it takes into account the string
	// length and justification. Character width is assumed to be
	// 0.6 * character height. Add on an extra 1.5 * character height
	// for safety.
	//
	cs = cos( rotation );
	sn = sin( rotation );
	l1 = -i * args->just;
	l2 = i * ( 1. - args->just );
	// Factor of 0.6 is an empirical fudge to convert character
	// height to average character width
	l1 *= 0.6;
	l2 *= 0.6;

	dev->llx = (int) ( MIN( dev->llx, args->x + ( MIN( l1 * cs, l2 * cs ) - 1.5 ) * font_factor * ft_ht * ENLARGE ) );
	dev->lly = (int) ( MIN( dev->lly, args->y + ( MIN( l1 * sn, l2 * sn ) - 1.5 ) * font_factor * ft_ht * ENLARGE ) );
	dev->urx = (int) ( MAX( dev->urx, args->x + ( MAX( l1 * cs, l2 * cs ) + 1.5 ) * font_factor * ft_ht * ENLARGE ) );
	dev->ury = (int) ( MAX( dev->ury, args->y + ( MAX( l1 * sn, l2 * sn ) + 1.5 ) * font_factor * ft_ht * ENLARGE ) );
*/
}

static void
esc_purge( unsigned char *dstr, unsigned char *sstr )
{
    char esc;

    plgesc( &esc );

    while ( *sstr )
    {
        if ( *sstr != esc )
        {
            *dstr++ = *sstr++;
            continue;
        }

        sstr++;
        if ( *sstr == esc )
        {
            *dstr++ = *sstr++;
            continue;
        }

        else
        {
            switch ( *sstr++ )
            {
            case 'f':
                sstr++;
                break; // two chars sequence

            default:
                break; // single char escape
            }
        }
    }
    *dstr = '\0';
}

//--------------------------------------------------------------------------
//  unsigned char plunicode2type1 (const PLUNICODE index,
//       const Unicode_to_Type1_table lookup[], const int number_of_entries)
//
//  Function takes an input unicode index, looks through the lookup
//  table (which must be sorted by PLUNICODE Unicode), then returns the
//  corresponding Type1 code in the lookup table.  If the Unicode index is
//  not present the returned value is 32 (which is normally a blank
//  for Type 1 fonts).
//--------------------------------------------------------------------------

static unsigned char
plunicode2type1( const PLUNICODE index,
                 const Unicode_to_Type1_table lookup[],
                 const int nlookup )
{
    int jlo = -1, jmid, jhi = nlookup;
    while ( jhi - jlo > 1 )
    {
        // Note that although jlo or jhi can be just outside valid
        // range (see initialization above) because of while condition
        // jlo < jmid < jhi and jmid must be in valid range.
        //
        jmid = ( jlo + jhi ) / 2;
        if ( index > lookup[jmid].Unicode )
            jlo = jmid;
        else if ( index < lookup[jmid].Unicode )
            jhi = jmid;
        else
            // We have found it!
            // index == lookup[jmid].Unicode
            //
            return ( lookup[jmid].Type1 );
    }
    // jlo is invalid or it is valid and index > lookup[jlo].Unicode.
    // jhi is invalid or it is valid and index < lookup[jhi].Unicode.
    // All these conditions together imply index cannot be found in lookup.
    // Mark with ' ' (which is normally the index for blank in type 1 fonts).
    //
    return ( ' ' );
}

//--------------------------------------------------------------------------
// get_font( PSDev* dev, PLUNICODE fci )
//
// Sets the Type1 font.
//--------------------------------------------------------------------------

#define CHANGE_ME "Helvetica"
static const char *
get_font( PLUNICODE fci )
{ 
  const char *font;
  switch(fci){
    case 1:
    case 2:
    case 3:font = "Helvetica"; break;
    case 4:font = "Helvetica-Bold"; break;
    case 5:font = "Helvetica-Narrow"; break;
    case 6:font = "Helvetica-Narrow-BoldOblique"; break;
    case 7:font = "Times-Roman"; break;
    case 8:font = "Times-BoldItalic"; break;
    case 9:font = "Symbol";  break;
    case 10:font = "ZapfDingbats"; break;
    case 11:font = "Courier"; break;
    case 12:font = "Courier-Oblique"; break;
    case 13:font = "Palatino"; break;
    case 14:font = "Palatino-Italic"; break;
    case 15:font = "Palatino-Bold"; break;
    case 16:font = "Palatino-BoldItalic"; break;
    case 17:font = "AvantGarde-Book"; break;
    case 18:font = "NewCenturySchlbk"; break;
    case 19:font = "NewCenturySchlbk-Bold"; break;
    case 20:font = CHANGE_ME ; break;
    default:font = "Helvetica";
      break;
  }
    fprintf(stderr, "set_font, fci = 0x%x, font name = %s\n", fci, font );
    return ( font );
}
