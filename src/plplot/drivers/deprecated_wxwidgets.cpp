// Copyright (C) 2005  Werner Smekal, Sjaak Verdoold
// Copyright (C) 2005  Germain Carrera Corraleche
// Copyright (C) 1999  Frank Huebner
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

// TODO:
// - NA
//

// wxwidgets headers
#include <wx/wx.h>
#include <wx/wfstream.h>
#include <wx/except.h>

#include "plDevs.h"

// plplot headers
#include "plplotP.h"
#include "drivers.h"

// C/C++ headers
#include <cstdio>

#include "deprecated_wxwidgets.h"

//define LINE2D, POLYLINE2D
#define LINE2D plD_line_wxwidgets
#define POLYLINE2D plD_polyline_wxwidgets
#include "plplot3d.h"

// private functions needed by the wxwidgets Driver
//static void install_buffer( PLStream *pls );
//static void wxRunApp( PLStream *pls, bool runonce = false );
//static void GetCursorCmd( PLStream *pls, PLGraphicsIn *ptr );
static void fill_polygon( PLStream *pls );
static void fill_multiple_polygon( PLStream *pls );

#ifdef __WXMAC__
        #include <Carbon/Carbon.h>
extern "C" { void CPSEnableForegroundOperation( ProcessSerialNumber* psn ); }
#endif

//DECLARE_PLAPP( wxPLplotApp )

//--------------------------------------------------------------------------
//  void Log_Verbose( const char *fmt, ... )
//
//  Print verbose debug message to stderr (printf style).
//--------------------------------------------------------------------------
void Log_Verbose( const char *fmt, ... )
{
#ifdef _DEBUG_VERBOSE
    va_list args;
    va_start( args, fmt );
    fprintf( stderr, "Verbose: " );
    vfprintf( stderr, fmt, args );
    fprintf( stderr, "\n" );
    va_end( args );
    fflush( stderr );
#else
    (void) fmt;        // Cast to void to silence compiler warnings about unused paraemeter
#endif
}


//--------------------------------------------------------------------------
//  void Log_Debug( const char *fmt, ... )
//
//  Print debug message to stderr (printf style).
//--------------------------------------------------------------------------
void Log_Debug( const char *fmt, ... )
{
#ifdef _DEBUG
    va_list args;
    va_start( args, fmt );
    fprintf( stderr, "Debug: " );
    vfprintf( stderr, fmt, args );
    fprintf( stderr, "\n" );
    va_end( args );
    fflush( stderr );
#else
    (void) fmt;        // Cast to void to silence compiler warnings about unused paraemeter
#endif
}


//--------------------------------------------------------------------------
//  In the following you'll find the driver functions which are
//  are needed by the plplot core.
//--------------------------------------------------------------------------

// Device info
#ifdef __cplusplus
extern "C" {
#endif

PLDLLIMPEXP_DRIVER const char* plD_DEVICE_INFO_wxwidgets =
#ifdef PLD_wxwidgets
    "wxwidgets:wxWidgets Driver:1:wxwidgets:51:wxwidgets\n"
#endif
#ifdef PLD_wxpng
    "wxpng:wxWidgets PNG Driver:0:wxwidgets:52:wxpng\n"
#endif
;

#ifdef __cplusplus
}
#endif


//--------------------------------------------------------------------------
//  wxPLDevBase::wxPLDevBase( void )
//
//  Contructor of base class of wxPLDev classes.
//--------------------------------------------------------------------------
wxPLDevBase::wxPLDevBase( int bcknd ) : backend( bcknd )
{
    // Log_Verbose( "wxPLDevBase::wxPLDevBase()" );

    ready    = false;
//    ownGUI   = false;
//    waiting  = false;
//    resizing = false;
//    exit     = false;
//
//    comcount = 0;

    m_frame = NULL;
    xpos    = 0;
    ypos    = 0;
    // width, height are set in plD_init_wxwidgets
    // bm_width, bm_height are set in install_buffer

    // xmin, xmax, ymin, ymax are set in plD_init_wxwidgets
    // scalex, scaley are set in plD_init_wxwidgets

    plstate_width  = false;
    plstate_color0 = false;
    plstate_color1 = false;

    locate_mode = 0;
    draw_xhair  = false;

    newclipregion = true;
    clipminx      = 1024;
    clipmaxx      = 0;
    clipminy      = 800;
    clipmaxy      = 0;

    devName = (const char **) malloc( NDEV * sizeof ( char** ) );
    memset( devName, '\0', NDEV * sizeof ( char** ) );
    devDesc = (const char **) malloc( NDEV * sizeof ( char** ) );
    memset( devDesc, '\0', NDEV * sizeof ( char** ) );
    ndev = NDEV;

    lineSpacing = 1.0;
}


wxPLDevBase::~wxPLDevBase( void )
{
    if ( devDesc )
        free( devDesc );
    if ( devName )
        free( devName );
}


void wxPLDevBase::AddtoClipRegion( int x1, int y1, int x2, int y2 )
{
    newclipregion = false;
    if ( x1 < x2 )
    {
        if ( x1 < clipminx )
            clipminx = x1;
        if ( x2 > clipmaxx )
            clipmaxx = x2;
    }
    else
    {
        if ( x2 < clipminx )
            clipminx = x2;
        if ( x1 > clipmaxx )
            clipmaxx = x1;
    }
    if ( y1 < y2 )
    {
        if ( y1 < clipminy )
            clipminy = y1;
        if ( y2 > clipmaxy )
            clipmaxy = y2;
    }
    else
    {
        if ( y2 < clipminy )
            clipminy = y2;
        if ( y1 > clipmaxy )
            clipmaxy = y1;
    }
}


void wxPLDevBase::PSDrawText( PLUNICODE* ucs4, int ucs4Len, bool drawText )
{
    int  i = 0;

    unsigned char utf8_string[ucs4Len*4];
    memset( utf8_string, '\0', ucs4Len*4 );

    // Get PLplot escape character
    char plplotEsc;
    plgesc( &plplotEsc );

    //Reset the size metrics
    textWidth         = 0;
    textHeight        = 0;
    superscriptHeight = 0;
    subscriptDepth    = 0;

    int l=0;
    int pos=0;
    while ( i < ucs4Len )
    {
        if ( ucs4[i] < PRIVATE_UNICODE_PLANE )                // not a font change
        {
            l=ucs4_to_utf8(&utf8_string[pos], ucs4[i]);  pos+=l;
            i++;
        }
        else // a font change
        {
            // draw string so far
            PSDrawTextToDC( utf8_string, drawText ); pos=0;

            // get new font
            fci = ucs4[i]-PRIVATE_UNICODE_PLANE;
            PSSetFont( fci );
            i++;
        }
    }

    PSDrawTextToDC( utf8_string, drawText );pos=0;
    plsc->string_length=textWidth; textWidth=0;
}


//--------------------------------------------------------------------------
//  void common_init(  PLStream *pls )
//
//  Basic initialization for all devices.
//--------------------------------------------------------------------------
wxPLDevBase* common_init( PLStream *pls) {
  // Log_Verbose( "common_init()" );

  wxPLDevBase* dev;
  PLFLT downscale, downscale2;

  // default options
  static PLINT text = -1;
  static PLINT hrshsym = 0;

  // we use wxGraphicsContext
  static PLINT backend = wxBACKEND_GC;

  DrvOpt wx_options[] = {
    { "hrshsym", DRV_INT, &hrshsym, "Use Hershey symbol set (hrshsym=0|1)"},
    { "text", DRV_INT, &text, "Use own text routines (text=0|1)"},
    { NULL, DRV_INT, NULL, NULL}
  };

  // Check for and set up driver options
  plParseDrvOpts(wx_options);

  // allocate memory for the device storage
#if wxUSE_GRAPHICS_CONTEXT
  dev = new wxPLDevGC;
#else
  dev = new wxPLDevDC; printf("!!!!!!!!!!!!!! USING Device Context!\n");
#endif
  // by default the own text routines are used for wxGC
  if (text == -1)
    text = 1;
  if (dev == NULL) {
    plexit("Insufficient memory");
  }
  pls->dev = (void *) dev;

  // be verbose and write out debug messages
#ifdef _DEBUG
  pls->verbose = 1;
  pls->debug = 1;
#endif

  pls->color = 1; // Is a color device
  pls->dev_flush = 1; // Handles flushes
  pls->dev_fill0 = 1; // Can handle solid fills
  pls->dev_fill1 = 0; // Can't handle pattern fills
  pls->dev_dash = 0;
  pls->dev_clear = 1; // driver supports clear

  if (text) {
    pls->dev_text = 1; // want to draw text
    pls->dev_unicode = 1; // want unicode
    if (hrshsym)
      pls->dev_hrshsym = 1;
  }

  // initialize frame size and position
  if (pls->xlength <= 0 || pls->ylength <= 0)
    plspage(0.0, 0.0, (PLINT) (CANVAS_WIDTH * DEVICE_PIXELS_PER_IN),
      (PLINT) (CANVAS_HEIGHT * DEVICE_PIXELS_PER_IN), 0, 0);

  dev->width = pls->xlength;
  dev->height = pls->ylength;
  dev->clipminx = pls->xlength;
  dev->clipminy = pls->ylength;

  if (pls->xoffset != 0 || pls->yoffset != 0) {
    dev->xpos = (int) (pls->xoffset);
    dev->ypos = (int) (pls->yoffset);
  }


  // If portrait mode, apply a rotation and set freeaspect
  if (pls->portrait) {
    plsdiori((PLFLT) (4 - ORIENTATION));
    pls->freeaspect = 1;
  }

  // Set the number of pixels per mm
  plP_setpxl((PLFLT) VIRTUAL_PIXELS_PER_MM, (PLFLT) VIRTUAL_PIXELS_PER_MM);

  // Set up physical limits of plotting device (in drawing units)
  downscale = (double) dev->width / (double) (PIXELS_X - 1);
  downscale2 = (double) dev->height / (double) PIXELS_Y;
  if (downscale < downscale2)
    downscale = downscale2;
  plP_setphy((PLINT) 0, (PLINT) (dev->width / downscale),
      (PLINT) 0, (PLINT) (dev->height / downscale));

  // get physical device limits coordinates
  plP_gphy(&dev->xmin, &dev->xmax, &dev->ymin, &dev->ymax);

  // setting scale factors
  dev->scalex = (PLFLT) (dev->xmax - dev->xmin) / (dev->width);
  dev->scaley = (PLFLT) (dev->ymax - dev->ymin) / (dev->height);
   
  // set dpi
  plspage(VIRTUAL_PIXELS_PER_IN / dev->scalex, VIRTUAL_PIXELS_PER_IN / dev->scaley, 0, 0, 0, 0);

  // find out what file drivers are available
  plgFileDevs(&dev->devDesc, &dev->devName, &dev->ndev);

  return dev;
}


#ifdef PLD_wxwidgets

//--------------------------------------------------------------------------
//  void plD_dispatch_init_wxwidgets( PLDispatchTable *pdt )
//
//  Make wxwidgets driver functions known to plplot.
//--------------------------------------------------------------------------
void plD_dispatch_init_wxwidgets( PLDispatchTable *pdt )
{
    currDispatchTab=pdt;
    Status3D=0;
    pdt->pl_MenuStr = "wxWidgets DC";
    pdt->pl_DevName = "wxwidgets";
    pdt->pl_type     = plDevType_Interactive;
    pdt->pl_seq      = 51;
    pdt->pl_init     = (plD_init_fp) plD_init_wxwidgets;
    pdt->pl_line     = (plD_line_fp) plD_line_wxwidgets;
    pdt->pl_polyline = (plD_polyline_fp) plD_polyline_wxwidgets;
    pdt->pl_eop      = (plD_eop_fp) plD_eop_wxwidgets;
    pdt->pl_bop      = (plD_bop_fp) plD_bop_wxwidgets;
    pdt->pl_tidy     = (plD_tidy_fp) plD_tidy_wxwidgets;
    pdt->pl_state    = (plD_state_fp) plD_state_wxwidgets;
    pdt->pl_esc      = (plD_esc_fp) plD_esc_wxwidgets;
}

//--------------------------------------------------------------------------
//  plD_init_wxwidgets( PLStream* pls )
//
//  Initialize wxWidgets device.
//--------------------------------------------------------------------------
void plD_init_wxwidgets( PLStream* pls )
{
    // Log_Verbose( "plD_init_wxwidgets()" );

    wxPLDevBase* dev;
    dev = common_init( pls );

    pls->plbuf_write = 1;             // use the plot buffer!
    pls->termin      = 1;             // interactive device
    pls->graphx      = GRAPHICS_MODE; //  No text mode for this driver (at least for now, might add a console window if I ever figure it out and have the inclination)

//    dev->showGUI    = true;
    dev->bitmapType = (wxBitmapType) 0;
}

#endif  // PLD_wxwidgets


//--------------------------------------------------------------------------
//  void plD_line_wxwidgets( PLStream *pls, short x1a, short y1a,
//													 short x2a, short y2a )
//
//  Draws a line from (x1a, y1a) to (x2a, y2a).
//--------------------------------------------------------------------------
void plD_line_wxwidgets( PLStream *pls, short x1a, short y1a, short x2a, short y2a )
{
    // Log_Verbose( "plD_line_wxwidgets(x1a=%d, y1a=%d, x2a=%d, y2a=%d)", x1a, y1a, x2a, y2a );

    wxPLDevBase* dev = (wxPLDevBase *) pls->dev;

//    if ( !( dev->ready ) )        install_buffer( pls );

    dev->DrawLine( x1a, y1a, x2a, y2a );

//    if ( !( dev->resizing ) && dev->ownGUI )
//    {
//        dev->comcount++;
//        if ( dev->comcount > MAX_COMCOUNT )
//        {
//            wxRunApp( pls, true );
//            dev->comcount = 0;
//        }
//    }
}


//--------------------------------------------------------------------------
//  void plD_polyline_wxwidgets( PLStream *pls, short *xa, short *ya,
//															 PLINT npts )
//
//  Draw a poly line - points are in xa and ya arrays.
//--------------------------------------------------------------------------
void plD_polyline_wxwidgets( PLStream *pls, short *xa, short *ya, PLINT npts )
{
    // Log_Verbose( "plD_polyline_wxwidgets()" );

    // should be changed to use the wxDC::DrawLines function?
    wxPLDevBase* dev = (wxPLDevBase *) pls->dev;

//    if ( !( dev->ready ) )        install_buffer( pls );

    dev->DrawPolyline( xa, ya, npts );

//    if ( !( dev->resizing ) && dev->ownGUI )
//    {
//        dev->comcount++;
//        if ( dev->comcount > MAX_COMCOUNT )
//        {
//            wxRunApp( pls, true );
//            dev->comcount = 0;
//        }
//    }
}


//--------------------------------------------------------------------------
//  void plD_eop_wxwidgets( PLStream *pls )
//
//  End of Page. This function is called if a "end of page" is send by the
//  user. This command is ignored if we have the plot embedded in a
//  wxWidgets application, otherwise the application created by the device
//  takes over.
//--------------------------------------------------------------------------
void plD_eop_wxwidgets( PLStream *pls )
{
    // Log_Verbose( "plD_eop_wxwidgets()" );

    wxPLDevBase* dev = (wxPLDevBase *) pls->dev;

    if ( dev->bitmapType )
    {
        wxMemoryDC memDC;
        wxBitmap   bitmap( dev->width, dev->height, -1 );
        memDC.SelectObject( bitmap );

        dev->BlitRectangle( &memDC, 0, 0, dev->width, dev->height );
        wxImage             buffer = bitmap.ConvertToImage();
        wxFFileOutputStream fstream( pls->OutFile );
        if ( !( buffer.SaveFile( fstream, dev->bitmapType ) ) )
            puts( "Troubles saving file!" );
        memDC.SelectObject( wxNullBitmap );
    }
//
//    if ( dev->ownGUI && !dev->resizing )
//    {
//        if ( pls->nopause || !dev->showGUI )
//            wxRunApp( pls, true );
//        else
//            wxRunApp( pls );
//    }
}


//--------------------------------------------------------------------------
//  void plD_bop_wxwidgets( PLStream *pls )
//
//  Begin of page. Before any plot command, this function is called, If we
//  have already a dc the background is cleared in background color and some
//  state calls are resent - this is because at the first call of this
//  function, a dc does most likely not exist, but state calls are recorded
//  and when a new dc is created this function is called again.
//--------------------------------------------------------------------------
void plD_bop_wxwidgets( PLStream *pls )
{
    // Log_Verbose( "plD_bop_wxwidgets()" );

    wxPLDevBase* dev = (wxPLDevBase *) pls->dev;

    if ( dev->ready )
    {
        //if( pls->termin==0 ) {
        // plGetFam( pls );
        // force new file if pls->family set for all subsequent calls to plGetFam
        // n.b. putting this after plGetFam call is important since plinit calls
        // bop, and you don't want the familying sequence started until after
        // that first call to bop.

        // n.b. pls->dev can change because of an indirect call to plD_init_png
        // from plGetFam if familying is enabled.  Thus, wait to define dev until
        // now.
        //dev = (wxPLDevBase*)pls->dev;
        //
        // pls->famadv = 1;
        // pls->page++;
        // }

        // clear background
        PLINT bgr=0, bgg=0, bgb=0;                  // red, green, blue
        plgcolbg( &bgr, &bgg, &bgb );         // get background color information
        dev->ClearBackground( bgr, bgg, bgb );

        // Replay escape calls that come in before PLESC_DEVINIT.  All of them
        // required a DC that didn't exist yet.
        //
        if ( dev->plstate_width )
            plD_state_wxwidgets( pls, PLSTATE_WIDTH );
        dev->plstate_width = false;

        if ( dev->plstate_color0 )
            plD_state_wxwidgets( pls, PLSTATE_COLOR0 );
        dev->plstate_color0 = false;

        if ( dev->plstate_color1 )
            plD_state_wxwidgets( pls, PLSTATE_COLOR1 );
        dev->plstate_color1 = false;

        // why this? xwin driver has this
        // pls->page++;
    }
}


//--------------------------------------------------------------------------
//  void plD_tidy_wxwidgets( PLStream *pls )
//
//  This function is called, if all plots are done.
//--------------------------------------------------------------------------
void plD_tidy_wxwidgets( PLStream *pls )
{
    // Log_Verbose( "plD_tidy_wxwidgets()" );

    wxPLDevBase* dev = (wxPLDevBase *) pls->dev;

    delete dev;
    pls->dev = NULL; // since in plcore.c pls->dev is free_mem'd
}


//--------------------------------------------------------------------------
//  void plD_state_wxwidgets( PLStream *pls, PLINT op )
//
//  Handler for several state codes. Here we take care of setting the width
//  and color of the pen.
//--------------------------------------------------------------------------
void plD_state_wxwidgets( PLStream *pls, PLINT op )
{
    // Log_Verbose( "plD_state_wxwidgets(op=%d)", op );

    wxPLDevBase* dev = (wxPLDevBase *) pls->dev;

    switch ( op )
    {
    case PLSTATE_WIDTH: // 1
        if ( dev->ready )
            dev->SetWidth( pls );
        else
            dev->plstate_width = true;
        break;

    case PLSTATE_COLOR0: // 2
        if ( dev->ready )
            dev->SetColor0( pls );
        else
            dev->plstate_color0 = true;
        break;

    case PLSTATE_COLOR1: // 3
        if ( dev->ready )
            dev->SetColor1( pls );
        else
            dev->plstate_color1 = true;
        break;

    //For all these state changes we don't need to do anything
    //and if they occur before/during initialization we don't
    //want to call install_buffer
    case PLSTATE_FILL:
    case PLSTATE_CMAP0:
    case PLSTATE_CMAP1:
    case PLSTATE_CHR:
    case PLSTATE_SYM:
        break;

//    default:
//        if ( !( dev->ready ) )            install_buffer( pls );
    }
}


//--------------------------------------------------------------------------
//  void plD_esc_wxwidgets( PLStream *pls, PLINT op, void *ptr )
//
//  Handler for several escape codes. Here we take care of filled polygons,
//  XOR or copy mode, initialize device (install dc from outside), and if
//  there is freetype support, rerendering of text.
//--------------------------------------------------------------------------
void plD_esc_wxwidgets( PLStream *pls, PLINT op, void *ptr )
{
    // Log_Verbose( "plD_esc_wxwidgets(op=%d, ptr=%x)", op, ptr );

    wxPLDevBase* dev = (wxPLDevBase *) pls->dev;

    switch ( op )
    {
    case PLESC_FILL:
        fill_polygon( pls );
        break;
      case PLESC_FILL_MULTIPATH:
        fill_multiple_polygon( pls );
        break;
    case PLESC_XORMOD:
      printf("PLESC_XORMOD not implemented, FIXME\n");
        // switch between wxXOR and wxCOPY
        // if( dev->ready ) {
        //                  if( dev->m_dc->GetLogicalFunction() == wxCOPY )
        //                          dev->m_dc->SetLogicalFunction( wxXOR );
        //                  else if( dev->m_dc->GetLogicalFunction() == wxXOR )
        //                          dev->m_dc->SetLogicalFunction( wxCOPY );
        //          }
        break;

    case PLESC_DEVINIT:
        dev->SetExternalBuffer( ptr );

        // replay begin of page call and state settings
        plD_bop_wxwidgets( pls );
        break;
      case PLESC_LOAD_FONT:
        dev->PSSetFont(* (PLUNICODE*) ptr);
        break;
      case PLESC_HAS_TEXT:

            dev->ProcessString( pls, (EscText *) ptr );
        break;

    case PLESC_RESIZE:
    {
        wxSize* size = (wxSize *) ptr;
        wx_set_size( pls, size->GetWidth(), size->GetHeight() );
    }
    break;

    case PLESC_CLEAR:
//        if ( !( dev->ready ) )            install_buffer( pls );
//        // Since the plot is updated only every MAX_COMCOUNT commands (usually 5000)
//        //       before we clear the screen we need to show the plot at least once :)
//        if ( !( dev->resizing ) && dev->ownGUI )
//        {
//            wxRunApp( pls, true );
//            dev->comcount = 0;
//        }
        dev->ClearBackground( pls->cmap0[0].r, pls->cmap0[0].g, pls->cmap0[0].b,
            pls->sppxmi, pls->sppymi, pls->sppxma, pls->sppyma );
        break;

    case PLESC_FLUSH:        // forced update of the window
//        if ( !( dev->resizing ) && dev->ownGUI )
//        {
//            wxRunApp( pls, true );
//            dev->comcount = 0;
//        }
        break;

    case PLESC_GETC:
      printf("Wrongly calling PLESC_GETC, FIXME!\n");
//        if ( dev->ownGUI )
//            GetCursorCmd( pls, (PLGraphicsIn *) ptr );
        break;

    case PLESC_GETBACKEND:
        *( (int *) ptr ) = dev->backend;
        break;

    case PLESC_3D:
      Set3D(ptr);
      break;

    case PLESC_2D:
      UnSet3D();
      break;

    default:
        break;
    }
}


//--------------------------------------------------------------------------
//  static void fill_polygon( PLStream *pls )
//
//  Fill polygon described in points pls->dev_x[] and pls->dev_y[].
//--------------------------------------------------------------------------
static void fill_polygon( PLStream *pls )
{
    // Log_Verbose( "fill_polygon(), npts=%d, x[0]=%d, y[0]=%d", pls->dev_npts, pls->dev_y[0], pls->dev_y[0] );

    wxPLDevBase* dev = (wxPLDevBase *) pls->dev;

//    if ( !( dev->ready ) )        install_buffer( pls );

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
    dev->FillPolygon( pls );

//    if ( !( dev->resizing ) && dev->ownGUI )
//    {
//        dev->comcount += 10;
//        if ( dev->comcount > MAX_COMCOUNT )
//        {
//            wxRunApp( pls, true );
//            dev->comcount = 0;
//        }
//    }
}
//--------------------------------------------------------------------------
//  static void fill_polygon( PLStream *pls )
//
//  Fill polygon described in points pls->dev_x[] and pls->dev_y[].
//--------------------------------------------------------------------------
static void fill_multiple_polygon( PLStream *pls) {
  // Log_Verbose( "fill_polygon(), npts=%d, x[0]=%d, y[0]=%d", pls->dev_npts, pls->dev_y[0], pls->dev_y[0] );

  wxPLDevBase* dev = (wxPLDevBase *) pls->dev;

  //    if ( !( dev->ready ) )        install_buffer( pls );

  if (Status3D == 1) { //enable use everywhere.
    //perform conversion on the fly
    for (PLINT i = 0; i < pls->dev_npath; ++i) {
      PLINT *x = pls->dev_pathx[i];
      PLINT *y = pls->dev_pathy[i];
      for (PLINT j = 0; j < pls->dev_pathnxy[i]; ++j) {
        // 3D convert, must take into account that y is inverted.
        int ix=x[j];
        int iy=y[j];
        if (ix >= 0) {
          SelfTransform3D(&ix, &iy); //avoid negative "codes"
        x[j]=ix;
        y[j]=iy;
        }
      }
    }
  }
  dev->FillPolygons(pls);
}


//--------------------------------------------------------------------------
//  void wx_set_size( PLStream* pls, int width, int height )
//
//  Adds a dc to the stream. The associated device is attached to the canvas
//  as the property "dev".
//--------------------------------------------------------------------------
void wx_set_size( PLStream* pls, int width, int height )
{
    // TODO: buffer must be resized here or in wxplotstream
    // Log_Verbose( "wx_set_size()" );

    wxPLDevBase* dev = (wxPLDevBase *) pls->dev;

    // set new size and scale parameters
    dev->width  = width;
    dev->height = height;

  pls->xlength = dev->width;
  pls->ylength = dev->height;
    dev->clipminx = pls->xlength;
    dev->clipminy = pls->ylength;

  PLFLT downscale, downscale2;
  // Set up physical limits of plotting device (in drawing units)
  downscale = (double) dev->width / (double) (PIXELS_X - 1);
  downscale2 = (double) dev->height / (double) PIXELS_Y;
  if (downscale < downscale2)
    downscale = downscale2;
  plP_setphy((PLINT) 0, (PLINT) (dev->width / downscale),
    (PLINT) 0, (PLINT) (dev->height / downscale));

  // get physical device limits coordinates
  plP_gphy(&dev->xmin, &dev->xmax, &dev->ymin, &dev->ymax);

  // setting scale factors
  dev->scalex = (PLFLT) (dev->xmax - dev->xmin) / (dev->width);
  dev->scaley = (PLFLT) (dev->ymax - dev->ymin) / (dev->height);
   pls->xdpi=VIRTUAL_PIXELS_PER_IN / dev->scalex;
   pls->ydpi=VIRTUAL_PIXELS_PER_IN / dev->scaley;
   
    // clear background if we have a dc, since it's invalid (TODO: why, since in bop
    // it must be cleared anyway?)
    if ( dev->ready )
    {
        PLINT bgr=0, bgg=0, bgb=0;                  // red, green, blue
        plgcolbg( &bgr, &bgg, &bgb );         // get background color information

        dev->CreateCanvas();
        dev->ClearBackground( bgr, bgg, bgb );
  }

}


//--------------------------------------------------------------------------
//  int plD_errorexithandler_wxwidgets( const char *errormessage )
//
//  If an PLplot error occurs, this function shows a dialog regarding
//  this error and than exits.
//--------------------------------------------------------------------------
int plD_errorexithandler_wxwidgets( const char *errormessage )
{
    if ( errormessage[0] )
    {
        wxMessageDialog dialog( 0, wxString( errormessage, *wxConvCurrent ), wxString( "wxWidgets PLplot App error", *wxConvCurrent ), wxOK | wxICON_ERROR );
        dialog.ShowModal();
    }

    return 0;
}


//--------------------------------------------------------------------------
//  void plD_erroraborthandler_wxwidgets( const char *errormessage )
//
//  If PLplot aborts, this function shows a dialog regarding
//  this error.
//--------------------------------------------------------------------------
void plD_erroraborthandler_wxwidgets( const char *errormessage )
{
    if ( errormessage[0] )
    {
        wxMessageDialog dialog( 0, ( wxString( errormessage, *wxConvCurrent ) + wxString( " aborting operation...", *wxConvCurrent ) ), wxString( "wxWidgets PLplot App abort", *wxConvCurrent ), wxOK | wxICON_ERROR );
        dialog.ShowModal();
    }
}

