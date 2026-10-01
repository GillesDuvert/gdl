//----------------------------------*-C++-*----------------------------------//
//
// Copyright (C) 1995 Geoffrey Furnish
// Copyright (C) 1995-2002 Maurice LeBrun
// Copyright (C) 2000-2018 Alan W. Irwin
// Copyright (C) 2003 Joao Cardoso
// Copyright (C) 2003-2013 Andrew Ross
// Copyright (C) 2004-2005 Rafael Laboissiere
// Copyright (C) 2006-2008 Werner Smekal
// Copyright (C) 2009 Hazen Babcock
// Copyright (C) 2010-2011 Hezekiah M. Carty
// Copyright (C) 2014-2015 Phil Rosenberg
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

//--------------------------------------------------------------------------
// @> Source file plstream.
//--------------------------------------------------------------------------

#include "plplot.h"
#include "plstream.h"

#include <iostream>

#ifdef PL_USE_NAMESPACE
using namespace std;
#endif

//Callbacks

// Callback for plfill. This will just call the C plfill function

void plcallback::fill( PLINT n, const PLFLT *x, const PLFLT *y )
{
    plfill( n, x, y );
}

// Transformation routines

// Identity transformation.

void plcallback::tr0( PLFLT x, PLFLT y, PLFLT *tx, PLFLT *ty,
                      PLPointer pltr_data )
{
    pltr0( x, y, tx, ty, pltr_data );
}

// Does linear interpolation from singly dimensioned coord arrays.

void plcallback::tr1( PLFLT x, PLFLT y, PLFLT *tx, PLFLT *ty,
                      PLPointer pltr_data )
{
    pltr1( x, y, tx, ty, pltr_data );
}

// Does linear interpolation from doubly dimensioned coord arrays
// (column dominant, as per normal C 2d arrays).

void plcallback::tr2( PLFLT x, PLFLT y, PLFLT *tx, PLFLT *ty,
                      PLPointer pltr_data )
{
    pltr2( x, y, tx, ty, pltr_data );
}

// Just like pltr2() but uses pointer arithmetic to get coordinates from
// 2d grid tables.

void plcallback::tr2p( PLFLT x, PLFLT y, PLFLT *tx, PLFLT *ty,
                       PLPointer pltr_data )
{
    pltr2p( x, y, tx, ty, pltr_data );
}
// Just like pltr2() but uses fortran pointer arithmetic to get coordinates from
// 2d grid tables.

void plcallback::tr2f( PLFLT x, PLFLT y, PLFLT *tx, PLFLT *ty,
                       PLPointer pltr_data )
{
    pltr2f( x, y, tx, ty, pltr_data );
}

PLINT plstream::active_streams = 0;

plstream::plstream()
{
    ::c_plmkstrm( &stream );
    //::c_plinit();
    active_streams++;
}

plstream::plstream ( PLS::stream_id sid, PLINT strm /*=0*/ )
{
    switch ( sid )
    {
    case PLS::Next:
//      throw( "plstream ctor option not implemented." );
        break;

    case PLS::Current:
        ::c_plgstrm( &stream );
        break;

    case PLS::Specific:
        stream = strm;
        break;

    default:
//      throw( "plstream ctor option not implemented." );
        break;
    }
}

plstream::plstream( PLINT nx, PLINT ny, const char *driver, const char *file )
{
    ::c_plmkstrm( &stream );

    if ( driver )
        ::c_plsdev( driver );
    if ( file )
        ::c_plsfnam( file );
    ::c_plssub( nx, ny );
    //::c_plinit();

    active_streams++;
}

plstream::plstream( PLINT nx, PLINT ny, PLINT r, PLINT g, PLINT b,
                    const char *driver, const char *file )
{
    ::c_plmkstrm( &stream );

    if ( driver )
        ::c_plsdev( driver );
    if ( file )
        ::c_plsfnam( file );
    ::c_plssub( nx, ny );
    ::c_plscolbg( r, g, b );
    //::c_plinit();

    active_streams++;
}

plstream::~plstream()
{
    ::c_plsstrm( stream );
    ::c_plend1();

    active_streams--;
    if ( !active_streams )
        ::c_plend();
}

#define BONZAI    { throw "plstream method not implemented."; }

// C routines callable from stub routines come first

// Advance to subpage "page", or to the next one if "page" = 0.

void
plstream::adv( PLINT page )
{
    set_stream();

    pladv( page );
}

// This functions similarly to plbox() except that the origin of the axes is
// placed at the user-specified point (x0, y0).

void
plstream::axes( PLFLT x0, PLFLT y0, const char *xopt, PLFLT xtick, PLINT nxsub,
                const char *yopt, PLFLT ytick, PLINT nysub )
{
    set_stream();

    plaxes( x0, y0, xopt, xtick, nxsub, yopt, ytick, nysub );
}

// Start new page.  Should only be used with pleop().

void plstream::bop()
{
    set_stream();

    plbop();
}

// This draws a box around the current viewport.

void plstream::box( const char *xopt, PLFLT xtick, PLINT nxsub,
                    const char *yopt, PLFLT ytick, PLINT nysub )
{
    set_stream();

    plbox( xopt, xtick, nxsub, yopt, ytick, nysub );
}


//// Calculate broken-down time from continuous time for current stream.
//void plstream::btime( PLINT & year, PLINT & month, PLINT & day, PLINT & hour,
//                      PLINT & min, PLFLT & sec, PLFLT ctime )
//{
//    set_stream();
//
//    plbtime( &year, &month, &day, &hour, &min, &sec, ctime );
//}

// Calculate world coordinates and subpage from relative device coordinates.

void plstream::calc_world( PLFLT rx, PLFLT ry, PLFLT & wx, PLFLT & wy,
                           PLINT & window )
{
    set_stream();

    plcalc_world( rx, ry, &wx, &wy, &window );
}

// Clear the current subpage.

void plstream::clear()
{
    set_stream();

    plclear();
}

// Set color, map 0.  Argument is integer between 0 and 15.

void plstream::col0( PLINT icol0 )
{
    set_stream();

    plcol0( icol0 );
}

// Set the color using a descriptive name.  Replaces plcol0().

void plstream::col( PLcolor c )
{
    set_stream();

    plcol0( (int) c );
}

// Set color, map 1.  Argument is a float between 0. and 1.

void plstream::col1( PLFLT c )
{
    set_stream();

    plcol1( c );
}

#ifdef PL_DEPRECATED
// Old (incorrect) version retained only for compatibility
void plstream::col( PLFLT c )
{
    set_stream();

    cerr <<
        "plstream::col(PLFLT c) : function deprecated. Use plstream::col1(PLFLT c) instead"
         << endl;

    plcol1( c );
}
#endif //PL_DEPRECATED

//// Configure transformation between continuous and broken-down time (and
//// vice versa) for current stream.
//void plstream::configtime( PLFLT scale, PLFLT offset1, PLFLT offset2,
//                           PLINT ccontrol, PLBOOL ifbtime_offset, PLINT year,
//                           PLINT month, PLINT day, PLINT hour, PLINT min,
//                           PLFLT sec )
//{
//    set_stream();
//
//    plconfigtime( scale, offset1, offset2, ccontrol, ifbtime_offset, year,
//        month, day, hour, min, sec );
//}
//


// Draws a contour plot from data in f(nx,ny).  Is just a front-end to
// plfcont, with a particular choice for f2eval and f2eval_data.

void plstream::cont( const PLFLT * const *f, PLINT nx, PLINT ny, PLINT kx, PLINT lx,
                     PLINT ky, PLINT ly, const PLFLT *clevel, PLINT nlevel,
                     PLTRANSFORM_callback pltr, PLPointer pltr_data )
{
    set_stream();

    plcont( f, nx, ny, kx, lx, ky, ly, clevel, nlevel,
        pltr, pltr_data );
}

// Draws a contour plot using the function evaluator f2eval and data stored
// by way of the f2eval_data pointer.  This allows arbitrary organizations
// of 2d array data to be used.

void plstream::fcont( PLFLT ( *f2eval )( PLINT, PLINT, PLPointer ),
                      PLPointer f2eval_data,
                      PLINT nx, PLINT ny, PLINT kx, PLINT lx,
                      PLINT ky, PLINT ly, const PLFLT *clevel, PLINT nlevel,
                      PLTRANSFORM_callback pltr, PLPointer pltr_data )
{
    set_stream();

    plfcont( f2eval, f2eval_data,
        nx, ny, kx, lx, ky, ly, clevel, nlevel,
        pltr, pltr_data );
}

// Copies state parameters from the reference stream to the current stream.

void plstream::cpstrm( plstream & pls, bool flags )
{
    set_stream();

    plcpstrm( pls.stream, (PLBOOL) flags );
}

//// Calculate continuous time from broken-down time for current stream.
//void plstream::ctime( PLINT year, PLINT month, PLINT day, PLINT hour, PLINT min,
//                      PLFLT sec, PLFLT & ctime )
//{
//    set_stream();
//
//    plctime( year, month, day, hour, min, sec, &ctime );
//}

// Converts input values from relative device coordinates to relative plot
// coordinates.

void plstream::did2pc( PLFLT & xmin, PLFLT & ymin, PLFLT & xmax, PLFLT & ymax )
{
    set_stream();

    pldid2pc( &xmin, &ymin, &xmax, &ymax );
}

// Converts input values from relative plot coordinates to relative device
// coordinates.

void plstream::dip2dc( PLFLT & xmin, PLFLT & ymin, PLFLT & xmax, PLFLT & ymax )
{
    set_stream();

    pldip2dc( &xmin, &ymin, &xmax, &ymax );
}

// These shouldn't be needed, are supposed to be handled by ctor/dtor
// semantics of the plstream object.

// End a plotting session for all open streams.

// void plstream::end()
// {
//     set_stream();

//     plend();
// }

// End a plotting session for the current stream only.

// void plstream::end1()
// {
//     set_stream();

//     plend1();
// }

// Simple interface for defining viewport and window.

void plstream::env( PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax,
                    PLINT just, PLINT axis )
{
    set_stream();

    plenv( xmin, xmax, ymin, ymax, just, axis );
}

// Similar to env() above, but in multiplot mode does not advance
// the subpage, instead the current subpage is cleared

void plstream::env0( PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax,
                     PLINT just, PLINT axis )
{
    set_stream();

    plenv0( xmin, xmax, ymin, ymax, just, axis );
}

// End current page.  Should only be used with plbop().

void plstream::eop()
{
    set_stream();

    pleop();
}

// Plot horizontal error bars (xmin(i),y(i)) to (xmax(i),y(i)).

void plstream::errx( PLINT n, const PLFLT *xmin, const PLFLT *xmax, const PLFLT *y )
{
    set_stream();

    plerrx( n, xmin, xmax, y );
}

// Plot vertical error bars (x,ymin(i)) to (x(i),ymax(i)).

void plstream::erry( PLINT n, const PLFLT *x, const PLFLT *ymin, const PLFLT *ymax )
{
    set_stream();

    plerry( n, x, ymin, ymax );
}

// Pattern fills the polygon bounded by the input points.

void plstream::fill( PLINT n, const PLFLT *x, const PLFLT *y )
{
    set_stream();

    plfill( n, x, y );
}

// Flushes the output stream.  Use sparingly, if at all.

void plstream::flush()
{
    set_stream();

    ::c_plflush();
}

// Get character default height and current (scaled) height.

void plstream::gchr( PLFLT & p_def, PLFLT & p_ht )
{
    set_stream();

    plgchr( &p_def, &p_ht );
}

// Returns 8 bit RGB values for given color from color map 0.

void plstream::gcol0( PLINT icol0, PLINT & r, PLINT & g, PLINT & b )
{
    set_stream();

    plgcol0( icol0, &r, &g, &b );
}

// Returns 8 bit RGB values + alpha value for given color from color map 0.

void plstream::gcol0a( PLINT icol0, PLINT & r, PLINT & g, PLINT & b, PLFLT & a )
{
    set_stream();

    plgcol0a( icol0, &r, &g, &b, &a );
}

// Returns the background color by 8 bit RGB value.

void plstream::gcolbg( PLINT & r, PLINT & g, PLINT & b )
{
    set_stream();

    plgcolbg( &r, &g, &b );
}

// Returns the background color by 8 bit RGB value + alpha value.

void plstream::gcolbga( PLINT & r, PLINT & g, PLINT & b, PLFLT & a )
{
    set_stream();

    plgcolbga( &r, &g, &b, &a );
}

// Returns the current compression setting

void plstream::gcompression( PLINT & compression )
{
    set_stream();

    plgcompression( &compression );
}

// Retrieve current window into device space.

void plstream::gdidev( PLFLT & mar, PLFLT & aspect, PLFLT & jx, PLFLT & jy )
{
    set_stream();

    plgdidev( &mar, &aspect, &jx, &jy );
}

// Get plot orientation.

void plstream::gdiori( PLFLT & rot )
{
    set_stream();

    plgdiori( &rot );
}

// Retrieve current window into plot space.

void plstream::gdiplt( PLFLT & xmin, PLFLT & ymin, PLFLT & xmax, PLFLT & ymax )
{
    set_stream();

    plgdiplt( &xmin, &ymin, &xmax, &ymax );
}

// Get family file parameters.

void plstream::gfam( PLINT & fam, PLINT & num, PLINT & bmax )
{
    set_stream();

    plgfam( &fam, &num, &bmax );
}

// Get the (current) output file name.  Must be preallocated to >80 bytes.

void plstream::gfnam( char *fnam )
{
    set_stream();

    plgfnam( fnam );
}

// Get current run level.

void plstream::glevel( PLINT & level )
{
    set_stream();

    plglevel( &level );
}

// Get output device parameters.

void plstream::gpage( PLFLT & xp, PLFLT & yp, PLINT & xleng, PLINT & yleng,
                      PLINT & xoff, PLINT & yoff )
{
    set_stream();

    plgpage( &xp, &yp, &xleng, &yleng, &xoff, &yoff );
}

// Switches to graphics screen.

void plstream::gra()
{
    set_stream();

    plgra();
}


// Get subpage boundaries in absolute coordinates.

void plstream::gspa( PLFLT & xmin, PLFLT & xmax, PLFLT & ymin, PLFLT & ymax )
{
    set_stream();

    plgspa( &xmin, &xmax, &ymin, &ymax );
}

// This shouldn't be needed in this model.

// Get current stream number.

// void plstream::gstrm( PLINT *p_strm )
// {
//     set_stream();

//     plgstrm(p_strm);
// }

// Get the current library version number.

void plstream::gver( char *p_ver )
{
    set_stream();

    plgver( p_ver );
}

// Get viewport window in normalized world coordinates

void plstream::gvpd( PLFLT & xmin, PLFLT & xmax, PLFLT & ymin, PLFLT & ymax )
{
    set_stream();

    plgvpd( &xmin, &xmax, &ymin, &ymax );
}

// Get viewport window in world coordinates

void plstream::gvpw( PLFLT & xmin, PLFLT & xmax, PLFLT & ymin, PLFLT & ymax )
{
    set_stream();

    plgvpw( &xmin, &xmax, &ymin, &ymax );
}

// Get x axis labeling parameters.

void plstream::gxax( PLINT & digmax, PLINT & digits )
{
    set_stream();

    plgxax( &digmax, &digits );
}

// Get y axis labeling parameters.

void plstream::gyax( PLINT & digmax, PLINT & digits )
{
    set_stream();

    plgyax( &digmax, &digits );
}

// Get z axis labeling parameters

void plstream::gzax( PLINT & digmax, PLINT & digits )
{
    set_stream();

    plgzax( &digmax, &digits );
}

// Set current color (map 0) by hue, lightness, and saturation.

// Initializes PLplot, using preset or default options

void plstream::init()
{
    set_stream();

    plinit();

    plgstrm( &stream );

    // This is only set in the constructor.
    //active_streams++;
}

// load a new truetype font (will make it current)
void plstream::loadtt(const char *name)
{
    set_stream();
    ttFontLoad(name);
}
// set a known truetype font as current
void plstream::settt(int n)
{
    set_stream();
    ttFontSet(n);
}

// Draws a line segment from (x1, y1) to (x2, y2).

void plstream::join( PLFLT x1, PLFLT y1, PLFLT x2, PLFLT y2 )
{
    set_stream();

    pljoin( x1, y1, x2, y2 );
}

// Sets position of the light source

void plstream::lightsource( PLFLT x, PLFLT y, PLFLT z )
{
    set_stream();

    pllightsource( x, y, z );
}

// Draws line segments connecting a series of points.

void plstream::line( PLINT n, const PLFLT *x, const PLFLT *y )
{
    set_stream();

    plline( n, x, y );
}

// Draws a line in 3 space.

void plstream::line3( PLINT n, const PLFLT *x, const PLFLT *y, const PLFLT *z )
{
    set_stream();

    plline3( n, x, y, z );
}

// Set line style.

void plstream::lsty( PLINT lin )
{
    set_stream();

    pllsty( lin );
}

// Plots a mesh representation of the function z[x][y].

void plstream::mesh( const PLFLT *x, const PLFLT *y, const PLFLT * const *z, PLINT nx, PLINT ny,
                     PLINT opt )
{
    set_stream();

    plmesh( x, y, z, nx, ny, opt );
}

// Plots a mesh representation of the function z[x][y] with contour.

void plstream::meshc( const PLFLT *x, const PLFLT *y, const PLFLT * const *z, PLINT nx, PLINT ny,
                      PLINT opt, const PLFLT *clevel, PLINT nlevel )
{
    set_stream();

    plmeshc( x, y, z, nx, ny, opt, clevel, nlevel );
}

// Prints out "text" at specified position relative to viewport

void plstream::mtex( const char *side, PLFLT disp, PLFLT pos, PLFLT just,
                     const char *text )
{
    set_stream();

    plmtex( side, disp, pos, just, text );
}

// Plots a 3-d shaded representation of the function z[x][y].

void plstream::surf3d( const PLFLT *x, const PLFLT *y, const PLFLT * const *z,
                       PLINT nx, PLINT ny, PLINT opt,
                       const PLFLT *clevel, PLINT nlevel, PLINT* shademap )
{
    set_stream();

    plsurf3d( x, y, z, nx, ny, opt, clevel, nlevel, shademap );
}

// Plots a 3-d shaded representation of the function z[x][y] with
// y index limits

void plstream::surf3dl( const PLFLT *x, const PLFLT *y, const PLFLT * const *z,
                        PLINT nx, PLINT ny, PLINT opt,
                        const PLFLT *clevel, PLINT nlevel,
                        PLINT ixstart, PLINT ixn,
                        const PLINT *indexymin, const PLINT *indexymax, PLINT* shademap )
{
    set_stream();

    plsurf3dl( x, y, z, nx, ny, opt, clevel, nlevel, ixstart, ixn,
        indexymin, indexymax, shademap );
}

// Plots a 3-d representation of the function z[x][y].

void plstream::plot3d( const PLFLT *x, const PLFLT *y, const PLFLT * const *z,
                       PLINT nx, PLINT ny, PLINT opt, bool side )
{
    set_stream();

    ::plot3d( x, y, z, nx, ny, opt, (PLBOOL) side );
}

// Plots a 3-d representation of the function z[x][y] with contour.

void plstream::plot3dc( const PLFLT *x, const PLFLT *y, const PLFLT * const *z,
                        PLINT nx, PLINT ny, PLINT opt,
                        const PLFLT *clevel, PLINT nlevel )
{
    set_stream();

    ::plot3dc( x, y, z, nx, ny, opt, clevel, nlevel );
}

// Plots a 3-d representation of the function z[x][y] with contour
// and y index limits

void plstream::plot3dcl( const PLFLT *x, const PLFLT *y, const PLFLT * const *z,
                         PLINT nx, PLINT ny, PLINT opt,
                         const PLFLT *clevel, PLINT nlevel,
                         PLINT ixstart, PLINT ixn,
                         const PLINT *indexymin, const PLINT *indexymax, PLINT* shademap )
{
    set_stream();

    ::plot3dcl( x, y, z, nx, ny, opt, clevel, nlevel, ixstart, ixn,
        indexymin, indexymax, shademap );
}

// Process options list using current options info.

PLINT plstream::parseopts( int *p_argc, char **argv, PLINT mode )
{
    set_stream();

    return ::plparseopts( p_argc, argv, mode );
}

// Set fill pattern directly.

void plstream::pat( PLINT nlin, const PLINT *inc, const PLINT *del )
{
    set_stream();

    plpat( nlin, inc, del );
}

// Draw a line connecting two points, accounting for coordinate transforms

void plstream::path( PLINT n, PLFLT x1, PLFLT y1, PLFLT x2, PLFLT y2 )
{
    set_stream();

    plpath( n, x1, y1, x2, y2 );
}

// Set the floating point precision (in number of places) in numeric labels.

void plstream::prec( PLINT setp, PLINT prec )
{
    set_stream();

    plprec( setp, prec );
}

// Set fill pattern, using one of the predefined patterns.

void plstream::psty( PLINT patt )
{
    set_stream();

    plpsty( patt );
}

// Prints out "text" at world cooordinate (x,y).

void plstream::ptex( PLFLT x, PLFLT y, PLFLT dx, PLFLT dy, PLFLT just,
                     const char *text )
{
    set_stream();

    plptex( x, y, dx, dy, just, text );
}

// Get the world coordinates associated with device coordinates

PLINT plstream::translatecursor( PLGraphicsIn *gin )
{
    set_stream();

    return plTranslateCursor( gin );
}

// Set character height.

void plstream::schr( PLFLT def, PLFLT scale )
{
    set_stream();

    plschr( def, scale );
}

// Set number of colors in cmap 0

void plstream::scmap0n( PLINT ncol0 )
{
    set_stream();

    plscmap0n( ncol0 );
}

// Set number of colors in cmap 1

void plstream::scmap1n( PLINT ncol1 )
{
    set_stream();

    plscmap1n( ncol1 );
}

// Set number of colors in cmap 1

void plstream::scmap1_range( PLFLT min_color, PLFLT max_color )
{
    set_stream();

    plscmap1_range( min_color, max_color );
}

// Set number of colors in cmap 1

void plstream::gcmap1_range( PLFLT & min_color, PLFLT & max_color )
{
    set_stream();

    plgcmap1_range( &min_color, &max_color );
}

// Set color map 0 colors by 8 bit RGB values

void plstream::scmap0( const PLINT *r, const PLINT *g, const PLINT *b, PLINT ncol0 )
{
    set_stream();

    plscmap0( r, g, b, ncol0 );
}

// Set color map 0 colors by 8 bit RGB values + alpha value

void plstream::scmap0a( const PLINT *r, const PLINT *g, const PLINT *b, const PLFLT *a, PLINT ncol0 )
{
    set_stream();

    plscmap0a( r, g, b, a, ncol0 );
}

// Set color map 1 colors by 8 bit RGB values

void plstream::scmap1( const PLINT *r, const PLINT *g, const PLINT *b, PLINT ncol1 )
{
    set_stream();

    plscmap1( r, g, b, ncol1 );
}

// Set color map 1 colors by 8 bit RGB values + alpha value

void plstream::scmap1a( const PLINT *r, const PLINT *g, const PLINT *b, const PLFLT *a, PLINT ncol1 )
{
    set_stream();

    plscmap1a( r, g, b, a, ncol1 );
}

// Set color map 1 colors using a piece-wise linear relationship between
// intensity [0,1] (cmap 1 index) and position in HLS or RGB color space.

void plstream::scmap1l( bool itype, PLINT npts, const PLFLT *intensity,
                        const PLFLT *coord1, const PLFLT *coord2, const PLFLT *coord3,
                        const bool *alt_hue_path )
{
    PLBOOL *loc_alt_hue_path = NULL;
    if ( alt_hue_path != NULL )
    {
        loc_alt_hue_path = new PLBOOL[npts - 1];
        for ( int i = 0; i < npts - 1; i++ )
        {
            loc_alt_hue_path[i] = (PLBOOL) alt_hue_path[i];
        }
    }

    set_stream();

    plscmap1l( (PLBOOL) itype, npts, intensity, coord1, coord2, coord3, loc_alt_hue_path );

    if ( loc_alt_hue_path != NULL )
        delete [] loc_alt_hue_path;
}

// Set color map 1 colors using a piece-wise linear relationship between
// intensity [0,1] (cmap 1 index) and position in HLS or RGB color space
// and alpha value.

void plstream::scmap1la( bool itype, PLINT npts, const PLFLT *intensity,
                         const PLFLT *coord1, const PLFLT *coord2, const PLFLT *coord3,
                         const PLFLT *a, const bool *alt_hue_path )
{
    PLBOOL *loc_alt_hue_path = NULL;
    if ( alt_hue_path != NULL )
    {
        loc_alt_hue_path = new PLBOOL[npts - 1];
        for ( int i = 0; i < npts - 1; i++ )
        {
            loc_alt_hue_path[i] = (PLBOOL) alt_hue_path[i];
        }
    }

    set_stream();

    plscmap1la( (PLBOOL) itype, npts, intensity, coord1, coord2, coord3,
        a, loc_alt_hue_path );

    if ( loc_alt_hue_path != NULL )
        delete [] loc_alt_hue_path;
}

// Set a given color from color map 0 by 8 bit RGB value

void plstream::scol0( PLINT icol0, PLINT r, PLINT g, PLINT b )
{
    set_stream();

    plscol0( icol0, r, g, b );
}

// Set a given color from color map 0 by 8 bit RGB value + alpha value

void plstream::scol0a( PLINT icol0, PLINT r, PLINT g, PLINT b, PLFLT a )
{
    set_stream();

    plscol0a( icol0, r, g, b, a );
}

// Set the background color by 8 bit RGB value

void plstream::scolbg( PLINT r, PLINT g, PLINT b )
{
    set_stream();

    plscolbg( r, g, b );
}

// Set the background color by 8 bit RGB + alpha value

void plstream::scolbga( PLINT r, PLINT g, PLINT b, PLFLT a )
{
    set_stream();

    plscolbga( r, g, b, a );
}

// Used to globally turn color output on/off

void plstream::scolor( PLINT color )
{
    set_stream();

    plscolor( color );
}

// Sets the compression level

void plstream::scompression( PLINT compression )
{
    set_stream();

    plscompression( compression );
}

// Set the device (keyword) name

void plstream::sdev( const char *devname )
{
    set_stream();

    plsdev( devname );
}

// Get the device (keyword) name

void plstream::gdev( char *devname )
{
    set_stream();

    plgdev( devname );
}

// Set window into device space using margin, aspect ratio, and
// justification

void plstream::sdidev( PLFLT mar, PLFLT aspect, PLFLT jx, PLFLT jy )
{
    set_stream();

    plsdidev( mar, aspect, jx, jy );
}

// Set up transformation from metafile coordinates.

void plstream::sdimap( PLINT dimxmin, PLINT dimxmax,
                       PLINT dimymin, PLINT dimymax,
                       PLFLT dimxpmm, PLFLT dimypmm )
{
    set_stream();

    plsdimap( dimxmin, dimxmax, dimymin, dimymax, dimxpmm, dimypmm );
}

// Set plot orientation, specifying rotation in units of pi/2.

void plstream::sdiori( PLFLT rot )
{
    set_stream();

    plsdiori( rot );
}

// Set window into plot space

void plstream::sdiplt( PLFLT xmin, PLFLT ymin, PLFLT xmax, PLFLT ymax )
{
    set_stream();

    plsdiplt( xmin, ymin, xmax, ymax );
}

// Set window into plot space incrementally (zoom)

void plstream::sdiplz( PLFLT xmin, PLFLT ymin, PLFLT xmax, PLFLT ymax )
{
    set_stream();

    plsdiplz( xmin, ymin, xmax, ymax );
}

// Set the escape character for text strings.

void plstream::sesc( char esc )
{
    set_stream();

    plsesc( esc );
}

// Set the offset and spacing of contour labels

void plstream::setcontlabelparam( PLFLT offset, PLFLT size, PLFLT spacing,
                                  PLINT active )
{
    set_stream();

    pl_setcontlabelparam( offset, size, spacing, active );
}

// Set the format of the contour labels

void plstream::setcontlabelformat( PLINT lexp, PLINT sigdig )
{
    set_stream();

    pl_setcontlabelformat( lexp, sigdig );
}

// Set family file parameters

void plstream::sfam( PLINT fam, PLINT num, PLINT bmax )
{
    set_stream();

    plsfam( fam, num, bmax );
}

// Set the output file name.

void plstream::sfnam( const char *fnam )
{
    set_stream();

    plsfnam( fnam );
}

// Set the pointer to the data used in driver initialisation

void plstream::sdevdata( void *data )
{
    set_stream();

    plsdevdata( data );
}

// Shade region.

void
plstream::shade( const PLFLT * const *a, PLINT nx, PLINT ny,
                 PLDEFINED_callback defined,
                 PLFLT left, PLFLT right, PLFLT bottom, PLFLT top,
                 PLFLT shade_min, PLFLT shade_max,
                 PLINT sh_cmap, PLFLT sh_color, PLFLT sh_width,
                 PLINT min_color, PLFLT min_width,
                 PLINT max_color, PLFLT max_width,
                 PLFILL_callback fill, bool rectangular,
                 PLTRANSFORM_callback pltr, PLPointer pltr_data )
{
    set_stream();

    plshade( a, nx, ny, defined, left, right, bottom, top,
        shade_min, shade_max,
        sh_cmap, sh_color, sh_width,
        min_color, min_width, max_color, max_width,
        fill, (PLBOOL) rectangular, pltr, pltr_data );
}

void
plstream::shades( const PLFLT * const *a, PLINT nx, PLINT ny,
                  PLDEFINED_callback defined,
                  PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax,
                  const PLFLT *clevel, PLINT nlevel, PLFLT fill_width,
                  PLINT cont_color, PLFLT cont_width,
                  PLFILL_callback fill, bool rectangular,
                  PLTRANSFORM_callback pltr, PLPointer pltr_data )
{
    set_stream();

    plshades( a, nx, ny, defined, xmin, xmax, ymin, ymax,
        clevel, nlevel, fill_width, cont_color, cont_width,
        fill, (PLBOOL) rectangular, pltr, pltr_data );
}

void
plstream::fshade( PLFLT ( *f2eval )( PLINT, PLINT, PLPointer ),
                  PLPointer f2eval_data,
                  PLFLT ( *c2eval )( PLINT, PLINT, PLPointer ),
                  PLPointer c2eval_data,
                  PLINT nx, PLINT ny,
                  PLFLT left, PLFLT right, PLFLT bottom, PLFLT top,
                  PLFLT shade_min, PLFLT shade_max,
                  PLINT sh_cmap, PLFLT sh_color, PLFLT sh_width,
                  PLINT min_color, PLFLT min_width,
                  PLINT max_color, PLFLT max_width,
                  PLFILL_callback fill, bool rectangular,
                  PLTRANSFORM_callback pltr, PLPointer pltr_data )
{
    set_stream();

    plfshade( f2eval, f2eval_data,
        c2eval, c2eval_data,
        nx, ny, left, right, bottom, top,
        shade_min, shade_max,
        sh_cmap, sh_color, sh_width,
        min_color, min_width, max_color, max_width,
        fill, (PLBOOL) rectangular, pltr, pltr_data );
}

// Setup a user-provided custom labeling function

void plstream::slabelfunc( PLLABEL_FUNC_callback label_func, PLPointer label_data )
{
    set_stream();

    plslabelfunc( label_func, label_data );
}

// Set up lengths of major tick marks.

void plstream::smaj( PLFLT def, PLFLT scale )
{
    set_stream();

    plsmaj( def, scale );
}

// Set the RGB memory area to be plotted (with the 'mem' or 'memcairo' drivers)

void plstream::smem( PLINT maxx, PLINT maxy, void *plotmem, void *zbuff  )
{
    set_stream();

    plsmem( maxx, maxy, plotmem, zbuff );
}

// Set the RGBA memory area to be plotted (with the 'memcairo' drivers)

void plstream::smema( PLINT maxx, PLINT maxy, void *plotmem,  void *zbuff   )
{
    set_stream();

    plsmema( maxx, maxy, plotmem, zbuff );
}

// Set up lengths of minor tick marks.

void plstream::smin( PLFLT def, PLFLT scale )
{
    set_stream();

    plsmin( def, scale );
}

// Set orientation.  Must be done before calling plinit.

void plstream::sori( PLINT ori )
{
    set_stream();

    plsori( ori );
}

// Set output device parameters.  Usually ignored by the driver.

void plstream::spage( PLFLT xp, PLFLT yp, PLINT xleng, PLINT yleng,
                      PLINT xoff, PLINT yoff )
{
    set_stream();

    plspage( xp, yp, xleng, yleng, xoff, yoff );
}

// Set the colors for color table 0 from a cmap0 file

void plstream::spal0( const char *filename )
{
    set_stream();

    plspal0( filename );
}

// Set the colors for color table 1 from a cmap1 file

void plstream::spal1( const char *filename, bool interpolate )
{
    set_stream();

    plspal1( filename, (PLBOOL) interpolate );
}

// Set the pause (on end-of-page) status

void plstream::spause( bool pause )
{
    set_stream();

    plspause( (PLBOOL) pause );
}

// Set stream number.

void plstream::sstrm( PLINT strm )
{
    set_stream();

    plsstrm( strm );
}

// Set the number of subwindows in x and y

void plstream::ssub( PLINT nx, PLINT ny )
{
    set_stream();

    plssub( nx, ny );
}

// Set symbol height.

void plstream::ssym( PLFLT def, PLFLT scale )
{
    set_stream();

    plssym( def, scale );
}

// Initialize PLplot, passing in the windows/page settings.

void plstream::star( PLINT nx, PLINT ny )
{
    set_stream();

    plstar( nx, ny );
}

// Initialize PLplot, passing the device name and windows/page settings.

void plstream::start( const char *devname, PLINT nx, PLINT ny )
{
    set_stream();

    plstart( devname, nx, ny );
}

// Set the coordinate transform

void plstream::stransform( PLTRANSFORM_callback coordinate_transform, PLPointer coordinate_transform_data )
{
    set_stream();

    plstransform( coordinate_transform, coordinate_transform_data );
}

// Set up a new line style

void plstream::styl( PLINT nms, const PLINT *mark, const PLINT *space )
{
    set_stream();

    plstyl( nms, mark, space );
}

// Sets the edges of the viewport to the specified absolute coordinates

void plstream::svpa( PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax )
{
    set_stream();

    plsvpa( xmin, xmax, ymin, ymax );
}

// Set x axis labeling parameters

void plstream::sxax( PLINT digmax, PLINT digits )
{
    set_stream();

    plsxax( digmax, digits );
}

// Set inferior X window

void plstream::sxwin( PLINT window_id )
{
    set_stream();

    plsxwin( window_id );
}

// Set y axis labeling parameters

void plstream::syax( PLINT digmax, PLINT digits )
{
    set_stream();

    plsyax( digmax, digits );
}

// Set z axis labeling parameters

void plstream::szax( PLINT digmax, PLINT digits )
{
    set_stream();

    plszax( digmax, digits );
}

// Switches to text screen.

void plstream::text()
{
    set_stream();

    pltext();
}

// Set the format for date / time labels

void plstream::timefmt( const char *fmt )
{
    set_stream();

    pltimefmt( fmt );
}

// Sets the edges of the viewport with the given aspect ratio, leaving
// room for labels.

void plstream::vasp( PLFLT aspect )
{
    set_stream();

    plvasp( aspect );
}

// Creates the largest viewport of the specified aspect ratio that fits
// within the specified normalized subpage coordinates.

void plstream::vpas( PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax,
                     PLFLT aspect )
{
    set_stream();

    plvpas( xmin, xmax, ymin, ymax, aspect );
}

// Creates a viewport with the specified normalized subpage coordinates.

void plstream::vpor( PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax )
{
    set_stream();

    plvpor( xmin, xmax, ymin, ymax );
}

// Defines a "standard" viewport with seven character heights for
// the left margin and four character heights everywhere else.

void plstream::vsta()
{
    set_stream();

    plvsta();
}

// Set up a window for three-dimensional plotting.

void plstream::w3d( PLFLT basex, PLFLT basey, PLFLT height, PLFLT xmin0,
                    PLFLT xmax0, PLFLT ymin0, PLFLT ymax0, PLFLT zmin0,
                    PLFLT zmax0, PLFLT alt, PLFLT az )
{
    set_stream();

    plw3d( basex, basey, height, xmin0, xmax0, ymin0, ymax0, zmin0, zmax0,
        alt, az );
}

// Set pen width.

void plstream::width( PLFLT width )
{
    set_stream();

    plwidth( width );
}

// Set up world coordinates of the viewport boundaries (2d plots).

void plstream::wind( PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax )
{
    set_stream();

    plwind( xmin, xmax, ymin, ymax );
}

// The rest for use from C / C++ only

// Returns a list of file-oriented device names and their menu strings

void plstream::gFileDevs( const char ***p_menustr, const char ***p_devname,
                          int *p_ndev )
{
    set_stream();

    plgFileDevs( p_menustr, p_devname, p_ndev );
}

// Set the function pointer for the keyboard event handler

void plstream::sKeyEH( void ( *KeyEH )( PLGraphicsIn *, void *, int * ),
                       void *KeyEH_data )
{
    set_stream();

    plsKeyEH( KeyEH, KeyEH_data );
}

// Set the variables to be used for storing error info

void plstream::sError( PLINT *errcode, char *errmsg )
{
    set_stream();

    plsError( errcode, errmsg );
}

// Sets an optional user exit handler.

void plstream::sexit( int ( *handler )( const char * ) )
{
    set_stream();

    plsexit( handler );
}


// Function evaluators

// Does a lookup from a 2d function array.  Array is of type (PLFLT **),
// and is column dominant (normal C ordering).

PLFLT plstream::f2eval2( PLINT ix, PLINT iy, PLPointer plf2eval_data )
{
    set_stream();

    return ::plf2eval2( ix, iy, plf2eval_data );
}

// Does a lookup from a 2d function array.  Array is of type (PLFLT *),
// and is column dominant (normal C ordering).

PLFLT plstream::f2eval( PLINT ix, PLINT iy, PLPointer plf2eval_data )
{
    set_stream();

    return ::plf2eval( ix, iy, plf2eval_data );
}

// Does a lookup from a 2d function array.  Array is of type (PLFLT *),
// and is row dominant (Fortran ordering).

PLFLT plstream::f2evalr( PLINT ix, PLINT iy, PLPointer plf2eval_data )
{
    set_stream();

    return ::plf2evalr( ix, iy, plf2eval_data );
}

// Command line parsing utilities

// Clear internal option table info structure.

void plstream::ClearOpts()
{
    set_stream();

    ::plClearOpts();
}

// Reset internal option table info structure.

void plstream::ResetOpts()
{
    set_stream();

    ::plResetOpts();
}

// Merge user option table into internal info structure.

PLINT plstream::MergeOpts( PLOptionTable *options, const char *name,
                           const char **notes )
{
    set_stream();

    return ::plMergeOpts( options, name, notes );
}

// Set the strings used in usage and syntax messages.

void plstream::SetUsage( char *program_string, char *usage_string )
{
    set_stream();

    ::plSetUsage( program_string, usage_string );
}

// Process input strings, treating them as an option and argument pair.

PLINT plstream::setopt( const char *opt, const char *optarg )
{
    set_stream();

    return ::plsetopt( opt, optarg );
}

// Print usage & syntax message.

void plstream::OptUsage()
{
    set_stream();

    ::plOptUsage();
}

// Miscellaneous

// Set the output file pointer

void plstream::gfile( FILE **p_file )
{
    set_stream();

    ::plgfile( p_file );
}

// Get the output file pointer

void plstream::sfile( FILE *file )
{
    set_stream();

    ::plsfile( file );
}


// Get the escape character for text strings.

void plstream::gesc( char *p_esc )
{
    set_stream();

    ::plgesc( p_esc );
}

// Front-end to driver escape function.

void plstream::cmd( PLINT op, void *ptr )
{
    set_stream();

    ::pl_cmd( op, ptr );
}

// Return full pathname for given file if executable

PLINT plstream::FindName( char *p )
{
    set_stream();

    return plFindName( p );
}


// Gets search name for file by concatenating the dir, subdir, and file
// name, allocating memory as needed.

void plstream::GetName( char *dir, char *subdir, char *filename,
                        char **filespec )
{
    set_stream();

    plGetName( dir, subdir, filename, filespec );
}

// Prompts human to input an integer in response to given message.

PLINT plstream::GetInt( char *s )
{
    set_stream();

    return plGetInt( s );
}

// Prompts human to input a float in response to given message.

PLFLT plstream::GetFlt( char *s )
{
    set_stream();

    return plGetFlt( s );
}

// Determine the Iliffe column vector of pointers to PLFLT row
// vectors corresponding to a 2D matrix of PLFLT's that is statically
// allocated.

void plstream::Static2dGrid( PLFLT_NC_MATRIX zIliffe, PLFLT_VECTOR zStatic, PLINT nx, PLINT ny )
{
    set_stream();

    ::plStatic2dGrid( zIliffe, zStatic, nx, ny );
}

// Allocate a block of memory for use as a 2-d grid of PLFLT's organized
// as an Iliffe column vector of pointers to PLFLT row vectors.

void plstream::Alloc2dGrid( PLFLT ***f, PLINT nx, PLINT ny )
{
    set_stream();

    ::plAlloc2dGrid( f, nx, ny );
}

// Frees a block of memory allocated with plAlloc2dGrid().

void plstream::Free2dGrid( PLFLT **f, PLINT nx, PLINT ny )
{
    set_stream();

    ::plFree2dGrid( f, nx, ny );
}

// Find the maximum and minimum of a 2d matrix allocated with plAllc2dGrid().
void plstream::MinMax2dGrid( const PLFLT * const *f, PLINT nx, PLINT ny,
                             PLFLT *fmax, PLFLT *fmin )
{
    set_stream();

    ::plMinMax2dGrid( f, nx, ny, fmax, fmin );
}

// Functions for converting between HLS and RGB color space

void plstream::hlsrgb( PLFLT h, PLFLT l, PLFLT s, PLFLT *p_r, PLFLT *p_g,
                       PLFLT *p_b )
{
    set_stream();

    ::c_plhlsrgb( h, l, s, p_r, p_g, p_b );
}

void plstream::rgbhls( PLFLT r, PLFLT g, PLFLT b, PLFLT *p_h, PLFLT *p_l,
                       PLFLT *p_s )
{
    set_stream();

    ::c_plrgbhls( r, g, b, p_h, p_l, p_s );
}

// Wait for right button mouse event and translate to world coordinates

PLINT plstream::GetCursor( PLGraphicsIn *gin )
{
    set_stream();

    return plGetCursor( gin );
}

//--------------------------------------------------------------------------
//                              end of plstream.cc
//--------------------------------------------------------------------------
