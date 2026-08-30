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
// - text clipping
// - implement AddToClipRegion for text correctly
//
// wxwidgets headers
#include <wx/wx.h>
#include <wx/cmndata.h>
#include <wx/dcps.h>
#include <wx/dcsvg.h>

#include "plDevs.h"

// plplot headers
#include "plplotP.h"

// std and driver headers
#include <cmath>
#include "wxwidgets.h"

#ifdef USE_DEVICE_CONTEXT_ALSO

//define LINE2D, POLYLINE2D
#define LINE2D plD_line_wxwidgets
#define POLYLINE2D plD_polyline_wxwidgets
#include "plplot3d.h"

//--------------------------------------------------------------------------
//  wxPLDevDC::wxPLDevDC( void )
//
//  Constructor of the standard wxWidgets device based on the wxPLDevBase
//  class. Only some initialisations are done.
//--------------------------------------------------------------------------
wxPLDevDC::wxPLDevDC( void ) : wxPLDevBase( wxBACKEND_DC )
{
    m_dc       = NULL;
    m_bitmap   = NULL;
    m_font     = NULL;
    underlined = false;

}


//--------------------------------------------------------------------------
//  wxPLDevDC::~wxPLDevDC( void )
//
//  The deconstructor frees memory allocated by the device.
//--------------------------------------------------------------------------
wxPLDevDC::~wxPLDevDC()
{

    if ( m_font )
        delete m_font;
    if ( m_dc ) {
      delete m_dc;
    }
}


//--------------------------------------------------------------------------
//  void wxPLDevDC::DrawLine( short x1a, short y1a, short x2a, short y2a )
//
//  Draw a line from (x1a, y1a) to (x2a, y2a).
//--------------------------------------------------------------------------
void wxPLDevDC::DrawLine( short x1a, short y1a, short x2a, short y2a )
{
    x1a = (short) ( x1a / scalex ); y1a = (short) ( height - y1a / scaley );
    x2a = (short) ( x2a / scalex );        y2a = (short) ( height - y2a / scaley );

    m_dc->DrawLine( (wxCoord) x1a, (wxCoord) y1a, (wxCoord) x2a, (wxCoord) y2a );

    AddtoClipRegion( (int) x1a, (int) y1a, (int) x2a, (int) y2a );
}


//--------------------------------------------------------------------------
//  void wxPLDevDC::DrawPolyline( short *xa, short *ya, PLINT npts )
//
//  Draw a poly line - coordinates are in the xa and ya arrays.
//--------------------------------------------------------------------------
void wxPLDevDC::DrawPolyline( short *xa, short *ya, PLINT npts )
{
    wxCoord x1a, y1a, x2a, y2a;

    x2a = (wxCoord) ( xa[0] / scalex );
    y2a = (wxCoord) ( height - ya[0] / scaley );
    for ( PLINT i = 1; i < npts; i++ )
    {
        x1a = x2a; y1a = y2a;
        x2a = (wxCoord) ( xa[i] / scalex );
        y2a = (wxCoord) ( height - ya[i] / scaley );

        m_dc->DrawLine( x1a, y1a, x2a, y2a );

        AddtoClipRegion( (int) x1a, (int) y1a, (int) x2a, (int) y2a );
    }
}


//--------------------------------------------------------------------------
//  void wxPLDevDC::ClearBackground( PLINT bgr, PLINT bgg, PLINT bgb,
//                                   PLINT x1, PLINT y1, PLINT x2, PLINT y2 )
//
//  Clear parts ((x1,y1) to (x2,y2)) of the background in color (bgr,bgg,bgb).
//--------------------------------------------------------------------------
void wxPLDevDC::ClearBackground( PLINT bgr, PLINT bgg, PLINT bgb,
                                 PLINT x1, PLINT y1, PLINT x2, PLINT y2 )
{
    if ( x1 < 0 )
        x1 = 0;
    else
        x1 = (PLINT) ( x1 / scalex );
    if ( y1 < 0 )
        y1 = 0;
    else
        y1 = (PLINT) ( height - y1 / scaley );
    if ( x2 < 0 )
        x2 = width;
    else
        x2 = (PLINT) ( x2 / scalex );
    if ( y2 < 0 )
        y2 = height;
    else
        y2 = (PLINT) ( height - y2 / scaley );

    const wxPen   oldPen   = m_dc->GetPen();
    const wxBrush oldBrush = m_dc->GetBrush();

    m_dc->SetPen( *( wxThePenList->FindOrCreatePen( wxColour( bgr, bgg, bgb ), 1, wxPENSTYLE_SOLID ) ) );
    m_dc->SetBrush( wxBrush( wxColour( bgr, bgg, bgb ) ) );
    m_dc->DrawRectangle( x1, y1, x2 - x1, y2 - y1 );

    m_dc->SetPen( oldPen );
    m_dc->SetBrush( oldBrush );

    AddtoClipRegion( x1, y1, x2, y2 );
}


//--------------------------------------------------------------------------
//  void wxPLDevDC::FillPolygon( PLStream *pls )
//
//  Draw a filled polygon.
//--------------------------------------------------------------------------
void wxPLDevDC::FillPolygon( PLStream *pls )
{
    wxPoint *points = new wxPoint[pls->dev_npts];
    wxCoord xoffset = 0;
    wxCoord yoffset = 0;
    
    for ( int i = 0; i < pls->dev_npts; i++ )
    {
        points[i].x = (int) ( pls->dev_x[i] / scalex );
        points[i].y = (int) ( height - pls->dev_y[i] / scaley );
        if ( i > 0 )
            AddtoClipRegion( points[i - 1].x, points[i - 1].y, points[i].x, points[i].y );
    }

    if ( pls->dev_eofill )
    {
        m_dc->DrawPolygon( pls->dev_npts, points, xoffset, yoffset, wxODDEVEN_RULE );
    }
    else
    {
        m_dc->DrawPolygon( pls->dev_npts, points, xoffset, yoffset, wxWINDING_RULE );
    }
    delete[] points;
}


//--------------------------------------------------------------------------
//  void wxPLDevDC::BlitRectangle( wxDC* dc, int vX, int vY,
//                                 int vW, int vH )
//
//  Copy/Blit a rectangle ((vX,vY) to (vX+vW,vY+vH)) into given dc.
//--------------------------------------------------------------------------
void wxPLDevDC::BlitRectangle( wxDC* dc, int vX, int vY, int vW, int vH )
{
    if ( m_dc )
        dc->Blit( vX, vY, vW, vH, m_dc, vX, vY );
}


//--------------------------------------------------------------------------
//  void wxPLDevDC::CreateCanvas( void )
//
//  Create canvas (bitmap and dc) if the driver provides the GUI.
//--------------------------------------------------------------------------
void wxPLDevDC::CreateCanvas()
{

}


//--------------------------------------------------------------------------
//  void wxPLDevDC::SetWidth( PLStream *pls )
//
//  Set the width of the drawing pen.
//--------------------------------------------------------------------------
void wxPLDevDC::SetWidth( PLStream *pls )
{
    m_dc->SetPen( *( wxThePenList->FindOrCreatePen( wxColour( pls->curcolor.r, pls->curcolor.g, pls->curcolor.b ),
                         pls->width > 0 ? pls->width : 1, wxPENSTYLE_SOLID ) ) );
}


//--------------------------------------------------------------------------
//  void wxPLDevDC::SetColor0( PLStream *pls )
//
//  Set color from colormap 0.
//--------------------------------------------------------------------------
void wxPLDevDC::SetColor0( PLStream *pls )
{
    m_dc->SetPen( *( wxThePenList->FindOrCreatePen( wxColour( pls->curcolor.r, pls->curcolor.g, pls->curcolor.b, pls->curcolor.a * 255 ),
                         pls->width > 0 ? pls->width : 1, wxPENSTYLE_SOLID ) ) );
    m_dc->SetBrush( wxBrush( wxColour( pls->curcolor.r, pls->curcolor.g, pls->curcolor.b, pls->curcolor.a * 255 ) ) );
}


//--------------------------------------------------------------------------
//  void wxPLDevDC::SetColor1( PLStream *pls )
//
//  Set color from colormap 1.
//--------------------------------------------------------------------------
void wxPLDevDC::SetColor1( PLStream *pls )
{
    m_dc->SetPen( *( wxThePenList->FindOrCreatePen( wxColour( pls->curcolor.r, pls->curcolor.g, pls->curcolor.b, pls->curcolor.a * 255 ),
                         pls->width > 0 ? pls->width : 1, wxPENSTYLE_SOLID ) ) );
    m_dc->SetBrush( wxBrush( wxColour( pls->curcolor.r, pls->curcolor.g, pls->curcolor.b, pls->curcolor.a * 255 ) ) );
}


//--------------------------------------------------------------------------
//  void wxPLDevDC::SetExternalBuffer( void* dc )
//
//  Adds a dc to the device. In that case, the drivers doesn't provide
//  a GUI.
//--------------------------------------------------------------------------
void wxPLDevDC::SetExternalBuffer( void* dc )
{
  wxPrintData *p=new wxPrintData();
  p->SetFilename("output.ps");
  m_dc = (wxDC*) new wxPostScriptDC(*p);
  m_dc->StartDoc("Document Started");
  
  //or could be a SVG file
//  wxSVGFileDC* svg=new wxSVGFileDC("output.svg", 1024, 680, 72);
//  m_dc=(wxDC*) svg; // new  	wxSVGFileDC ("output.svg", 1024, 680, 72);

//  m_dc->StartPage();
  
//    m_dc   = (wxDC *) dc; // Add the dc to the device
  
    ready  = true;
}


#endif
