// Copyright (C) 2008  Werner Smekal
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
#include "deprecated_wxwidgets.h"
//define LINE2D, POLYLINE2D
#define LINE2D dummy_line
#define POLYLINE2D dummy_polyline
#include "plplot3d.h"

#include <wx/string.h>
#include <string>

// only compile code if wxGraphicsContext available
#if wxUSE_GRAPHICS_CONTEXT

wxPLDevGC::wxPLDevGC( void ) : wxPLDevBase( wxBACKEND_GC )
{
    m_dc       = NULL;
    m_bitmap   = NULL;
    m_context  = NULL;
    underlined = false;
}


wxPLDevGC::~wxPLDevGC()
{

    delete m_context;
}


void wxPLDevGC::DrawLine( short x1a, short y1a, short x2a, short y2a )
{
    wxDouble       x1 = x1a / scalex;
    wxDouble       y1 = height - y1a / scaley;
    wxDouble       x2 = x2a / scalex;
    wxDouble       y2 = height - y2a / scaley;

    wxGraphicsPath path = m_context->CreatePath();
    path.MoveToPoint( x1, y1 );
    path.AddLineToPoint( x2, y2 );
    m_context->StrokePath( path );

    AddtoClipRegion( (int) x1, (int) y1, (int) x2, (int) y2 );
}


//--------------------------------------------------------------------------
//  void wxPLDevGC::DrawPolyline( short *xa, short *ya, PLINT npts )
//
//  Draw a poly line - coordinates are in the xa and ya arrays.
//--------------------------------------------------------------------------
void wxPLDevGC::DrawPolyline( short *xa, short *ya, PLINT npts )
{

    wxGraphicsPath path = m_context->CreatePath();
    path.MoveToPoint( xa[0] / scalex, height - ya[0] / scaley );
    for ( PLINT i = 1; i < npts; i++ )
        path.AddLineToPoint( xa[i] / scalex, height - ya[i] / scaley );
    m_context->StrokePath( path );

    wxDouble x, y, w, h;
    path.GetBox( &x, &y, &w, &h );
    AddtoClipRegion( (int) x, (int) y, (int) ( x + w ), (int) ( y + h ) );
}


void wxPLDevGC::ClearBackground( PLINT bgr, PLINT bgg, PLINT bgb, PLINT x1, PLINT y1, PLINT x2, PLINT y2 )
{
    // Log_Verbose( "%s", __FUNCTION__ );

    wxDouble x1a, y1a, x2a, y2a;

    if ( x1 < 0 )
        x1a = 0;
    else
        x1a = x1 / scalex;
    if ( y1 < 0 )
        y1a = 0;
    else
        y1a = height - y1 / scaley;
    if ( x2 < 0 )
        x2a = width;
    else
        x2a = x2 / scalex;
    if ( y2 < 0 )
        y2a = height;
    else
        y2a = height - y2 / scaley;

    m_context->SetPen( *( wxThePenList->FindOrCreatePen( wxColour( bgr, bgg, bgb ), 1, wxPENSTYLE_SOLID ) ) );
    m_context->SetBrush( wxBrush( wxColour( bgr, bgg, bgb ) ) );
    m_context->DrawRectangle( x1a, y1a, x2a - x1a, y2a - y1a );

    m_context->SetPen( *( wxThePenList->FindOrCreatePen( wxColour( mColorRedStroke, mColorGreenStroke,
                                  mColorBlueStroke, mStrokeOpacity ),
                              1, wxPENSTYLE_SOLID ) ) );
    m_context->SetBrush( wxBrush( wxColour( mColorRedFill, mColorGreenFill, mColorBlueFill, mStrokeOpacity ) ) );

    AddtoClipRegion( (int) x1a, (int) y1a, (int) x2a, (int) y2a );
}

void wxPLDevGC::FillPolygon(PLStream *pls) {
  // Log_Verbose( "%s", __FUNCTION__ );

  bool isRect = false;
  short* x = pls->dev_x;
  short* y = pls->dev_y;

  if (pls->dev_npts == 4) // Check if it's a rectangle. If so, it can be made faster to display
  {
    if (x[0] == x[1] && x[2] == x[3] && y[0] == y[3] && y[1] == y[2])
      isRect = true;
    else if (x[0] == x[3] && x[1] == x[2] && y[0] == y[1] && y[2] == y[3])
      isRect = true;
  }
  if (pls->dev_npts == 5) {
    if (x[0] == x[4] && y[0] == y[4]) {
      if (x[0] == x[1] && x[2] == x[3] && y[0] == y[3] && y[1] == y[2])
        isRect = true;
      else if (x[0] == x[3] && x[1] == x[2] && y[0] == y[1] && y[2] == y[3])
        isRect = true;
    }
  }

  if (isRect) //isRect) {
  {
    double x1, y1, x2, y2, x0, y0, w, h;

    x1 = x[0] / scalex;
    x2 = x[2] / scalex;
    y1 = height - y[0] / scaley;
    y2 = height - y[2] / scaley;

    if (x1 < x2) {
      x0 = x1;
      w = x2 - x1;
    } else {
      x0 = x2;
      w = x1 - x2;
    }
    if (y1 < y2) {
      y0 = y1;
      h = y2 - y1;
    } else {
      y0 = y2;
      h = y1 - y2;
    }
    m_context->DrawRectangle(x0, y0, w, h);
    AddtoClipRegion((int) x0, (int) y0, (int) w, (int) h);
  } else {
    wxGraphicsPath path = m_context->CreatePath();
    path.MoveToPoint(x[0] / scalex, height - y[0] / scaley);
    for (int i = 1; i < pls->dev_npts; i++)
      path.AddLineToPoint(x[i] / scalex, height - y[i] / scaley);
    path.CloseSubpath();

    if (pls->dev_eofill)
      m_context->DrawPath(path, wxODDEVEN_RULE);
    else
      m_context->DrawPath(path, wxWINDING_RULE);

    wxDouble bx, by, bw, bh;
    path.GetBox(&bx, &by, &bw, &bh);

    AddtoClipRegion((int) bx, (int) by, (int) (bx + bw), (int) (by + bh));
  }
}

// paths need to be in ints, not shorts, as shorts may overflow in this case. 
void wxPLDevGC::FillPolygons(PLStream *pls) {
  // Log_Verbose( "%s", __FUNCTION__ );

  wxGraphicsPath path = m_context->CreatePath();
  for (int i = 0; i < pls->dev_npath; ++i) {
    PLINT* x = pls->dev_pathx[i];
    PLINT* y = pls->dev_pathy[i];
    path.MoveToPoint(x[0] / scalex, height - y[0] / scaley);
    for (int j = 1; j < pls->dev_pathnxy[i]; ++j) {
      switch (x[j]) {
        case -1: //line
          path.AddLineToPoint(x[j+1] / scalex, height - y[j+1] / scaley);
          j++;
          break;
        case -2:
          path.AddQuadCurveToPoint(x[j+1] / scalex, height - y[j+1]/ scaley, x[j+2] / scalex, height - y[j+2] / scaley);
          j+=2;
          break;
        case -3:
          path.AddCurveToPoint(x[j+1] / scalex, height - y[j+1] / scaley, x[j+2] / scalex, height - y[j+2] / scaley, x[j+3] / scalex, height - y[j+3] / scaley);
          j+=3;
          break;
          break;
        default:
          printf("should not happen in FillPolygons(%d), please report!\n",x[j]);
      }
    }
    path.CloseSubpath();
  }
  if (pls->dev_eofill)
    m_context->DrawPath(path, wxODDEVEN_RULE);
  else
    m_context->DrawPath(path, wxWINDING_RULE);

  wxDouble bx, by, bw, bh;
  path.GetBox(&bx, &by, &bw, &bh);
  AddtoClipRegion((int) bx, (int) by, (int) (bx + bw), (int) (by +bh));
}
void wxPLDevGC::BlitRectangle( wxDC* dc, int vX, int vY, int vW, int vH )
{
    // Log_Verbose( "%s", __FUNCTION__ );

    if ( m_dc )
        dc->Blit( vX, vY, vW, vH, m_dc, vX, vY );
}


void wxPLDevGC::CreateCanvas()
{
    // Log_Verbose( "%s", __FUNCTION__ );

    if ( m_dc )
    {
        delete m_context;
        m_context = wxGraphicsContext::Create( *( (wxMemoryDC *) m_dc ) );
        // see what procedure PROFILES do.
    }
}


void wxPLDevGC::SetWidth( PLStream *pls )
{
    // Log_Verbose( "%s", __FUNCTION__ );

    m_context->SetPen( *( wxThePenList->FindOrCreatePen( wxColour( mColorRedStroke, mColorGreenStroke,
                                  mColorBlueStroke, mStrokeOpacity ),
                              pls->width > 0 ? pls->width : 1, wxPENSTYLE_SOLID ) ) );
}


void wxPLDevGC::SetColor0( PLStream *pls )
{
    // Log_Verbose( "%s", __FUNCTION__ );

    mColorRedStroke   = pls->curcolor.r;
    mColorGreenStroke = pls->curcolor.g;
    mColorBlueStroke  = pls->curcolor.b;
    mColorRedFill     = pls->curcolor.r;
    mColorGreenFill   = pls->curcolor.g;
    mColorBlueFill    = pls->curcolor.b;
    mStrokeOpacity    = (unsigned char) ( pls->curcolor.a * 255 );

    m_context->SetPen( *( wxThePenList->FindOrCreatePen( wxColour( mColorRedStroke, mColorGreenStroke,
                                  mColorBlueStroke, mStrokeOpacity ),
                              pls->width > 0 ? pls->width : 1, wxPENSTYLE_SOLID ) ) );
    m_context->SetBrush( wxBrush( wxColour( mColorRedFill, mColorGreenFill, mColorBlueFill, mStrokeOpacity ) ) );
}


void wxPLDevGC::SetColor1( PLStream *pls )
{
    // Log_Verbose( "%s", __FUNCTION__ );

    mColorRedStroke   = pls->curcolor.r;
    mColorGreenStroke = pls->curcolor.g;
    mColorBlueStroke  = pls->curcolor.b;
    mColorRedFill     = pls->curcolor.r;
    mColorGreenFill   = pls->curcolor.g;
    mColorBlueFill    = pls->curcolor.b;
    mStrokeOpacity    = (unsigned char) ( pls->curcolor.a * 255 );

    m_context->SetPen( *( wxThePenList->FindOrCreatePen( wxColour( mColorRedStroke, mColorGreenStroke,
                                  mColorBlueStroke, mStrokeOpacity ),
                              pls->width > 0 ? pls->width : 1, wxPENSTYLE_SOLID ) ) );
    m_context->SetBrush( wxBrush( wxColour( mColorRedFill, mColorGreenFill, mColorBlueFill, mStrokeOpacity ) ) );
}


//--------------------------------------------------------------------------
//  void wx_set_dc( PLStream* pls, wxDC* dc )
//
//  Adds a dc to the stream. The associated device is attached to the canvas
//  as the property "dev".
//--------------------------------------------------------------------------
void wxPLDevGC::SetExternalBuffer( void* dc )
{
    // Log_Verbose( "%s", __FUNCTION__ );

    m_dc      = (wxDC *) dc; // Add the dc to the device
    m_context = wxGraphicsContext::Create( *( (wxMemoryDC *) m_dc ) );
  
// something possible with wxWidgets 3.3.3: write a SVG   
//  wxSVGFileDC* svg=new wxSVGFileDC("output.svg", 1024, 680, 72);
//  m_dc=(wxDC*) svg; // new  	wxSVGFileDC ("output.svg", 1024, 680, 72);
//  m_context = svg->GetGraphicsContext 	( 		) 	;
 // 
    char* do_antialias = getenv("GDL_DO_ANTIALIASING");
    if (do_antialias == NULL) m_context->SetAntialiasMode(wxANTIALIAS_NONE); //GD May 2022 force no antialias as antialiasing prevents erasing lines by redrawing them ontop by a color 0.
	//NOTE: antialiasing and no double buffer makes plots very slow.
    ready     = true;
//    ownGUI    = false;
}



#endif
