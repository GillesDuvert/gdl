/* *************************************************************************
                          gdlgstream.cpp  -  graphic stream
                             -------------------
    begin                : July 22 2002
    copyright            : (C) 2002 by Marc Schellens
    email                : m_schellens@users.sf.net
 ***************************************************************************/

/* *************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "includefirst.hpp"

#include <iostream>

#include "graphicsdevice.hpp"
#include "gdlgstream.hpp"
#include "initsysvar.hpp"
#ifndef MAX
#define MAX(a,b) ((a) < (b) ? (b) : (a))
#endif

using namespace std;

static float psCharFudge=1; //to compensate the wrong size of fonts in PS using Hershey
static float psSymFudge=1; //to compensate the wrong size of symbols in PS

void GDLGStream::Thick(DFloat thick)
{
    plstream::width(static_cast<PLFLT>(thick*thickFactor));
}

#define BLACK 0
#define WHITE 16777215
void GDLGStream::Color( ULong color, DLong decomposed) {
    bool printer = (((*static_cast<DLongGDL*> (SysVar::D()->GetTag(SysVar::D()->Desc()->TagIndex("FLAGS"), 0)))[0] & 512) == 512);
    bool bw = (((*static_cast<DLongGDL*> (SysVar::D()->GetTag(SysVar::D()->Desc()->TagIndex("FLAGS"), 0)))[0] & 16) == 0); //in that case, 
    //plplot postscript driver uses gray levels instead of colorindex, and 1 is black, not 0 !!!
    if (decomposed == 0) {
      if (printer && (color & 0xFF) == 0) { color=(bw)?WHITE:BLACK; //note that if bw other colors will be a gray value 
        GDLGStream::SetColorMap1SingleColor(color);
      } else plstream::col0(color & 0xFF); //just set color index [0..255]. simple and fast.
    } else {
      if (printer && color == 0) color=(bw)?WHITE:BLACK;
      GDLGStream::SetColorMap1SingleColor(color);
    }
}
#undef BLACK

void GDLGStream::SetColorMap1SingleColor( ULong color)
{
    PLINT red[1],green[1],blue[1];
    red[0] = color & 0xFF;
    green[0] = (color >> 8)  & 0xFF;
    blue[0]=(color >> 16) & 0xFF;
    SetColorMap1(red, green, blue, 1);
    plstream::col1(0); 
}

void GDLGStream::SetColorMap1DefaultColors(PLINT ncolors, DLong decomposed)
{
  if (decomposed == 0) { //just copy Table0 to Table1 so that scale from 0 to 1 in table 1 goes through the whole table
    PLINT r[ctSize], g[ctSize], b[ctSize];
    GraphicsDevice::GetCT()->Get( r, g, b); 
    SetColorMap1(r, g, b, ctSize); 
  } else {
    PLFLT r[2], g[2], b[2], pos[2];
    r[0] = pos[0] = 0.0;
    r[1] = pos[1] = 1.0;
    g[0] = g[1] = 0.0;
    b[0] = b[1] = 0.0;
    SetColorMap1n(ncolors);
    SetColorMap1l(TRUE,2,pos,r, g, b, NULL); 
  }
}

void GDLGStream::SetColorMap1Table( PLINT tableSize, DLongGDL *colors,  DLong decomposed)
{ //cycle on passed colors to fill tableSize.
  DLong n=colors->N_Elements();
#ifdef _MSC_VER
  PLINT *r = (PLINT*)alloca(sizeof(PLINT)*tableSize);
  PLINT *g = (PLINT*)alloca(sizeof(PLINT)*tableSize);
  PLINT *b = (PLINT*)alloca(sizeof(PLINT)*tableSize);
#else
  PLINT r[tableSize], g[tableSize], b[tableSize];
#endif
  if (decomposed == 0) {
    PLINT red[ctSize], green[ctSize], blue[ctSize], col;
    GraphicsDevice::GetCT()->Get( red, green, blue);
    for (SizeT i=0; i< tableSize; ++i) {
      col = (*colors)[i%n]& 0xFF;
      r[i] = red[col];
      g[i] = green[col];
      b[i] = blue[col];
    }
  } else {
    PLINT col;
     for (SizeT i=0; i< tableSize; ++i) {
      col = (*colors)[i%n];
      r[i] =  col        & 0xFF;
      g[i] = (col >> 8)  & 0xFF;
      b[i] = (col >> 16) & 0xFF;   
     }
  }
  SetColorMap1(r, g, b, tableSize); 
}

DLong GDLGStream::ForceColorMap1Ramp(PLFLT minlight) {
  DLong old_decomposed=GraphicsDevice::GetDevice()->GetDecomposed();
  if (old_decomposed == 0) { //just copy Table0 to Table1 so that scale from 0 to 1 in table 1 goes through the whole table
    PLINT r[ctSize], g[ctSize], b[ctSize];
    GraphicsDevice::GetCT()->Get(r, g, b);
    SetColorMap1(r, g, b, ctSize);
  } else {
  //force decomposed=false otherwise too difficult with silly plplot colortables.
  GraphicsDevice::GetDevice()->Decomposed(false);
  //fill table1 with grey ramp.
    PLFLT h[2], l[2], s[2], pos[2];
    bool rev[2];
    h[0] = h[1] = 0.0;
    l[0] = minlight;
    l[1] = 1;
    s[0] = s[1]= 0.0;
    rev[0] = rev[1] = false;
    pos[0] = 0; pos[1]=1;
    SetColorMap1n(256);
    SetColorMap1l(false,2,pos,h, l, s, rev); 
  }
  return old_decomposed;
}
#define WHITEB 255
void GDLGStream::Background( ULong color, DLong decomposed)
{
  if ((*static_cast<DLongGDL*>(SysVar::D()->GetTag(SysVar::D()->Desc()->TagIndex("FLAGS"), 0)))[0] & 512 ) {  ;//printer like PostScript
      GraphicsDevice::GetDevice()->SetDeviceBckColor(WHITEB, WHITEB, WHITEB );
   return;
  }
  DByte r,g,b;
  PLINT red,green,blue;
  if (decomposed == 0) { //just an index
    GraphicsDevice::GetCT()->Get( color & 0xFF, r, g, b);
    red=r; green=g; blue=b;
  } else {
    red = color & 0xFF;
    green = (color >> 8)  & 0xFF;
    blue = (color >> 16) & 0xFF;
  }
  GraphicsDevice::GetDevice()->SetDeviceBckColor( red, green, blue);
}
void GDLGStream::DefaultBackground()
{
  if ((*static_cast<DLongGDL*>(SysVar::D()->GetTag(SysVar::D()->Desc()->TagIndex("FLAGS"), 0)))[0] & 512 ) {  ;//printer like PostScript
    GraphicsDevice::GetDevice()->SetDeviceBckColor(WHITEB, WHITEB, WHITEB );
    return;
  }
  DStructGDL* pStruct=SysVar::P();   //MUST NOT BE STATIC, due to .reset 
  DLong background=(*static_cast<DLongGDL*>(pStruct->GetTag(pStruct->Desc()->TagIndex("BACKGROUND"), 0)))[0];
  DByte r,g,b;
  PLINT red,green,blue;
  if (GraphicsDevice::GetDevice()->GetDecomposed() == 0) { //just an index
    GraphicsDevice::GetCT()->Get( background & 0xFF, r, g, b);
    red=r; green=g; blue=b;
  } else {
    red = background & 0xFF;
    green = (background >> 8)  & 0xFF;
    blue = (background >> 16) & 0xFF;
  }
  GraphicsDevice::GetDevice()->SetDeviceBckColor( red, green, blue);
}
#undef WHITEB

void GDLGStream::SetPageDPMM(float setPsCharFudge, float setPsSymFudge) {
  //This is supposed to be called each time DPI is changed, which happens only at creation of a new device.
  // contrary to the page size, that may change for intercative devices that can be resized.
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "SetPageDPMM()\n");
  psCharFudge=setPsCharFudge;
  psSymFudge=setPsSymFudge;
  PLINT level;
  plstream::glevel(level);
  if (level <= 1) return;
  if (thePage.nbPages == 0) return;
  plstream::ssub(1, 1);
  plstream::adv(0);
  PLFLT xdpi, ydpi;
  PLINT xleng, yleng, xoff, yoff;
  plstream::gpage( xdpi, ydpi, xleng, yleng, xoff, yoff); //so-called page parameters, the units vary pixels or mm (screen vs. printers)
//  std::cerr<<"xdpi "<<xdpi<<" ydpi "<<ydpi<<std::endl;
//  std::cerr<<"xleng "<<xleng<<" yleng "<<yleng<<std::endl;
  PLFLT charHeight=pls->chrht;
//  std::cerr<<"charHeight="<<charHeight<<std::endl;
  thePage.length = xleng;
  thePage.height = yleng;
  //get the mm values using gspa:
  PLFLT bxsize_mm, bysize_mm, offx_mm, offy_mm;
  PLFLT xmin, ymin, xmax, ymax;
  plstream::gspa(xmin, xmax, ymin, ymax); //subpage in mm
  bxsize_mm = xmax - xmin;
  bysize_mm = ymax - ymin;
//  std::cerr<<"reported page size inmm "<<bxsize_mm<<","<<bysize_mm<<std::endl;
  thePage.xsizemm = bxsize_mm;
  thePage.ysizemm = bysize_mm;
  //we need to rescale all plplot values such as X_PX_CM, X_CH_SIZE, X_SIZE combinations give the same results as combining plplot values.
  //the returned xdpi and chrht is not good. We need to adjust chrht
  offx_mm = xmin;
  offy_mm = ymin;
  // test if plplot is consistent:
  PLFLT theory = xoff / xdpi * 25.4;
  if (fabs(offx_mm - theory) > 1e-4) if (GDL_DEBUG_PLSTREAM)  fprintf(stderr, "plpot driver returns inconsistent x DPI values.\n");
  theory = yoff / ydpi * 25.4;
  if (fabs(offy_mm - theory) > 1e-4) if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "plpot driver returns inconsistent y DPI values.\n");
  theory = xleng / xdpi * 25.4;
  if (fabs(bxsize_mm - theory) > 1e-4) if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "plpot driver returns inconsistent x DPI values (2).\n");
  theory = yleng / ydpi * 25.4;
  if (fabs(bysize_mm - theory) > 1e-4) if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "plpot driver returns inconsistent y DPI values (2).\n");
  //we can derive the dpm in x and y which converts mm to device coords:
  thePage.xdpmm = xdpi/25.4; //abs(thePage.length / bxsize_mm);
  thePage.ydpmm = ydpi/25.4; //abs(thePage.height / bysize_mm);
}

bool GDLGStream::updatePageInfo() {
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "updatePageInfo():\n");
  if (thePage.nbPages == 0) {
    if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "            FAILED\n");
    return false;
  }
  long xsize, ysize;
  GetGeometry(xsize, ysize);
  if (thePage.length == xsize && thePage.height == ysize) return true;
  thePage.length = xsize;
  thePage.height = ysize;
  (*static_cast<DLongGDL*> (SysVar::D()->GetTag(SysVar::D()->Desc()->TagIndex("X_SIZE"), 0)))[0] = xsize;
  (*static_cast<DLongGDL*> (SysVar::D()->GetTag(SysVar::D()->Desc()->TagIndex("Y_SIZE"), 0)))[0] = ysize;
  (*static_cast<DLongGDL*> (SysVar::D()->GetTag(SysVar::D()->Desc()->TagIndex("X_VSIZE"), 0)))[0] = xsize;
  (*static_cast<DLongGDL*> (SysVar::D()->GetTag(SysVar::D()->Desc()->TagIndex("Y_VSIZE"), 0)))[0] = ysize;
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "             %fx%f device units.\n", thePage.length, thePage.height);
  return true;
}

void GDLGStream::CurrentCharSize(PLFLT scale) {
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "CurrentCharsize()\n");
  if (gdlDefaultCharInitialized == 0) {
    if (updatePageInfo() == true) {
      GetPlplotDefaultCharSize();
    }
  }
  theCurrentChar.scale = scale;
  theCurrentChar.ndsx = scale * theDefaultChar.ndsx;
  theCurrentChar.ndsy = scale * theDefaultChar.ndsy;
  theCurrentChar.dsx = scale * theDefaultChar.dsx;
  theCurrentChar.dsy = scale * theDefaultChar.dsy;
  theCurrentChar.mmsx = scale * theDefaultChar.mmsx;
  theCurrentChar.mmsy = scale * theDefaultChar.mmsy;
  theCurrentChar.wsx = scale * theDefaultChar.wsx;
  theCurrentChar.wsy = scale * theDefaultChar.wsy;
  theCurrentChar.mmspacing = scale * theDefaultChar.mmspacing;
  theCurrentChar.nspacing = scale * theDefaultChar.nspacing;
  theCurrentChar.dspacing = scale * theDefaultChar.dspacing;
  theCurrentChar.wspacing = scale * theDefaultChar.wspacing;
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "            sized by %f is %fx%f mm or %fx%f device or %fx%f world\n", scale, theCurrentChar.mmsx, theCurrentChar.mmsy, theCurrentChar.dsx, theCurrentChar.dsy, theCurrentChar.wsx, theCurrentChar.wsy);
}

void GDLGStream::UpdateCurrentCharWorldSize() {
  PLFLT x, y, dx, dy;
  DeviceToWorld(0, 0, x, y);
  DeviceToWorld(theDefaultChar.dsx, theDefaultChar.dsy, dx, dy);
  theDefaultChar.wsx = abs(dx - x);
  theDefaultChar.wsy = abs(dy - y);
  theCurrentChar.wsx = theCurrentChar.scale * theDefaultChar.wsx;
  theCurrentChar.wsy = theCurrentChar.scale * theDefaultChar.wsy;

  DeviceToWorld(0, theDefaultChar.dspacing, dx, dy);
  theDefaultChar.wspacing = abs(dy - y);

  if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "UpdateCurrentCharWorldSize(%f,%f)\n",
    theCurrentChar.wsx, theCurrentChar.wsy);
}

void GDLGStream::updateBoxDeviceCoords() {
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "updateBoxDeviceCoords()\n");
  // world coordinates of current subpage boundaries and page boundaries
  NormedDeviceToWorld(0.0, 0.0, theBox.pageWorldCoordinates[0], theBox.pageWorldCoordinates[2]);
  NormedDeviceToWorld(1.0, 1.0, theBox.pageWorldCoordinates[1], theBox.pageWorldCoordinates[3]);
  NormToWorld(0.0, 0.0, theBox.subPageWorldCoordinates[0], theBox.subPageWorldCoordinates[2]);
  NormToWorld(1.0, 1.0, theBox.subPageWorldCoordinates[1], theBox.subPageWorldCoordinates[3]);
  NormToDevice(theBox.nx1, theBox.ny1, theBox.dx1, theBox.dy1);
  NormToDevice(theBox.nx2, theBox.ny2, theBox.dx2, theBox.dy2);
  theBox.dxsize=theBox.dx2-theBox.dx1;
  theBox.dysize=theBox.dy2-theBox.dy1;
}
void GDLGStream::syncPageInfo()
{
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "SyncPageInfo()\n");
  PLINT level;
  plstream::glevel(level);
  if (level > 1 && thePage.nbPages != 0) //we need to have a vpor defined, and a page!
  {
    thePage.subpage.dxoff = 0; //our subpages have 0 offset
    thePage.subpage.dyoff = 0;
    thePage.subpage.dxsize = thePage.length / thePage.nx;
    thePage.subpage.dysize = thePage.height / thePage.ny;
    if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "         subpage is %fx%f at [%f,%f] device units\n",
      thePage.subpage.dxsize, thePage.subpage.dysize, thePage.subpage.dxoff, thePage.subpage.dyoff);

  } else if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "       WARNING: not initalized\n");
}
void GDLGStream::DefaultCharSize() {
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"GDLGStream::DefaultCharSize()\n");
  DStructGDL* d = SysVar::D();
  DStructDesc* s = d->Desc();
  unsigned X_CH_SIZE = s->TagIndex("X_CH_SIZE");
  unsigned Y_CH_SIZE = s->TagIndex("Y_CH_SIZE");
  DLong ichx = (*static_cast<DLongGDL*> (d->GetTag(X_CH_SIZE, 0)))[0];
  DLong ichy = (*static_cast<DLongGDL*> (d->GetTag(Y_CH_SIZE, 0)))[0];
  PLFLT chx=ichx; //needed as floats for subsequent computations!
  PLFLT chy=ichy;
  unsigned FLAGS = s->TagIndex("FLAGS");
  DLong flags = (*static_cast<DLongGDL*> (d->GetTag(FLAGS, 0)))[0];
  if (flags & 0x1) {
    unsigned X_PX_CM = s->TagIndex("X_PX_CM");
    unsigned Y_PX_CM = s->TagIndex("Y_PX_CM");
    DFloat xpxcm = (*static_cast<DFloatGDL*> (d->GetTag(X_PX_CM, 0)))[0];
    DFloat ypxcm = (*static_cast<DFloatGDL*> (d->GetTag(Y_PX_CM, 0)))[0];
    setVariableCharacterSize(chx, 1.0, chy,xpxcm,ypxcm);
  } else {
    setFixedCharacterSize(chx, 1.0, chy);
  }
}
void GDLGStream::SetCharSize(DLong ichx, DLong chy) {
  PLFLT chx=ichx;
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"GDLGStream::SetCharSize()\n");
  DStructGDL* d = SysVar::D();
  DStructDesc* s = d->Desc();
  unsigned FLAGS = s->TagIndex("FLAGS");
  DLong flags = (*static_cast<DLongGDL*> (d->GetTag(FLAGS, 0)))[0];
  if (flags & 0x1) {
    unsigned X_PX_CM = s->TagIndex("X_PX_CM");
    unsigned Y_PX_CM = s->TagIndex("Y_PX_CM");
    DFloat xpxcm = (*static_cast<DFloatGDL*> (d->GetTag(X_PX_CM, 0)))[0];
    DFloat ypxcm = (*static_cast<DFloatGDL*> (d->GetTag(Y_PX_CM, 0)))[0];
    setVariableCharacterSize(chx, 1.0, chy,xpxcm,ypxcm);
  } else {
    setFixedCharacterSize(chx, 1.0, chy);
  }
}
 
  void GDLGStream::GetPlplotDefaultCharSize()
  {
        
    if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"GetPlPlotDefaultCharsize()\n");
    if (thePage.nbPages==0)   {return;}
    //dimensions in normalized, device and millimetres
    if (gdlDefaultCharInitialized==1) {if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"     Already initialized\n"); return;}
    theDefaultChar.scale=1.0;
    theDefaultChar.mmsx=pls->chrht; //millimeter
    theDefaultChar.mmsy=pls->chrht;
    theDefaultChar.ndsx=mm2ndx(theDefaultChar.mmsx); //normalized device
    theDefaultChar.ndsy=mm2ndy(theDefaultChar.mmsy);
    theDefaultChar.dsy=theDefaultChar.ndsy*thePage.height;
    theDefaultChar.dsx=theDefaultChar.ndsx*thePage.length;
    theDefaultChar.mmspacing=theLineSpacing_in_mm;
    theDefaultChar.nspacing=mm2ndy(theDefaultChar.mmspacing);
    theDefaultChar.dspacing=theDefaultChar.nspacing*thePage.height;
    theDefaultChar.wspacing=mm2wy(theDefaultChar.mmspacing);
    
    theDefaultChar.wsx=mm2wx(theDefaultChar.mmsx); //world
    theDefaultChar.wsy=mm2wy(theDefaultChar.mmsy);
    if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"             %fx%f,%f (mm)\n",theDefaultChar.mmsx   ,theDefaultChar.mmsy ,theDefaultChar.mmspacing);
    if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"             %fx%f,%f (norm)\n",theDefaultChar.ndsx ,theDefaultChar.ndsy ,theDefaultChar.nspacing);
    if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"             %fx%f,%f (dev)\n",theDefaultChar.dsx   ,theDefaultChar.dsy  ,theDefaultChar.dspacing);
    if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"             %fx%f,%f (world)\n",theDefaultChar.wsx ,theDefaultChar.wsy  ,theDefaultChar.wspacing);
    gdlDefaultCharInitialized=1;
}

void GDLGStream::NextPlot(bool erase) {

  DLongGDL* pMulti = SysVar::GetPMulti();

  DLong nx = (*pMulti)[ 1];
  DLong ny = (*pMulti)[ 2];
  DLong nz = (*pMulti)[ 3];
  DLong dir = (*pMulti)[ 4];

  nx = (nx > 0) ? nx : 1;
  ny = (ny > 0) ? ny : 1;
  nz = (nz > 0) ? nz : 1;
  DLong nsub = nx * ny * nz;

  if (GDL_DEBUG_PLSTREAM) fprintf(stderr, "NextPlot(erase=%d)\n", erase);
  //this code reproduces the behaviour of IDL seen in:
  // !P.MULTI=[0,3,2] & data=dist(10) & (repeat many times: plot,data & plot,data,col='ff'x,/noe )
  // the previous code would not erase the screen correctly after 6 commands.
  DLong pMod = (*pMulti)[0] % (nsub);
  ssub(nx, ny, nz);
  if (dir == 0) {
	adv(nsub - pMod + 1);
  } else {
	int p = nsub - pMod;
	int pp = p * nx % (nx * ny) + p / ny + 1;
	adv(pp);
  }
  if (!erase) return;

  if (pMod == 0) // clear and restart to first subpage
  {
	eop(); // overridden (for Z-buffer)
	//get background value (*not pen 0*, we try to avoid plplot's silly behaviour).
	//use it for bop(), then reset the pen 0 to correct value.

	PLINT red, green, blue;
	DByte r, g, b;
	PLINT red0, green0, blue0;

	GraphicsDevice::GetCT()->Get(0, r, g, b);
	red = r;
	green = g;
	blue = b;

	red0 = GraphicsDevice::GetDevice()->BackgroundR();
	green0 = GraphicsDevice::GetDevice()->BackgroundG();
	blue0 = GraphicsDevice::GetDevice()->BackgroundB();
	plstream::scolbg(red0, green0, blue0); //overwrites col[0]
	plstream::bop(); // note: changes charsize
	plstream::scolbg(red, green, blue); //resets col[0]
	adv(1); //advance to first subpage
	(*pMulti)[0] = nsub - 1; //set PMULTI[0] to this page
  } else {
	if (dir == 0) {
	  //      plstream::adv(nx*ny - pMod + 1);
	  adv(nsub - pMod + 1);
	} else {
	  int p = nsub - pMod;
	  int pp = p * nx % (nx * ny) + p / ny + 1;
	  //      plstream::adv(pp);
	  adv(pp);
	}
	--(*pMulti)[0];
  }

}

void GDLGStream::NoSub()
{
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"NoSub()\n");
  ssub( 1, 1); // changes charsize ?
//plstream::adv( 0);
  adv( 0);
//  DefaultCharSize();
}


// default is a wrapper for gpage(). Is overriden by, e.g., X driver.
void GDLGStream::GetGeometry( long& xSize, long& ySize)
{
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"GDLGStream::GetGeometry()\n");
  PLFLT xp; PLFLT yp; 
  PLINT xleng; PLINT yleng;
  PLINT plxoff; PLINT plyoff;
  plstream::gpage( xp, yp, xleng, yleng, plxoff, plyoff); //for X-Window, wrapper give sizes from X11, not plplot which seems bugged.
  // for PostScript, Page size is FIXED (720x540) and GDLPSStream::GetGeometry replies correctly
  
//since the page sizes for PS and EPS images are processed by GDL after plplot finishes 
//its work, gpage will not output correct sizes 
  xSize = xleng;
  ySize = yleng;
  if (xSize<1.0||ySize<1) //plplot gives back crazy values! z-buffer for example!
  {
    PLFLT xmin,xmax,ymin,ymax;
    plstream::gspa(xmin,xmax,ymin,ymax); //subpage in mm
    xSize=min(1.0,xmax-xmin);
    ySize=min(1.0,ymax-ymin);
  }
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"    found (%ld %ld)\n", xSize, ySize);

}

void GDLGStream::setSymbolSize( PLFLT scale )
{
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"setSymbolScale(%f)\n",scale);
//  plstream::ssym(0.0, scale);
  theCurrentSymSize=scale*psSymFudge;
}

void GDLGStream::setLineSpacing(PLFLT newSpacing)
{
  theLineSpacing_in_mm=newSpacing;
}
PLFLT GDLGStream::getSymbolSize(){return theCurrentSymSize;}
void GDLGStream::mtex( const char *side, PLFLT disp, PLFLT posit, PLFLT just,
                       const char *text, double *stringCharLength, double *stringCharHeight)
{
  *stringCharLength=gdlGetStringLengthInMillimetres(text)/thePage.xsizemm;
  plstream::mtex(side,disp,posit,just,text);
  if (stringCharHeight!=NULL) *stringCharHeight = 1;
}

void GDLGStream::ptex( PLFLT x, PLFLT y, PLFLT dx, PLFLT dy, PLFLT just,
                       const char *text , double *stringCharLength)
{
    *stringCharLength=gdlGetStringLengthInMillimetres(text)/thePage.xsizemm;
    plstream::ptex(x,y,dx,dy,just,text);
}

//This defines the character size for SCALABLE character devices (POSTSCRIPT, SVG))
//The dimension of "average" character (given by X_CH_SIZE) is to be a physical (mm) size.
//this implies to have a correct value for DPI AND that the plplot driver is correctly written.
void GDLGStream::setVariableCharacterSize( PLFLT charwidthpixel, PLFLT scale , PLFLT lineSpacingpixel, PLFLT xpxcm, PLFLT ypxcm)
{
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"setVariableCharacterSize()\n");
  //tried by comparison of outputs of 
  // "set_plot,'ps' & !P.multi=[0,2,2]&a=dist(5)&for i=1,4 do begin&s=i*0.7& plot,a,psym=6,syms=s,chars=s,xtit="XXXX" $
  //  & xyouts,indgen(25),a,"M",ali=0.5,chars=s & end & !p.multi=0 & plots,[0.001,0.001,0.999,0.999,0.001],$
  //  [0.001,0.999,0.999,0.001,0.001],/norm & device,/close & set_plot,'x'

 if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"schr(width=%f, scale=%f, spacing=%f, xpxcm=%f, ypxcm=%f)\n",charwidthpixel,scale,lineSpacingpixel,xpxcm,ypxcm);
   PLFLT xdpi=xpxcm*INCHToCM;
   PLFLT ydpi=ypxcm*INCHToCM;
// GDL asks for pixels, plplot asks for mm size, but plplot's dpi is always wrong!
// to get 'charwidthpixel' with this 'wrongdpi', wee need to ask a (wrong) mm size of the character HEIGHT, 
// BUT we do not know the height/width ratio of the font used, plus the fact that some drivers make the wrong calculation!!!!
// AND!!! the expected pixel size (X_CH_SIZE) is the true pixel on screen (or paper), not the fake one
  PLFLT expectedheight_in_mm=charwidthpixel/xdpi*INCHToMM*DEFAULT_FONT_ASPECT_RATIO; //start with a height/width of FONT_ASPECT_RATIO (guessed) will be updated later.
 if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"current (fake?) dpi is %f : asking for a mm height size: %f)\n",ydpi, expectedheight_in_mm);
   plstream::schr(expectedheight_in_mm, 1); 
//trick: if 'em' is not 0, we have the character real width, in mm. It is assumed that when 0, then the size is OK
//if not 0, then we know the height/width ratio and can recompute the 'good' height that will give the 'good' width (in pixels) 
   PLFLT em=0;
    em=gdlGetStringLengthInMillimetres(ALLCHARACTERSFORSTRINGLENGTHTEST)/ALLCHARACTERSFORSTRINGLENGTHTEST_NCHARS; //mean of all
    if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"Able to check character width=%f, should have been %f\n",em, charwidthpixel/xdpi*INCHToMM);
  if (em > 0) {
    PLFLT ratio=charwidthpixel/xdpi*INCHToMM/em;
    plstream::schr(expectedheight_in_mm*ratio, 1);
   if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"got plplot character height in mm=%f (2nd pass))\n",pls->chrdef);
//    em=gdlGetStringLength(PATTERN)/PATTERN_LENGTH; //mean of all
//    ratio=charwidthpixel/xdpi*INCHToMM/em;
//    if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"re-check character width=%f, ratio is %f\n",em, ratio);
  }
  setLineSpacing(lineSpacingpixel/ydpi*INCHToMM); //this one is NOT related to characters idiosyncrasies.
  gdlDefaultCharInitialized=0; //reset Default
  CurrentCharSize(scale);
}


//This defines the character size for FIXED character devices (X, Z )
//The dimension of "average" character (given by X_CH_SIZE) is to be a number of pixels on screen
void GDLGStream::setFixedCharacterSize( PLFLT charwidthpixel, PLFLT scale , PLFLT lineSpacingpixel) {
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"setFixedCharacterSize()\n");
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"schr(width=%f, scale=%f, spacing=%f)\n",charwidthpixel,scale,lineSpacingpixel);
// GDL asks for pixels, plplot asks for mm size, but plplot's dpi is always wrong!
// to get 'charwidthpixel' with this 'wrongdpi', wee need to ask a (wrong) mm size of the character HEIGHT, 
// BUT we do not know the height/width ratio of the font used, plus the fact that some drivers make the wrong calculation!!!!
// AND!!! the expected pixel size (X_CH_SIZE) is the true pixel on screen (or paper), not the fake one
  PLFLT expectedheight=charwidthpixel/pls->xdpi*INCHToMM*DEFAULT_FONT_ASPECT_RATIO; //start with a height/width of FONT_ASPECT_RATIO (guessed) will be updated later.
 if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"current (fake?) dpi is %f : asking for a mm height size: %f)\n",pls->ydpi, expectedheight);
   plstream::schr(expectedheight, 1); 
//trick: if 'em' is not 0, we have the character real width, in mm. It is assumed that when 0, then the size is OK
//if not 0, then we know the height/width ratio and can recompute the 'good' height that will give the 'good' width (in pixels) 
   PLFLT em=0;
    em=gdlGetStringLengthInMillimetres(ALLCHARACTERSFORSTRINGLENGTHTEST)/ALLCHARACTERSFORSTRINGLENGTHTEST_NCHARS; //mean of all
    if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"Able to check character width=%f, should have been %f\n",em, charwidthpixel/pls->xdpi*INCHToMM);
  if (em > 0) {
    PLFLT ratio=charwidthpixel/pls->xdpi*INCHToMM/em;
    plstream::schr(expectedheight*ratio, 1);
   if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"got plplot character height in mm=%f (2nd pass))\n",pls->chrdef);
//    em=gdlGetStringLength(PATTERN)/PATTERN_LENGTH; //mean of all
//    ratio=charwidthpixel/pls->xdpi*INCHToMM/em;
//    if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"re-check character width=%f, ratio is %f, in pixels:%f\n",em, ratio, em/INCHToMM*pls->xdpi);
  }
 setLineSpacing(lineSpacingpixel/pls->ydpi*INCHToMM); //this one is NOT related to characters idiosyncrasies.
  gdlDefaultCharInitialized=0; //reset Default
  CurrentCharSize(scale);
}

void GDLGStream::sizeChar( PLFLT scale )
{
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"SizeChar(%f)\n",scale);
  plstream::schr(theDefaultChar.mmsx, scale*psCharFudge); //must FORCE a new size.
  CurrentCharSize(scale*psCharFudge);
}

bool GDLGStream::vpor(PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax )
{
  //make vpor units really min max, otherwise som problems appear spuriously
  // if (xmin > xmax || ymin > ymax) return true; // invalid: see isue #1387
  if (xmin >= xmax) {xmin=0; xmax=1;} //#1387
  if (ymin >= ymax) {ymin=0; ymax=1;} //#1387
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"vpor(): requesting x[%f:%f],y[%f:%f] (normalized, subpage)\n",xmin,xmax,ymin,ymax);
  //note that plplot apparently does not write the y=0 line of pixels (in device coords). IDL page is on the contrary limited to
  // [0..1[ in both axes (normalized coordinates)
  plstream::vpor(xmin, xmax, ymin, ymax);
  theBox.nx1=xmin;
  theBox.nx2=xmax;
  theBox.ny1=ymin;
  theBox.ny2=ymax;
  theBox.ndx1=xmin;
  theBox.ndx2=xmax;
  theBox.ndy1=ymin;
  theBox.ndy2=ymax;
  theBox.nxsize=xmax-xmin;
  theBox.nysize=ymax-ymin;
  theBox.initialized=true;
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"vpor(): got x[%f:%f],x[%f:%f] (normalized, device)\n",theBox.ndx1,theBox.ndx2,theBox.ndy1,theBox.ndy2);
  syncPageInfo();
  return false;
}

//returns true if positioning problem
bool GDLGStream::isovpor(PLFLT x1, PLFLT x2, PLFLT y1,  PLFLT y2,  PLFLT aspect)
{
  if (aspect <= 0.0) {
    return vpor(x1, x2, y1, y2);
  }
  if (x2 <= x1 || y2 <= y1) return true;
  //x1 < x2 && y1 < y2 implied
  PLFLT x1mm = nd2mx(x1);
  PLFLT y1mm = nd2my(y1);
  PLFLT x2mm = nd2mx(x2);
  PLFLT y2mm = nd2my(y2);
  PLFLT ys = y2mm - y1mm; //x and y are in normalized coordinates. ISO scaling must be performed using screen (or paper) coordinates:
  PLFLT xs = x2mm - x1mm;
  PLFLT page_aspect=ys/xs;
  aspect=page_aspect/aspect;
   if (aspect > 1) { //x ok, resize y
    y2mm = y1mm + ys/aspect;
  } else {
    x2mm = x1mm + xs*aspect;
  }
  x1 = mm2ndx(x1mm);
  x2 = mm2ndx(x2mm);
  y1 = mm2ndy(y1mm);
  y2 = mm2ndy(y2mm);
  return vpor(x1, x2, y1, y2);
}

void GDLGStream::wind( PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax )
{
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"wind(): setting x[%f:%f],y[%f:%f] (world) \n",xmin,xmax,ymin,ymax);
  //silly test to protect against plplot warnings ... side effects unkonwn.
  if (xmin==xmax) {xmin=0; xmax=1;}
  if (ymin==ymax) {ymin=0; ymax=1;}
  plstream::wind(xmin, xmax, ymin, ymax);
  theBox.wx1=xmin;
  theBox.wx2=xmax;
  theBox.wy1=ymin;
  theBox.wy2=ymax;
  updateBoxDeviceCoords();
  UpdateCurrentCharWorldSize();
  setSymbolSizeConversionFactors(); //because symbols are written in world coordinates. 
}

void GDLGStream::wind( PLFLT xmin, PLFLT xmax, bool xLog, PLFLT ymin, PLFLT ymax, bool yLog )
{
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"wind(): setting x[%f:%f],y[%f:%f] (world) \n",xmin,xmax,ymin,ymax);
  if (xLog) {
    xmin = log10(xmin);
    xmax = log10(xmax);
  }
  if (yLog) {
    ymin = log10(ymin);
    ymax = log10(ymax);
  }
  if (xmin==xmax) {xmin=0; xmax=1;}
  if (ymin==ymax) {ymin=0; ymax=1;}
  plstream::wind(xmin, xmax, ymin, ymax);
  theBox.wx1=xmin;
  theBox.wx2=xmax;
  theBox.wy1=ymin;
  theBox.wy2=ymax;
  updateBoxDeviceCoords();
  UpdateCurrentCharWorldSize();
  setSymbolSizeConversionFactors(); //because symbols are written in world coordinates. 
}
void GDLGStream::ssub(PLINT nx, PLINT ny, PLINT nz)
{
//  plstream::ssub( nx, ny ); // does not appear to change charsize.

  // set subpage numbers in X and Y
  thePage.nbPages=nx*ny*nz;
  thePage.nx=nx;
  thePage.ny=ny;
  thePage.nz=nz;
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"ssub() %dx%dx%d pages\n",nx,ny,nz);
  thePage.curPage=1;
  syncPageInfo();
}

void GDLGStream::adv(PLINT page)
{
//  plstream::adv(page); //plstream below soes not advance pages as it has only ONE.
  if (page==0) {thePage.curPage++;} else {thePage.curPage=page;}
  if (thePage.curPage > thePage.nbPages) thePage.curPage=1;
  if (GDL_DEBUG_PLSTREAM) fprintf(stderr,"adv() now at page %d\n",thePage.curPage);
  PLFLT sxmin,symin,sxmax,symax,szmin,szmax;
  getSubpageRegion(&sxmin,&symin,&sxmax,&symax,&szmin,&szmax);
  //SET ALL REGION TAGS
  unsigned regionTag=SysVar::X()->Desc()->TagIndex("REGION");
  (*static_cast<DFloatGDL*>(SysVar::X()->GetTag(regionTag, 0)))[0]=sxmin;
  (*static_cast<DFloatGDL*>(SysVar::X()->GetTag(regionTag, 0)))[1]=sxmax;
  regionTag=SysVar::Y()->Desc()->TagIndex("REGION");
  (*static_cast<DFloatGDL*>(SysVar::Y()->GetTag(regionTag, 0)))[0]=symin;
  (*static_cast<DFloatGDL*>(SysVar::Y()->GetTag(regionTag, 0)))[1]=symax;
  regionTag=SysVar::Z()->Desc()->TagIndex("REGION");
  (*static_cast<DFloatGDL*>(SysVar::Z()->GetTag(regionTag, 0)))[0]=szmin;
  (*static_cast<DFloatGDL*>(SysVar::Z()->GetTag(regionTag, 0)))[1]=szmax;
}

void GDLGStream::getSubpageRegion(PLFLT *sxmin, PLFLT *symin, PLFLT *sxmax, PLFLT *symax, PLFLT *szmin, PLFLT *szmax) {
  //here we must take into account the contents of ![X|Y|Z].OMARGIN
  unsigned omarginTag = SysVar::X()->Desc()->TagIndex("OMARGIN");
  DFloat xstart = (*static_cast<DFloatGDL*> (SysVar::X()->GetTag(omarginTag, 0)))[0];
  xstart=MAX(xstart,0);
  DFloat xend = (*static_cast<DFloatGDL*> (SysVar::X()->GetTag(omarginTag, 0)))[1];
  xend=MAX(xend,0);
  omarginTag = SysVar::Y()->Desc()->TagIndex("OMARGIN");
  DFloat ystart = (*static_cast<DFloatGDL*> (SysVar::Y()->GetTag(omarginTag, 0)))[0];
  ystart=MAX(ystart,0);
  DFloat yend = (*static_cast<DFloatGDL*> (SysVar::Y()->GetTag(omarginTag, 0)))[1];
  yend=MAX(yend,0);
  //  Z OMARGIN to BE CHECKED and code must be written.
  //  omarginTag = SysVar::Z()->Desc()->TagIndex("OMARGIN");
  //  DFloat zstart = (*static_cast<DFloatGDL*> (SysVar::Z()->GetTag(omarginTag, 0)))[0];
  //  DFloat zend = (*static_cast<DFloatGDL*> (SysVar::Z()->GetTag(omarginTag, 0)))[1];
  DFloat xNormedPageSize=1-(xend+xstart)*theCurrentChar.ndsx;
  DFloat yNormedPageSize=1-(yend+ystart)*theCurrentChar.nspacing;
  if (xNormedPageSize < 0 || xNormedPageSize > 1 || yNormedPageSize < 0 || yNormedPageSize > 1) {
	Message("Data coordinate system not established.");
	if (xNormedPageSize < 0) xNormedPageSize=0;
	if (yNormedPageSize < 0) yNormedPageSize=0;
	if (xNormedPageSize > 1) xNormedPageSize=1;
	if (yNormedPageSize > 1) yNormedPageSize=1;
  } 
  DFloat zNormedPageSize=1; //zend-zstart??;
  DFloat xNormedOffset=xstart*theCurrentChar.ndsx;
  DFloat yNormedOffset=yend*theCurrentChar.nspacing;
  //check silly values: normed must be >0 and < 1
  int p=thePage.curPage-1;
  PLFLT width=xNormedPageSize/thePage.nx;
  PLFLT height=yNormedPageSize/thePage.ny;
  PLFLT profund=zNormedPageSize/thePage.nz;
 int k= p / (thePage.nx*thePage.ny);
 int l= p - k*(thePage.nx*thePage.ny);
 int j= l /thePage.nx ;
 int i= (l - j*thePage.nx);
 *sxmin=i*width+xNormedOffset;
 *sxmax=*sxmin+width;
 *symax=1-(j*height+yNormedOffset);
 *symin=*symax-height;
 if (szmin != NULL) {
   *szmin=k*profund;
   *szmax=*szmin+profund;
 }
}

void GDLGStream::compute3DCubeLimits(PLFLT &xratio, PLFLT &yratio, PLFLT &zratio, PLFLT* displacement) {
  int p=thePage.curPage-1;
  int nx = thePage.nx;
  int ny = thePage.ny;
  xratio=1.0/thePage.nx;
  yratio=1.0/thePage.ny;
  zratio=1.0/thePage.nz;
 int k= p / (thePage.nx*thePage.ny);
 int l= p - k*(thePage.nx*thePage.ny);
 int j= l /thePage.nx ;
 int i= (l - j*thePage.nx);
 displacement[0]=i*xratio;
 displacement[1]=1-yratio-j*yratio;
 displacement[2]=k*zratio;
}

  //get region (3BPP data)

int GDLGStream::GetRegion(DLong& xoff, DLong& yoff, DLong& nPixelsX, DLong& nPixelsY) {
  long nxOrig,nyOrig;
  this->GetGeometry(nxOrig,nyOrig);
  if (nPixelsX <= 0) return 1;
  int xmin=xoff;
  int xmax = xoff + nPixelsX - 1;
  if (xmax < 0 || xmin > nxOrig - 1) return 1;
  
  if (nPixelsY <= 0) return 1;
  int ymin=yoff;
  int ymax = yoff + nPixelsY - 1;
  if (ymax < 0 || ymin > nyOrig - 1) return 1;
 
  if ( xmax > nxOrig - 1) xmax = nxOrig - 1;
  if ( xmin < 0 ) xmin=0;
  if ( xmin > xmax) return 1;
  
  if ( ymax > nyOrig - 1) ymax = nyOrig - 1;
  if ( ymin < 0 ) ymin=0;
  if ( ymin > ymax) return 1;
  //give back updated number of pixels! important!
  nPixelsX=xmax-xmin+1;
  nPixelsY=ymax-ymin+1;
  DByteGDL *bitmap = static_cast<DByteGDL*> (this->GetBitmapData(xmin,ymin,nPixelsX,nPixelsY));
  if (bitmap == NULL) return 2; //need to GDLDelete bitmap on exit after this line.
  DByte* bmp=static_cast<DByte*>(bitmap->DataAddr());

  GraphicsDevice* actDevice = GraphicsDevice::GetDevice();
  unsigned char* data = actDevice->SetCopyBuffer(nPixelsX * nPixelsY * 3);
  for (auto k=0; k< nPixelsX*nPixelsY*3; ++k) data[k] = bmp[k];
  GDLDelete(bitmap);
  return 0; //0 is OK
}

bool GDLGStream::SetRegion(DLong& xs, DLong& ys, DLong& nx, DLong& ny){
  DLong pos[4]={xs,nx,ys,ny};
  GraphicsDevice* actDevice = GraphicsDevice::GetDevice();
  return this->PaintImage(actDevice->GetCopyBuffer(), nx, ny, pos, 1, 0);  
}
//
////plplot main functions patched for 3d
//#include "plplotdriver/plutils.h"
