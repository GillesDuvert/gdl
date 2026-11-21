/* *************************************************************************
                          deviceps.hpp  -  postscript device
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

#ifndef DEVICEPS_HPP_
#define DEVICEPS_HPP_

#include "gdlpsstream.hpp"
#include "plotting.hpp" // get_axis_crange for TV()
#include "initsysvar.hpp"

#include "objects.hpp"

#undef MIN
#define MIN(a,b) ((a) > (b) ? (b) : (a))
#undef MAX
#define MAX(a,b) ((a) < (b) ? (b) : (a))

  static const float PS_CM2IN = (1.0 / INCHToCM) ;
  static const PLFLT PS_DPI = 72.0 ; //in dpi;
  static const float PS_RESOL = 1000.0;

class DevicePS: public GraphicsDevice
{
  std::string fileName;
  GDLPSStream* actStream;
  float XPageSize;
  float YPageSize;
  float XOffset;
  float YOffset;
  int color;
  int decomposed; // false -> use color table
  bool orient_portrait;
  bool encapsulated;
  int bitsPerPix;
  float scale;

  GDLStream  *psUnit;

  void InitStream()
{
		delete actStream;

		DLongGDL* pMulti = SysVar::GetPMulti();
		DLong nx = (*pMulti)[ 1];
		DLong ny = (*pMulti)[ 2];

		if (nx <= 0) nx = 1;
		if (ny <= 0) ny = 1;
		actStream = new GDLPSStream(nx, ny, encapsulated, color, bitsPerPix, orient_portrait);

		actStream->sfnam(fileName.c_str());
		// !D.UNIT should contain the unit opened for the postscript file:
		// bug report 3611898 "I use (fstat(!d.unit)).name to determine the filename of the current postscript file in some of my routines."
		// Solved AC 29-Avril-2013: just open the file here as a fileUnit, as long as the filename is the same in the driver, it will be the same file.
		DLong lun=GetLUN();
		psUnit = &fileUnits[ lun-1];
		psUnit->Open(fileName,fstream::out,false,false,false,defaultStreamWidth,false,false);
		(*static_cast<DLongGDL*>( dStruct->GetTag(dStruct->Desc()->TagIndex("UNIT"))))[0]=lun;

		actStream->spage(PS_DPI, PS_DPI, XPageSize * 72 * PS_CM2IN, YPageSize * 72 * PS_CM2IN, XOffset * 72 * PS_CM2IN, YOffset * 72 * PS_CM2IN);

		// no pause on destruction
		actStream->spause(false);

		PLINT r[ctSize], g[ctSize], b[ctSize];
		actCT.Get(r, g, b);
		actStream->SetColorMap0(r, g, b, ctSize);
		actStream->SetColorMap1(r, g, b, ctSize);
		actStream->scolbg(255, 255, 255); // start with a white background
		
		actStream->Init();

		// need to be called initially. permit to fix things
		actStream->plstream::ssub(1, 1); // plstream below stays with ONLY ONE page
		actStream->plstream::adv(0); //-->this one is the 1st and only pladv
		actStream->plstream::vpor(0, 1, 0, 1);
		actStream->plstream::wind(0, 1, 0, 1);

		actStream->ssub(1, 1);
		actStream->SetPageDPMM();
		actStream->DefaultCharSize();
		actStream->adv(0); //this is for us (counters) //needs DefaultCharSize
	}
	
public:
  DevicePS(): 
  GraphicsDevice()
  , fileName( "gdl.ps")
  , actStream( NULL)
  , XPageSize(17.78)
  , YPageSize(12.7)
  , XOffset(1.905)
  , YOffset(12.7)
   //IDL default for offsets: 54 pts /X and 360 pts/Y
  , color(0)
  , decomposed( 0)
  , orient_portrait(true)
  , encapsulated(false)
  , bitsPerPix(8)
  , scale(1.)
  {
    name = "PS";

    DLongGDL origin( dimension( 2));
    DLongGDL zoom( dimension( 2));
    zoom[0] = 1;
    zoom[1] = 1;

    dStruct = new DStructGDL( "!DEVICE");
    dStruct->InitTag("NAME",       DStringGDL( name)); 
    dStruct->InitTag("X_SIZE",     DLongGDL( XPageSize*scale*PS_RESOL)); 
    dStruct->InitTag("Y_SIZE",     DLongGDL( YPageSize*scale*PS_RESOL));
    dStruct->InitTag("X_VSIZE",    DLongGDL( XPageSize*scale*PS_RESOL));
    dStruct->InitTag("Y_VSIZE",    DLongGDL( YPageSize*scale*PS_RESOL));
    dStruct->InitTag("X_CH_SIZE",  DLongGDL( 222));
    dStruct->InitTag("Y_CH_SIZE",  DLongGDL( 352));
    dStruct->InitTag("X_PX_CM",    DFloatGDL( PS_RESOL)); 
    dStruct->InitTag("Y_PX_CM",    DFloatGDL( PS_RESOL)); 
    dStruct->InitTag("N_COLORS",   DLongGDL( 16777216)); 
    dStruct->InitTag("TABLE_SIZE", DLongGDL( 256)); 
    dStruct->InitTag("FILL_DIST",  DLongGDL( 1));
    dStruct->InitTag("WINDOW",     DLongGDL( -1)); 
    dStruct->InitTag("UNIT",       DLongGDL( 0)); 
    dStruct->InitTag("FLAGS",      DLongGDL( 266791)); //266807 if color, 266791 if monochrome.
    dStruct->InitTag("ORIGIN",     origin); 
    dStruct->InitTag("ZOOM",       zoom);

    SetPortrait();
  }
  
  ~DevicePS()
  {
    delete actStream;
  }

  GDLGStream* GetStream( bool open=true)
  {
    if( actStream == NULL) 
      {
	if( !open) return NULL;
	InitStream();
      }
    return actStream;
  }

  bool SetFileName( const std::string& f)
  {
    fileName = f;
    return true;
  }

  bool CloseFile()
  {
    // trying to solve bug report 3611898
    // this is needed to decrement Lun number ...
    (*static_cast<DLongGDL*>( dStruct->GetTag(dStruct->Desc()->TagIndex("UNIT"))))[0]=0;
    if (actStream != NULL)
    {
      delete actStream;
      actStream = NULL;
      psUnit->Close();
      psUnit->Free();
      psUnit = NULL;
    }
    return true;
  }

  bool SetXOffset( const float xo) // xo [cm]
  {
    XOffset=xo;
    return true;
  }

  bool SetYOffset( const float yo) // yo [cm]
  {
    YOffset=yo;
    return true;
  }

  bool SetXPageSize( const float xs) // xs [cm]
  {
    XPageSize=xs;
    (*static_cast<DLongGDL*>(dStruct->GetTag(dStruct->Desc()->TagIndex("X_SIZE"))))[0] 
      = DLong(floor(0.5+
        xs * (*static_cast<DFloatGDL*>(dStruct->GetTag(dStruct->Desc()->TagIndex("X_PX_CM"))))[0]
      ));
   (*static_cast<DLongGDL*>(dStruct->GetTag(dStruct->Desc()->TagIndex("X_VSIZE"))))[0]
      = DLong(floor(0.5+
        xs * (*static_cast<DFloatGDL*>(dStruct->GetTag(dStruct->Desc()->TagIndex("X_PX_CM"))))[0]
      ));
    return true;
  }

  bool SetYPageSize( const float ys) // ys [cm]
  {
    YPageSize=ys;
    (*static_cast<DLongGDL*>(dStruct->GetTag(dStruct->Desc()->TagIndex("Y_SIZE"))))[0] 
      = DLong(floor(0.5+
        ys * (*static_cast<DFloatGDL*>(dStruct->GetTag(dStruct->Desc()->TagIndex("Y_PX_CM"))))[0]
      ));
    (*static_cast<DLongGDL*>(dStruct->GetTag(dStruct->Desc()->TagIndex("Y_VSIZE"))))[0]
      = DLong(floor(0.5+
        ys * (*static_cast<DFloatGDL*>(dStruct->GetTag(dStruct->Desc()->TagIndex("Y_PX_CM"))))[0]
      ));
    return true;
  }

  bool SetColor(const long hascolor)
  {
    if (hascolor==1) color=1; else color=0;
      if (hascolor==1) 
      {
        DLong FLAG=(*static_cast<DLongGDL*>( dStruct->GetTag(dStruct->Desc()->TagIndex("FLAGS"))))[0];
        (*static_cast<DLongGDL*>( dStruct->GetTag(dStruct->Desc()->TagIndex("FLAGS"))))[0]=FLAG|16; //set colored device
      } else {
      DLong FLAG=(*static_cast<DLongGDL*>( dStruct->GetTag(dStruct->Desc()->TagIndex("FLAGS"))))[0];
        (*static_cast<DLongGDL*>( dStruct->GetTag(dStruct->Desc()->TagIndex("FLAGS"))))[0]=FLAG&(~16); //set monochrome device
      }
    return true;
  }

  bool SetPortrait()
  {
      // no need to update !D
    orient_portrait = true;
//    nb: IDL defaults to:
    SetXPageSize(7 * INCHToCM);
    SetYPageSize(5 * INCHToCM);
    SetXOffset(.75 * INCHToCM);
    SetYOffset(5 * INCHToCM); 
    return true;
  }

  bool SetLandscape()
  {
      // no need to update !D
    orient_portrait = false;
    SetXPageSize(9.5 * INCHToCM);
    SetYPageSize(7.0 * INCHToCM);
    SetXOffset(.75 * INCHToCM);
    SetYOffset(10.25 * INCHToCM);
    return true;
  }

  bool SetScale(float value)
  {
      //no effect for postscript in IDL up to 8 (?)
    scale = value;
    return true;
  }

  bool SetEncapsulated(bool val)
  {
    encapsulated = val;
    return true;
  }
  
  bool SetBPP(const int val)
  {
    int bpp = MAX(MIN(8,val),1);
    if (bpp > 4) bpp = 8;
    else if (bpp > 2) bpp = 4;
    bitsPerPix = bpp;
    return true;
  }
  
  bool Decomposed( bool value)           
  {   
    decomposed = value;
    if (decomposed==1) (*static_cast<DLongGDL*>( dStruct->GetTag(dStruct->Desc()->TagIndex("N_COLORS"))))[0]=256*256*256;
    else (*static_cast<DLongGDL*>( dStruct->GetTag(dStruct->Desc()->TagIndex("N_COLORS"))))[0]=256;
    return true;
  }

  DLong GetDecomposed()        
  {
    return decomposed;  
  }

  DIntGDL* GetPageSize()
  {
    DIntGDL* res;
    res = new DIntGDL(2, BaseGDL::NOZERO);
    (*res)[0]= XPageSize*PS_DPI*PS_CM2IN;
    (*res)[1]= YPageSize*PS_DPI*PS_CM2IN;
    return res;
  }
};

#endif
