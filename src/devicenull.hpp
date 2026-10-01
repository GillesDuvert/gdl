/* *************************************************************************
                          devicenull.hpp  -  NULL device
                             -------------------
    begin                : 20 February 2014
    copyright            : (C) 2012 by Alain Coulais
    email                : alaingdl@users.sf.net
 ***************************************************************************/

/* *************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef DEVICENULL_HPP_
#define DEVICENULL_HPP_
#include "gdlnullstream.hpp"
class DeviceNULL : public GraphicsDevice
{
   GDLNULLStream*   actStream;
  void DeleteStream()
  {
    delete actStream; actStream = NULL;
  }

  void InitStream()
  {
    DeleteStream();

    // always allocate the buffer with creating a new stream
    actStream = new GDLNULLStream( 100, 100);
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
  
  GDLGStream* GetStream( bool open=true)
  {
    if( actStream == NULL) 
      {
	InitStream();
      }
    return actStream;
  }
  
public:
  //  DeviceNULL(): GraphicsDevice(), fileName( "gdl.null"), actStream( NULL)
  DeviceNULL(): GraphicsDevice(), actStream( NULL)
  {
    name = "NULL";

    DLongGDL origin( dimension( 2));
    DLongGDL zoom( dimension( 2));
    zoom[0] = 1;
    zoom[1] = 1;

    dStruct = new DStructGDL( "!DEVICE");
    dStruct->InitTag("NAME",       DStringGDL( name)); 
    dStruct->InitTag("X_SIZE",     DLongGDL( 1000)); 
    dStruct->InitTag("Y_SIZE",     DLongGDL( 1000)); 
    dStruct->InitTag("X_VSIZE",    DLongGDL( 1000)); 
    dStruct->InitTag("Y_VSIZE",    DLongGDL( 1000)); 
    dStruct->InitTag("X_CH_SIZE",  DLongGDL( 8)); 
    dStruct->InitTag("Y_CH_SIZE",  DLongGDL( 13)); 
    dStruct->InitTag("X_PX_CM",    DFloatGDL( 13.0)); 
    dStruct->InitTag("Y_PX_CM",    DFloatGDL( 13.0)); 
    dStruct->InitTag("N_COLORS",   DLongGDL( 256)); 
    dStruct->InitTag("TABLE_SIZE", DLongGDL( 256)); 
    dStruct->InitTag("FILL_DIST",  DLongGDL( 1)); 
    dStruct->InitTag("WINDOW",     DLongGDL( -1)); 
    dStruct->InitTag("UNIT",       DLongGDL( 0)); 
    dStruct->InitTag("FLAGS",      DLongGDL( 16)); 
    dStruct->InitTag("ORIGIN",     origin); 
    dStruct->InitTag("ZOOM",       zoom); 
  }
  ~DeviceNULL()
  {
    DeleteStream();
  }
  virtual DLong GetDecomposed()  final {return 1; }
  virtual DString GetCurrentFont() final                 {return "__$";}
  virtual DLong GetGraphicsFunction() final                 { return -1;}
  virtual DIntGDL* GetPageSize() final                      { return NULL;}
  virtual DInt GetPixelDepth() final                       { return -1;}
  virtual bool SetPixelDepth(DInt depth) final               { return true;}
  virtual bool Decomposed( bool value) final                { return true;}
  virtual BaseGDL* GetFontnames() final                  { return NULL;}
  virtual DLong GetFontnum() final                        { return 0;}
  virtual DLong GetVisualDepth() final                      { return -1;}
  virtual DString GetVisualName() final                     { return "";}
  virtual DIntGDL* GetWindowPosition() final                { return NULL;}
  virtual DLong GetWriteMask() final                        { return -1;}
  virtual DByteGDL* WindowState() final                     { return NULL;}
  virtual bool CloseFile() final                            { return true;}
  virtual bool SetFileName( const std::string& f) final     { return true;}
  virtual bool SetGraphicsFunction( DLong value) final      { return true;}
  virtual bool CursorStandard( int value) final             { return true;}
  virtual bool CursorCrosshair(bool standard=false) final   { return true;}
  virtual bool CursorImage(char* v, int x=0, int y=0, char* mask=NULL) final   { return true;}
  virtual int  getCursorId() final                             { return -1;}
  virtual bool UnsetFocus() final                           { return true;}
  virtual bool SetBackingStore(int value) final             { return true;}
  virtual int  getBackingStore() final                      { return -1;}
  virtual bool SetXPageSize( const float xs) final          { return true;}
  virtual bool SetYPageSize( const float ys) final          { return true;}
  virtual bool SetColor(const long color=0) final           { return true;}
  virtual bool SetScale(const float) final                  { return true;}
  virtual bool SetXOffset(const float) final                { return true;}
  virtual bool SetYOffset(const float) final                { return true;}
  virtual bool SetPortrait() final                          { return true;}
  virtual bool SetLandscape() final                         { return true;}
  virtual bool SetEncapsulated(bool val) final              { return true;}
  virtual bool SetBPP(const int bpp) final                  { return true;}
  virtual bool Hide() final                                 { return true;}
  virtual bool CopyRegion(DLongGDL* me) final               { return true;}

  // Z buffer device
  virtual bool ZBuffering( bool yes) final                  { return true;}
  virtual bool SetResolution( DLong nx, DLong ny) final     { return true;}
};

#endif
