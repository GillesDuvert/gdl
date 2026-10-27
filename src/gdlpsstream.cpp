/* *************************************************************************
                          gdlpsstream.cpp  -  graphic stream postscript
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

//#include <iostream>
#include "graphicsdevice.hpp"
#include "gdlpsstream.hpp"

using namespace std;

void GDLPSStream::Init()
{
//select the fonts in all cases...
   plstream::init();
   //AFTER init, change back to selected
   PLINT doFont = ((PLINT) SysVar::GetPFont()>-1) ? 1 : 0;
   pls->use_unicode=doFont;
   page = 0;
}
 
void GDLPSStream::eop()
{
   if (page != 0) 
   {
     if (encapsulated) 
     {
       Warning("Warning: multi-page output violates Encapsulated PostScript specification");
     }
     plstream::eop();
   }
   page++;
};
void image_compress(unsigned char *idata, PLINT size, long bpp)
{
  unsigned char byt=0x0;
  SizeT i,j,k;
//  static unsigned char mask[]={0x0,0x1,0x3,0x0,0x7};
  static short count[]={0,7,3,0,1};
  static short shiftLeft[] ={0,1,2,0,4};
  static short shiftRight[] ={0,7,6,0,4};
  bool complete=false;
  for (i=0, j=0, k=0; i<size; ++i) {
    byt=((byt<<shiftLeft[bpp]) | (idata[i]>> shiftRight[bpp]));
    if (j == count[bpp]) {idata[k++]=byt; byt=0x0; j=0; complete=true;} else {j++;complete=false;}
  }
  if (!complete) { //something left undone...
    for (int l=0; l< count[bpp]-j+1; ++l ) byt=(byt<<shiftLeft[bpp]); //pad on left
    idata[k] = byt;
  }
  return;
}
//use A85p compressor from bmp2eps package (https://www.mjr19.org.uk/sw/bmp2eps/)
#include "snippets/A85p.c"
//We need a copy of PSDev (defined in driver ps.c) 
typedef struct
{
    PLFLT pxlx, pxly;
    PLINT xold, yold;

    PLINT xmin, xmax, xlen;
    PLINT ymin, ymax, ylen;

    PLINT xmin_dev, xmax_dev, xlen_dev;
    PLINT ymin_dev, ymax_dev, ylen_dev;

    PLFLT xscale_dev, yscale_dev;

    int   llx, lly, urx, ury, ptcnt;
    // font variables.
    int   nlookup, if_symbol_font;
    int *lookup;

    // These are only used by the pstex driver for the additional
    // file required in this case
    long cur_pos;
    FILE *fp;
} PSDev;
bool GDLPSStream::PaintImage(unsigned char *idata, PLINT nx, PLINT ny, DLong *pos,
        DLong trueColorOrder, DLong channel, DLong Mode) {
  //need to : check position in file Ok; update bounding box values: IDL does not take into account images ?
  //test black and white:
  bool bw = (((*static_cast<DLongGDL*> (SysVar::D()->GetTag(SysVar::D()->Desc()->TagIndex("FLAGS"), 0)))[0] & 16) == 0); 
  if (channel > 0) {
    cerr << "TV: Value of CHANNEL (use TRUE instead) is out of allowed range." << endl;
    return false;
  } 
  
  PSDev *dev = (PSDev *) pls->dev;
  PLINT x1 = pos[0], y1 = pos[2];
  PLINT x2 = pos[1], y2 = pos[3];
  pls->bytecnt += fprintf(pls->OutFile, "\n%%BeginObject: Image\ngsave\n");
  if (portrait) {
      y1/=plsc->aspori;
      y2/=plsc->aspori;
      x1*=plsc->aspori;
      x2*=plsc->aspori;
  } else {
    pls->bytecnt += fprintf(pls->OutFile, "%d translate 270 rotate\n",plsc->phyxlen );
  }
  dev->llx = MIN(dev->llx, x1);
  dev->lly = MIN(dev->lly, y1);
  dev->urx = MAX(dev->urx, x1);
  dev->ury = MAX(dev->ury, y1);
  dev->llx = MIN(dev->llx, x2);
  dev->lly = MIN(dev->lly, y2);
  dev->urx = MAX(dev->urx, x2);
  dev->ury = MAX(dev->ury, y2);
  
  pls->bytecnt += fprintf(pls->OutFile, "%d %d translate %d %d scale\n", x1, y1, x2, y2 );
#define LINEWIDTH 80

  if (trueColorOrder == 0) { 
    if  (bw) { //black and white native value 0->255: image
      pls->bytecnt += fprintf(pls->OutFile, "%d %d %ld [%d 0 0 %d 0 0] currentfile /ASCII85Decode filter image\n", nx, ny, bitsPerPix, nx, ny);
      //if bpp is not 8, convert to, else use it directly
      if (bitsPerPix != 8) image_compress(idata,1*nx*ny,bitsPerPix);
	  pls->bytecnt +=A85p(pls->OutFile, idata,nx*ny);
    } else {
      pls->bytecnt += fprintf(pls->OutFile, "%d %d %ld [%d 0 0 %d 0 0] currentfile /ASCII85Decode filter false 3 colorimage\n", nx, ny, bitsPerPix, nx, ny);
      PLINT r[ctSize], g[ctSize], b[ctSize];
      GraphicsDevice::GetCT()->Get( r, g, b); //OUR colors, *NOT* plplot's colors (background mess)
      unsigned char *data=(unsigned char*)malloc(nx*ny*3*sizeof(unsigned char));
      //following is false for bits_per_pix not equal to 8. Colortable must be written differently. FIXME!
		for (SizeT i = 0; i < nx*ny; ++i) {data[i*3+0]=r[idata[i]];data[i*3+1]=g[idata[i]];data[i*3+2]=b[idata[i]];}
      //if bpp is not 8, convert to, else use it directly
      if (bitsPerPix != 8) image_compress(data,3*nx*ny,bitsPerPix);
	  pls->bytecnt +=A85p(pls->OutFile, data,3*nx*ny);
      free(data);
    }
  } else { //true color: native value (degrees of R, G and B)
    //if bpp is not 8, convert to, else use it directly
    if (bitsPerPix != 8) image_compress(idata,3*nx*ny,bitsPerPix);

    switch (trueColorOrder) {
      case 1:
        pls->bytecnt += fprintf(pls->OutFile, "%d %d %ld [%d 0 0 %d 0 0] currentfile /ASCII85Decode filter false 3 colorimage\n", nx, ny, bitsPerPix, nx, ny);
		pls->bytecnt +=A85p(pls->OutFile, idata,3*nx*ny);
        break;
      case 2:
        pls->bytecnt += fprintf(pls->OutFile, "/gdlImagePixStringR %d string def /gdlImagePixStringG %d string def /gdlImagePixStringB %d string def\n", nx, nx, nx);
        pls->bytecnt += fprintf(pls->OutFile, "%d %d %ld\n", nx, ny, bitsPerPix);
        pls->bytecnt += fprintf(pls->OutFile, "[%d 0 0 %d 0 0]\n{currentfile /ASCII85Decode filter gdlImagePixStringR readstring pop}\n{currentfile /ASCII85Decode filter gdlImagePixStringG readstring pop}\n{currentfile /ASCII85Decode filter gdlImagePixStringB readstring pop}\ntrue 3 colorimage\n",  nx, ny);
		for (auto line=0; line < 3*ny; line++) {
		  pls->bytecnt +=A85p(pls->OutFile, &idata[line*nx],nx);
		}
        break;
      case 3:
        pls->bytecnt += fprintf(pls->OutFile, "/gdlImagePixStringR %d string def /gdlImagePixStringG %d string def /gdlImagePixStringB %d string def\n", nx*ny, nx*ny, nx*ny);
        pls->bytecnt += fprintf(pls->OutFile, "%d %d %ld\n", nx, ny, bitsPerPix);
        pls->bytecnt += fprintf(pls->OutFile, "[%d 0 0 %d 0 0]\n{currentfile /ASCII85Decode filter gdlImagePixStringR readstring pop}\n{currentfile /ASCII85Decode filter gdlImagePixStringG readstring pop}\n{currentfile /ASCII85Decode filter gdlImagePixStringB readstring pop}\ntrue 3 colorimage\n",  nx, ny);
		for (auto plane=0; plane < 3; plane++) {
		  pls->bytecnt +=A85p(pls->OutFile, &idata[plane*nx*ny],nx*ny);
		}
		break;
      default:
        cerr<<"you should not get here!"<<endl;
    }
  }
 pls->bytecnt += fprintf(pls->OutFile, "\ngrestore\n%%EndObject: Image\n       ");
  return true;
#undef LINEWIDTH
}
