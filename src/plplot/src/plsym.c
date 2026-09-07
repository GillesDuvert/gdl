//      Point, symbol, and string plotting routines.
//      Also font management code.  See the description of plLibOpen() for
//      the search path used in finding the font files.
//
// Copyright (C) 1992 Geoffrey Furnish
// Copyright (C) 1993-2002 Maurice LeBrun
// Copyright (C) 1996 Rady Shouman
// Copyright (C) 2000-2019 Alan W. Irwin
// Copyright (C) 2001 Joao Cardoso
// Copyright (C) 2002 Vince Darley
// Copyright (C) 2003-2005 Rafael Laboissiere
// Copyright (C) 2004-2005 Andrew Roach
// Copyright (C) 2004-2011 Andrew Ross
// Copyright (C) 2005 Thomas Duck
// Copyright (C) 2006-2010 Hazen Babcock
// Copyright (C) 2009 Werner Smekal
// Copyright (C) 2010 Hezekiah M. Carty
// Copyright (C) 2015 Phil Rosenberg
// Copyright (C) 2015 jdishaw
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

//! @file
//!
//! Point-, symbol-, and string-plotting routines.
//!

#ifndef __PLSYM_H__
#define __PLSYM_H__
#define DEBUG
#define NEED_PLDEBUG

#include "plplotP.h"
#include <float.h>
#include <ctype.h>
#include "hershey_mapping.h"
#define STB_TRUETYPE_IMPLEMENTATION 
#include "stb_truetype.h"

extern const char* findFontPath(const char* name);

//for ntohl etc
#if defined(_WIN32) && !defined(__CYGWIN__)
#include <winsock2.h>
#else 
#include <arpa/inet.h>
#endif

// Declarations
#define NUMBERHERSHEYFONTS 20 
#define MAXTTFONTS 100
struct CTAB {
	unsigned char nvecs;
	unsigned char width;
	short offset;
};
typedef struct CTAB CTAB;
static int32_t *HersheyFontTableDirectory=NULL;
static unsigned char hersheyNumberChars[NUMBERHERSHEYFONTS]={}; 
static struct CTAB *hersheyFontLookupStruct[NUMBERHERSHEYFONTS]={}; 
static short int *hersheyFontVectors[NUMBERHERSHEYFONTS]={};
static stbtt_fontinfo* ttfVectors[MAXTTFONTS]={0};
static float charHeightCorr[MAXTTFONTS]={0};
static int   charDescent[MAXTTFONTS]={0};
static short int   *fntlkup;
static long   *fntindx;
static signed char *fntbffr;
static short int   numberfonts, numberchars;
static long   indxleng;

static short       fontloaded = 0;

#define STLEN       250

//static PLUNICODE symbol_buffer[PLMAXSTR];
static signed char xygrid[STLEN];

int hershey2unicode( int in );
int text2num( PLCHAR_VECTOR text, char end, PLUNICODE *num );

// Static function prototypes

static void
pldeco( PLUNICODE *sym, PLINT *length, PLCHAR_VECTOR text);
static void
plhershey( short *xygrid, int len, PLFLT * const xform, 
        PLINT refx, PLINT refy, PLFLT scale, PLFLT xpmm, PLFLT ypmm,
        PLFLT *p_xorg, PLFLT *p_yorg, PLFLT width );
static void
plttf( stbtt_vertex *vects, int descent, int len, PLFLT * const xform, 
        PLINT refx, PLINT refy, PLFLT scale, PLFLT xpmm, PLFLT ypmm,
        PLFLT *p_xorg, PLFLT *p_yorg, PLFLT width);

static void
plttf2( stbtt_vertex *vects, int descent, int len, PLFLT * const xform, 
        PLINT refx, PLINT refy, PLFLT scale, PLFLT xpmm, PLFLT ypmm,
        PLFLT *p_xorg, PLFLT *p_yorg, PLFLT width, void* userdata);

//--------------------------------------------------------------------------
// void plmtex()
//
// Prints out "text" at specified position relative to viewport
// (may be inside or outside)
//
// side	String which is one of the following:
//     B or b  :  Bottom of viewport
//     T or t  :  Top of viewport
//     BV or bv : Bottom of viewport, vertical text
//     TV or tv : Top of viewport, vertical text
//     L or l  :  Left of viewport
//     R or r  :  Right of viewport
//     LV or lv : Left of viewport, vertical text
//     RV or rv : Right of viewport, vertical text
//
// disp Displacement from specified edge of viewport, measured outwards from
//	the viewport in units of the current character height. The
//	centerlines of the characters are aligned with the specified
//	position.
//
// pos	Position of the reference point of the string relative to the
//	viewport edge, ranging from 0.0 (left-hand edge) to 1.0 (right-hand
//	edge)
//
// just	Justification of string relative to reference point
//	just = 0.0 => left hand edge of string is at reference
//	just = 1.0 => right hand edge of string is at reference
//	just = 0.5 => center of string is at reference
//--------------------------------------------------------------------------

void
c_plmtex( PLCHAR_VECTOR side, PLFLT disp, PLFLT pos, PLFLT just,
          PLCHAR_VECTOR text )
{
    PLINT clpxmi, clpxma, clpymi, clpyma;
    PLINT vert, refx, refy, x, y;
    PLFLT xdv, ydv, xmm, ymm, refxmm, refymm, shift, xform[4];
    PLFLT chrdef, chrht;
    PLFLT dispx, dispy;

    if ( plsc->level < 2 )
    {
        plabort( "plmtex: Please set up viewport first" );
        return;
    }

// Open clip limits to subpage limits

    plP_gclp( &clpxmi, &clpxma, &clpymi, &clpyma ); // get and store current clip limits
    plP_sclp( plsc->sppxmi, plsc->sppxma, plsc->sppymi, plsc->sppyma );

    if ( plP_stindex( side, "BV" ) != -1 || plP_stindex( side, "bv" ) != -1 )
    {
        vert  = 1;
        xdv   = plsc->vpdxmi + ( plsc->vpdxma - plsc->vpdxmi ) * pos;
        ydv   = plsc->vpdymi;
        dispx = 0;
        dispy = -disp;
    }
    else if ( plP_stindex( side, "TV" ) != -1 || plP_stindex( side, "tv" ) != -1 )
    {
        vert  = 1;
        xdv   = plsc->vpdxmi + ( plsc->vpdxma - plsc->vpdxmi ) * pos;
        ydv   = plsc->vpdyma;
        dispx = 0;
        dispy = disp;
    }
    else if ( plP_stsearch( side, 'b' ) )
    {
        vert  = 0;
        xdv   = plsc->vpdxmi + ( plsc->vpdxma - plsc->vpdxmi ) * pos;
        ydv   = plsc->vpdymi;
        dispx = 0;
        dispy = -disp;
    }
    else if ( plP_stsearch( side, 't' ) )
    {
        vert  = 0;
        xdv   = plsc->vpdxmi + ( plsc->vpdxma - plsc->vpdxmi ) * pos;
        ydv   = plsc->vpdyma;
        dispx = 0;
        dispy = disp;
    }
    else if ( plP_stindex( side, "LV" ) != -1 || plP_stindex( side, "lv" ) != -1 )
    {
        vert  = 0;
        xdv   = plsc->vpdxmi;
        ydv   = plsc->vpdymi + ( plsc->vpdyma - plsc->vpdymi ) * pos;
        dispx = -disp;
        dispy = 0;
    }
    else if ( plP_stindex( side, "RV" ) != -1 || plP_stindex( side, "rv" ) != -1 )
    {
        vert  = 0;
        xdv   = plsc->vpdxma;
        ydv   = plsc->vpdymi + ( plsc->vpdyma - plsc->vpdymi ) * pos;
        dispx = disp;
        dispy = 0;
    }
    else if ( plP_stsearch( side, 'l' ) )
    {
        vert  = 1;
        xdv   = plsc->vpdxmi;
        ydv   = plsc->vpdymi + ( plsc->vpdyma - plsc->vpdymi ) * pos;
        dispx = -disp;
        dispy = 0;
    }
    else if ( plP_stsearch( side, 'r' ) )
    {
        vert  = 1;
        xdv   = plsc->vpdxma;
        ydv   = plsc->vpdymi + ( plsc->vpdyma - plsc->vpdymi ) * pos;
        dispx = disp;
        dispy = 0;
    }
    else
    {
        plP_sclp( clpxmi, clpxma, clpymi, clpyma ); // restore initial clip limits
        return;
    }

// Transformation matrix

    if ( vert != 0 )
    {
        xform[0] = 0.0;
        xform[1] = -1.0;
        xform[2] = 1.0;
        xform[3] = 0.0;
    }
    else
    {
        xform[0] = 1.0;
        xform[1] = 0.0;
        xform[2] = 0.0;
        xform[3] = 1.0;
    }

// Convert to physical units (mm) and compute shifts

    plgchr( &chrdef, &chrht );
    shift = ( just == 0.0 ) ? 0.0 : plstrl( text ) * just;

    xmm    = plP_dcmmx( xdv ) + dispx * chrht;
    ymm    = plP_dcmmy( ydv ) + dispy * chrht;
    refxmm = xmm - shift * xform[0];
    refymm = ymm - shift * xform[2];

// Convert to device units (pixels) and call text plotter

    x    = plP_mmpcx( xmm );
    y    = plP_mmpcy( ymm );
    refx = plP_mmpcx( refxmm );
    refy = plP_mmpcy( refymm );

    plP_text( 0, just, xform, x, y, refx, refy, text );
    plP_sclp( clpxmi, clpxma, clpymi, clpyma ); // restore clip limits
}

//--------------------------------------------------------------------------
// void plptex()
//
// Prints out "text" at world cooordinate (wx,wy). The text may be
// at any angle "angle" relative to the horizontal. The parameter
// "just" adjusts the horizontal justification of the string:
//	just = 0.0 => left hand edge of string is at (wx,wy)
//	just = 1.0 => right hand edge of string is at (wx,wy)
//	just = 0.5 => center of string is at (wx,wy) etc.
//--------------------------------------------------------------------------

void
c_plptex( PLFLT wx, PLFLT wy, PLFLT dx, PLFLT dy, PLFLT just, PLCHAR_VECTOR text )
{
    PLINT x, y, refx, refy;
    PLFLT xdv, ydv, xmm, ymm, refxmm, refymm, shift, cc, ss;
    PLFLT xform[4], diag;
    PLFLT chrdef, chrht;
    PLFLT dispx, dispy;
    PLFLT wxt, wyt, dxt, dyt;

    if ( plsc->level < 3 )
    {
        plabort( "plptex: Please set up window first" );
        return;
    }

    // Transform both the origin and offset values
    TRANSFORM( wx, wy, &wxt, &wyt );
    TRANSFORM( wx + dx, wy + dy, &dxt, &dyt );
    dxt = dxt - wxt;
    dyt = dyt - wyt;
    if ( dxt == 0.0 && dyt == 0.0 )
    {
        dxt = 1.0;
        dyt = 0.0;
    }

    cc   = plsc->wmxscl * dxt;
    ss   = plsc->wmyscl * dyt;
    diag = sqrt( cc * cc + ss * ss );
    cc  /= diag;
    ss  /= diag;

    xform[0] = cc;
    xform[1] = -ss;
    xform[2] = ss;
    xform[3] = cc;

    xdv = plP_wcdcx( wxt );
    ydv = plP_wcdcy( wyt );

    dispx = 0.;
    dispy = 0.;

// Convert to physical units (mm) and compute shifts

    plgchr( &chrdef, &chrht );
    shift = ( just == 0.0 ) ? 0.0 : plstrl( text ) * just;

    xmm    = plP_dcmmx( xdv ) + dispx * chrht;
    ymm    = plP_dcmmy( ydv ) + dispy * chrht;
    refxmm = xmm - shift * xform[0];
    refymm = ymm - shift * xform[2];

    x    = plP_mmpcx( xmm );
    y    = plP_mmpcy( ymm );
    refx = plP_mmpcx( refxmm );
    refy = plP_mmpcy( refymm );

//    plP_text( 0, just, xform, x, y, refx, refy, text );
    plstr(text,0, 0, just, xform, x, y, refx, refy );
}

//--------------------------------------------------------------------------
// void plstr()
//
// Prints out a "string" at reference position with physical coordinates
// (refx,refy). The coordinates of the vectors defining the string are
// passed through the linear mapping defined by the 2 x 2 matrix xform()
// before being plotted.  The reference position is at the left-hand edge of
// the string. If base = 1, it is aligned with the baseline of the string.
// If base = 0, it is aligned with the center of the character box.
//
// Note, all calculations are done in terms of millimetres. These are scaled
// as necessary before plotting the string on the page.
//--------------------------------------------------------------------------

PLFLT
plstr(PLCHAR_VECTOR string, PLINT length_only, PLINT base, PLFLT just, PLFLT * const xform, PLINT x, PLINT y, PLINT refx, PLINT refy) {
	static PLFLT saverestore[1000] = {};
	int counter = -1;
	short *charPoints = 0;
	PLUNICODE ifont = plsc->ttFontIndex;
	if (!plsc->use_unicode) {ifont = plsc->HersheyFontIndex; ifont=MAX(3,ifont); ifont=MIN(numberfonts,ifont); }  
PLUNICODE oldifont = ifont;
#define HEIGHTRATIO 1.6
	PLINT ch, i, length, style;
	PLFLT width = 0., xorg = 0., yorg = 0., yline = 0., yref = 0., def, ht, dscale, scale;
	plgchr(&def, &ht);
	// TrueType fonts need a special correction as their size is all different.
	// The correction itself depends on the size of the hershey fonts, all this is quite relative
	// and the exact algorithm needs to be written, this one is too close to the vagaries of the plplot code.
	if (plsc->use_unicode) ht*=charHeightCorr[ifont];
	dscale = 0.05 * ht;
	scale = dscale;
	static const PLFLT scales[2] = {(1 - 0.56), (1 - 0.7)};
	const PLFLT dscale38 = dscale * (1 - 0.38);
	const PLFLT linespacing = HEIGHTRATIO * ht;
	const PLFLT levsuper = HEIGHTRATIO * ht * 0.5 - 0.5 * ht * dscale38;
	const PLFLT firstlevsubs = -HEIGHTRATIO * ht * 0.5 + 0.5 * ht * dscale38;
	const PLFLT secondlevsubs = -HEIGHTRATIO * ht * 0.75 + 0.5 * ht * dscale38;
	int ilev = 0;

	// Line style must be continuous
	style = plsc->lineStyleNumberOfElements;
	plsc->lineStyleNumberOfElements = 0;

	PLUNICODE *symbol = (PLUNICODE*) calloc(strlen(string), sizeof (PLUNICODE));

	pldeco(symbol, &length, string); // decode embedded commands, encode to unicode or hershey, depending.

	int revert = 0;
	PLINT oldglyph = -1; //for char-to-char advance
	for (i = 0; i < length; i++) {
		ch = symbol[i];
		switch (ch) {
			case A: // !A Shift above the division line.
				yorg = yref = yline + linespacing / 2;
				ilev = 0;
				scale = dscale;
				break;
			case B: // !B Shift below the division line.
				yorg = yref = yline - linespacing / 2;
				ilev = 0,
				scale = dscale;
				break;
			case C: // !C shift back to the starting position and down one line
				xorg = 0;
				yline -= linespacing;
				yorg = yref = yline;
				scale = dscale;
				ilev = 0;
				break;
			case D: // !D Shift down to the first level subscript, shrink the character size by 38%.
				yorg = yref = yline + firstlevsubs;
				ilev = 1;
				scale = dscale38;
				break;
			case U:// !U Shift to first and unique upper subscript level, shrink the character size by 38%.
				yorg = yref = yline + levsuper;
				ilev = 1;
				scale = dscale38;
				break;
			case L: // !L Shift down to the second level subscript, shrink the character size by 38%.
				yorg = yref = yline + secondlevsubs;
				ilev = 1;
				scale = dscale38;
				break;
				// 2 variable sizes
			case E: // !E Shift up to the exponent level, shrink the character size by 56%.
				yorg = yref + (HEIGHTRATIO * ht) * scales[ilev]; //not exactly same as IDL
				scale = dscale * scales[ilev];
				break;
			case I: // !I Shift down to the index level, shrink the character size by 56%.
				yorg = yref - (HEIGHTRATIO * ht) * scales[ilev]; //idem
				scale = dscale * scales[ilev];
				break;
			case M: // !M Switch to the !9 symbol font for one character, then switch back.
				oldifont = ifont; oldglyph=-1;
				ifont = 9;
				revert = 1;
			case N: // !N Shift back to the normal level and original character size.
				scale = dscale;
				yorg = yref = yline;
				ilev = 0;
				break;
			case R: // !R Restore position from the top of the saved positions stack.
				if (counter >= 0) {
					xorg = saverestore[counter--];
				} else {
					fprintf(stderr, "Error using Hershey characters: Restore without save.\n");
					return xorg;
				}
				break;
			case S:// !S Save position to the top of the saved positions stack.
				saverestore[++counter] = xorg;
				break;
			case V: // !V Switch to the !20 symbol font for one character, then switch back.
				oldifont = ifont; oldglyph=-1;
				ifont = 20;
				revert = 1;
			case SP:// SPACE : Just add space size
				xorg += ht;
				break;
			default:
				
redo:				if (plsc->use_unicode) {
					if (ch >= PRIVATE_UNICODE_PLANE) {
						ifont = ch - PRIVATE_UNICODE_PLANE;
						break;
					}
					if (ttfVectors[ifont] == NULL) {
						printf("True Type Fonts not loaded, reverting to Hershey fonts.\n");
						plsc->use_unicode=0;
						goto redo;
					}
					int glyph = stbtt_FindGlyphIndex(ttfVectors[ifont], ch);
					int ax;
					int lsb;
					stbtt_GetGlyphHMetrics(ttfVectors[ifont], glyph, &ax, &lsb);
					if (oldglyph != -1) {
						ax += stbtt_GetGlyphKernAdvance(ttfVectors[ifont], oldglyph, glyph);
					}
					oldglyph = glyph;
					//ax is advance width, so corr*ax will be advance in pixels.
					width = ax ;
					if (length_only) {
						xorg += (width * scale);
						break; // do not draw anything, just add to xorg
					}
/*
					printf("glyph: %c, ax=%d, corr=%f, scale=%f, width=%f\n",ch,ax,charHeightCorr[plsc->fontIndex],scale,width);
*/
					stbtt_vertex *vertices;
					int nvecs = stbtt_GetGlyphShape(ttfVectors[ifont], glyph, &vertices);
					if (plsc->dev_alt_unicode) {
					plttf2(vertices, charDescent[ifont], nvecs, xform, refx, refy, scale,
							plsc->xpmm, plsc->ypmm, &xorg, &yorg, width, ttfVectors[ifont]->userdata);
					} else {
					plttf(vertices, charDescent[ifont], nvecs, xform, refx, refy, scale,
							plsc->xpmm, plsc->ypmm, &xorg, &yorg, width);
					}

					stbtt_FreeShape(ttfVectors[ifont], vertices);
				} else {
					if (ch >= PRIVATE_UNICODE_PLANE) {
						ifont = ch - PRIVATE_UNICODE_PLANE;
						break;
					}
					if (hersheyNumberChars[ifont] == 0) break;
					if ((ch - 32) > hersheyNumberChars[ifont]) break;
					int offset = hersheyFontLookupStruct[ifont][ch - 32].offset;
					int nvecs = hersheyFontLookupStruct[ifont][ch - 32].nvecs;
					width = hersheyFontLookupStruct[ifont][ch - 32].width;
					if (length_only) {
						xorg += width * scale;
						break; // do not draw anything, just add to xorg
					}
/*
					printf("glyph: %c, scale=%f, width=%f\n",ch,scale,width);
*/
					charPoints = &(hersheyFontVectors[ifont][offset]);
					plhershey(charPoints, nvecs, xform, refx, refy, scale,
							plsc->xpmm, plsc->ypmm, &xorg, &yorg, width);
				}
		}
		if (revert) {
			revert = 0;
			ifont = oldifont; 
			oldglyph=-1;
		}
	}
	free(symbol);
	//reset line style
	plsc->lineStyleNumberOfElements = style;
	if (plsc->use_unicode) plsc->ttFontIndex=ifont; 
	else plsc->HersheyFontIndex=ifont; // hershey fonts history are managed entirely inside plsc.
	return xorg; //length
}

//--------------------------------------------------------------------------
// plhershey()
//
// Plots out Hershey fonts
//--------------------------------------------------------------------------
static void
plhershey( short *vects, int len, PLFLT * const xform, 
        PLINT refx, PLINT refy, PLFLT scale, PLFLT xpmm, PLFLT ypmm,
        PLFLT *p_xorg, PLFLT *p_yorg, PLFLT width) {

	if (vects[0] == -1) return;
	
	PLINT lx, ly;
    PLINT penup;
    PLFLT x, y;
    short cx, cy;
    PLINT *llx=(PLINT*)malloc(len*sizeof(PLINT));
    PLINT *lly=(PLINT*)malloc(len*sizeof(PLINT));
	PLINT l = 0;


    penup = 1;
	
    for ( int i=0; i< len; ++i )
    {
		cx = ( vects[i] >> 7 ) & 127;	//Get X and Y components
        cy = vects[i] & 127;
        if (cx & 64) cx-=128;
        if (cy & 64) cy-=128;
		cy-=10; //baseline. check with:
		//erase & for i=0.1,1.1,0.1 do begin & XYOUTS, 0, i,'mjmym#m;:',SIZE = 10*i, /NORMAL, width=w &plots,[0,w],replicate(i,2),/norm & end
        penup = ((vects[i] & 16384) != 0);
		x = *p_xorg + cx * scale;
		y = *p_yorg + cy * scale;
		lx = refx + ROUND(xpmm * (xform[0] * x + xform[1] * y));
		ly = refy + ROUND(ypmm * (xform[2] * x + xform[3] * y));
        if ( penup  == 1)
        {
            if ( l )
            {
                plP_draphy_poly( llx, lly, l );
                l = 0;
			}
			llx[l]   = lx;
			lly[l++] = ly; // store 1st point !
			plP_movphy( lx, ly );
			penup = 0;
		} else {
			llx[l] = lx;
			lly[l++] = ly;
		}
	}
	if (l) {
		plP_draphy_poly(llx, lly, l);
		l = 0;
	}
	free(llx);
	free(lly);
    *p_xorg = *p_xorg + width * scale;
}
//--------------------------------------------------------------------------
// plttf()
//
// Fills a given TTF character using device EOFILL capabilities.
//--------------------------------------------------------------------------
static void
plttf( stbtt_vertex *vects, int descent, int len, PLFLT  * const xform, 
        PLINT refx, PLINT refy, PLFLT scale, PLFLT xpmm, PLFLT ypmm,
        PLFLT *p_xorg, PLFLT *p_yorg, PLFLT width) {

	if (len == 0) return;
	PLINT lx, ly;
    PLFLT x, y;
    PLINT cx, cy;
	PLINT l = 0;
	PLINT nPath=0;
    for ( int i=0; i< len; ++i )
    {
        switch (vects[i].type) {
			case STBTT_vmove:
				nPath++;
		}
	}
	// compute allocation, according to type of path:
	int npassed=0;
    for ( int i=0; i< len; ++i )
    {
		switch (vects[i].type) {
			case STBTT_vmove:
				npassed++;
				break;
			case STBTT_vline:
				npassed+=2;
				break;
			case STBTT_vcurve:
				npassed+=3;
				break;
			case STBTT_vcubic:
				npassed+=4;
				break;
		}
	}	
    PLINT *llx=(PLINT*)malloc(npassed*sizeof(PLINT));
    PLINT *lly=(PLINT*)malloc(npassed*sizeof(PLINT));
			
	PLINT n=0;
	PLINT ipath=0;
	PLINT *pathnxy=(PLINT*)malloc(nPath*sizeof(PLINT));
	PLINT **pathx=(PLINT**)malloc(nPath*sizeof(PLINT*));
	PLINT **pathy=(PLINT**)malloc(nPath*sizeof(PLINT*));
	pathnxy[0]=0;
	pathx[0]=&(llx[0]);
	pathy[0]=&(lly[0]);
    for ( int i=0; i< len; ++i )
    {
        cx = vects[i].x, cy = vects[i].y + descent;
		x = *p_xorg + cx * scale;
		y = *p_yorg + cy * scale;
		lx = refx + ROUND(xpmm * (xform[0] * x + xform[1] * y));
		ly = refy + ROUND(ypmm * (xform[2] * x + xform[3] * y));
        switch (vects[i].type) {
            case STBTT_vmove:
				if (l!=0) { //not for the start
					pathnxy[ipath] = n; //finish previous path
					ipath++;
				}
				llx[l] = lx;
				lly[l] = ly;
				n = 1;
				pathx[ipath] = &(llx[l]);
				pathy[ipath] = &(lly[l]);
				l++;
				break;
            case STBTT_vline:
				llx[l] = -1; l++; n++; //line
				llx[l] = lx;
				lly[l] = ly;
				l++;
				n++;
               break;
            case STBTT_vcurve:
				llx[l] = -2; l++; n++; //quadratic , 2 pair of coords follow
				cx = vects[i].cx, cy = vects[i].cy +  descent;
		        x = *p_xorg + cx * scale;
		        y = *p_yorg + cy * scale;
				llx[l] = refx + ROUND(xpmm * (xform[0] * x + xform[1] * y));
				lly[l] = refy + ROUND(ypmm * (xform[2] * x + xform[3] * y));
				l++; n++;
				llx[l] = lx;
				lly[l] = ly;
				l++;
				n++;
               break;
            case STBTT_vcubic:
				llx[l] = -3; l++; n++; //cubic , 3 pair of coords follow
				cx = vects[i].cx, cy = vects[i].cy +  descent;
		        x = *p_xorg + cx * scale;
		        y = *p_yorg + cy * scale;
				llx[l] = refx + ROUND(xpmm * (xform[0] * x + xform[1] * y));
				lly[l] = refy + ROUND(ypmm * (xform[2] * x + xform[3] * y));
				l++; n++;
				cx = vects[i].cx1, cy = vects[i].cy1 +  descent;
		        x = *p_xorg + cx * scale;
		        y = *p_yorg + cy * scale;
				llx[l] = refx + ROUND(xpmm * (xform[0] * x + xform[1] * y));
				lly[l] = refy + ROUND(ypmm * (xform[2] * x + xform[3] * y));
				l++; n++;
				llx[l] = lx;
				lly[l] = ly;
				l++;
				n++;
               break;
         }
	}
	if (l > 2) { //3 for filling
	    pathnxy[nPath-1]=n;
		plP_pathfill( pathx, pathy, pathnxy, nPath);
		l = 0;
	} else l=0;
	free(llx);
	free(lly);
	free(pathnxy);
	free(pathx);
	free(pathy);
    *p_xorg = *p_xorg + width * scale;
}
//--------------------------------------------------------------------------
// plttf()
//
// Fills a given TTF character using device EOFILL capabilities.
//--------------------------------------------------------------------------

static void
plttf2(stbtt_vertex *vects, int descent, int len, PLFLT * const xform,
		PLINT refx, PLINT refy, PLFLT scale, PLFLT xpmm, PLFLT ypmm,
		PLFLT *p_xorg, PLFLT *p_yorg, PLFLT width, void* userdata) {
	PLINT cx,cy;
	float x,y;
	if (len == 0) return;
	int winding_count = 0;
	int *winding_lengths = NULL;
	stbtt__point *windings = stbtt_FlattenCurves(vects, len, 0.35f / scale, &winding_lengths, &winding_count, userdata);
	int n = 0;
	for (int i = 0; i < winding_count; ++i) {
		n += winding_lengths[i];
	}
	if (windings) {
		short *llx = (short*) malloc(n * sizeof (short));
		short *lly = (short*) malloc(n * sizeof (short));
		for (int i = 0; i < n; ++i) {
			cx = windings[i].x;
			cy = windings[i].y +  descent;
			x = *p_xorg + cx * scale;
			y = *p_yorg + cy * scale;
			llx[i] = refx + ROUND(xpmm * (xform[0] * x + xform[1] * y));
			lly[i] = refy + ROUND(ypmm * (xform[2] * x + xform[3] * y));
		}
		plP_fill(llx, lly, n);
		free(llx);
		free(lly);
		STBTT_free(winding_lengths, userdata);
		STBTT_free(windings, userdata);
	}
	*p_xorg = *p_xorg + width * scale;
}



//--------------------------------------------------------------------------
// PLFLT plstrl()
//
// Computes the length of a string in mm, including escape sequences.
//--------------------------------------------------------------------------

PLFLT
plstrl( PLCHAR_VECTOR string) {	
/*
	PLFLT def, ht;
	plgchr(&def, &ht);
	PLFLT ret =strlen(string)*ht;
    printf("strlen=%f\n",ret);
  	return ret;
*/
	// take into account that plstr can change the font! Restore it at end!
	PLUNICODE ifont = (plsc->use_unicode)?plsc->ttFontIndex:plsc->HersheyFontIndex;
	PLFLT ret = plstr(string, 1, 0,0, NULL,0,0,0,0);
	if (plsc->use_unicode) plsc->ttFontIndex=ifont; else plsc->HersheyFontIndex=ifont;
	return ret;
}

//--------------------------------------------------------------------------
// void pldeco()
//
// Decode a character string, and return an array of float integer symbol
// numbers. This routine is responsible for interpreting all escape sequences.
// At present the following escape sequences are defined (the letter following
// the ! may be either upper or lower case):
//
// !!	: !
// ![1-20]	: switch to font [3-20]
// !X	: revert to entry font
// !G	: switch to Gothic english (font 11)
// !W	: simplex script (font 12)
// !V	: switch to font 20 for 1 char
// !Z(nnn)	: Hershey symbol number nnn (any number of digits)
// !A   : Above line (-1)
// !B   : Below line (-2)
// !C   : "Carriage return", shift back to the starting position and down one line. This also performs a "!N", returning to the normal level and character size. (-3)
// !D   : down first level subscript, shft char size by 38% (-4)
// !E   : up to first exponent level, shrink size by 56% (-5)
// !I   : Shift down to the index level, shrink the character size by 56%. (-6)
// !L   : Shift down to the second level subscript, shrink the character size by 38%. (-7)
// !M   : Switch to the !9 symbol font for one character, then switch back.
// !N   : Shift back to the normal level and original character size. (-8)
// !R   : Restore position from the top of the saved positions stack. (-9)
// !S   : Save position to the top of the saved positions stack. (-10)
// !U   : Shift to upper subscript level, shrink the character size by 38% (-11)
//--------------------------------------------------------------------------

static void
pldeco( PLUNICODE *sym, PLINT *length, PLCHAR_VECTOR text)
{
    PLUNICODE     ch, ifont = plsc->ttFontIndex;
	PLINT ig, j = 0, lentxt = (PLINT) strlen( text );
    unsigned char      test, esc;

#define SPACE 32
	// Initialize parameters.

    *length = 0;

    plgesc( &esc );

// Get next character; treat non-printing characters as spaces.

    while ( j < lentxt )
    {
        test = text[j++];
        ch   = test;

        if ( ch == esc && ( lentxt - j ) >= 1 )
        {
            test = text[j++];
			switch(test) {
				case 0x21 : sym[( *length )++] = ch; break; 
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
					ifont=(test-'0'); sym[( *length )++]=ifont+PRIVATE_UNICODE_PLANE; break;
				case '1':
					ifont = (test - '0');
					test = text[j++];
					switch (test) {
						case '0':
						case '1':
						case '2':
						case '3':
						case '4':
						case '5':
						case '6':
						case '7':
						case '8':
						case '9':
							ifont *= 10;
							ifont += (test - '0');
							break;
						default:
							j--;
					}
					sym[(*length)++] = ifont + PRIVATE_UNICODE_PLANE;
					break;
				case '2':
					ifont=2;
					test = text[j++];
					switch (test) {
						case '0':
							ifont = 20;
							break;
						default:
							j--;
					}
					sym[(*length)++] = ifont + PRIVATE_UNICODE_PLANE;
					break;
				case 'G':
				case 'g':
					ifont=11;	sym[( *length )++]=ifont+PRIVATE_UNICODE_PLANE;				break;
				case 'W':
				case 'w':
					ifont=12;	sym[( *length )++]=ifont+PRIVATE_UNICODE_PLANE;				break;
				case 'X':
				case 'x':
					sym[( *length )++]=ifont+PRIVATE_UNICODE_PLANE; break;
				case 'A': case 'a':sym[( *length )++] = A; break;
				case 'B': case 'b':sym[( *length )++] = B; break;
				case 'C': case 'c':sym[( *length )++] = C; break;
				case 'D': case 'd':sym[( *length )++] = D; break;
				case 'E': case 'e':sym[( *length )++] = E; break;
				case 'I': case 'i':sym[( *length )++] = I; break;
				case 'L': case 'l':sym[( *length )++] = L; break;
				case 'M': case 'm':sym[( *length )++] = M; break;
				case 'N': case 'n':sym[( *length )++] = N; break;
				case 'R': case 'r':sym[( *length )++] = R; break;
				case 'S': case 's':sym[( *length )++] = S; break;
				case 'U': case 'u':sym[( *length )++] = U; break;
				case 'V': case 'v':sym[( *length )++] = V; break;
				case 'Z': case 'z':
					test  = text[j++];
					if ( test == '(' )
					{
						sym[*length] = 0;
						while ( text[j] != ')' && ( lentxt - j ) >= 1 ) 
						{
							if ( text[j] == SPACE ) {j++;continue;}
							if ( text[j] == ',' ) {j++; ( *length )++; sym[*length] = 0; continue;}
							if ( '0' <= text[j] && text[j] <= '9' )	sym[*length] = (PLUNICODE) ( (int) sym[*length] * 16 + text[j] - '0' );
							else if ( 'A' <= text[j] && text[j] <= 'F' )	sym[*length] = (PLUNICODE) ( (int) sym[*length] * 16 + 10 + text[j] - 'A' );
							else if ( 'a' <= text[j] && text[j] <= 'f' )	sym[*length] = (PLUNICODE) ( (int) sym[*length] * 16 + 10 + text[j] - 'a' );
							j++;
						}
						( *length )++;
						if ( text[j] == ')' ) j++; else {
							fprintf(stderr, "Error using Hershey characters: Parentheses required for !Z\n");
							return;
						}
					} else j--;
					break;
				default:
					break;
			}
		} else {
				if (ch == SPACE) sym[( *length )++] = SP; else sym[( *length )++] = ch;
		}
    }
}

PLINT
plP_strpos( PLCHAR_VECTOR str, int chr )
{
    char *temp;

    if ( ( temp = strchr( str, chr ) ) )
        return (PLINT) ( temp - str );
    else
        return (PLINT) -1;
}

//--------------------------------------------------------------------------
// PLINT plP_stindex()
//
// Similar to strpos, but searches for occurence of string str2.
//--------------------------------------------------------------------------

PLINT
plP_stindex( PLCHAR_VECTOR str1, PLCHAR_VECTOR str2 )
{
    PLINT base, str1ind, str2ind;

    for ( base = 0; *( str1 + base ) != '\0'; base++ )
    {
        for ( str1ind = base, str2ind = 0; *( str2 + str2ind ) != '\0' &&
              *( str2 + str2ind ) == *( str1 + str1ind ); str1ind++, str2ind++ )
            ;

        if ( *( str2 + str2ind ) == '\0' )
            return (PLINT) base;
    }
    return (PLINT) -1;          // search failed
}

//--------------------------------------------------------------------------
// PLBOOL plP_stsearch()
//
// Searches string str for character chr (case insensitive).
//--------------------------------------------------------------------------

PLBOOL
plP_stsearch( PLCHAR_VECTOR str, int chr )
{
    if ( strchr( str, chr ) )
        return TRUE;
    else if ( strchr( str, toupper( chr ) ) )
        return TRUE;
    else
        return FALSE;
}


#include <fcntl.h>

void hersheyFontLoad(char* file) {
	long mask = 0x7fffffffUL;
	int fd = open(file, O_RDONLY);
	if (fd == -1) {printf("No fontfile %s available, exiting.\n",file); exit(1);}
	numberfonts = NUMBERHERSHEYFONTS;
	if (HersheyFontTableDirectory == NULL) HersheyFontTableDirectory = calloc(NUMBERHERSHEYFONTS * 2, sizeof (int32_t));
	// Read HersheyFontTableDirectory[]	
	ssize_t n = read(fd, HersheyFontTableDirectory, NUMBERHERSHEYFONTS * 2 * sizeof (int32_t)); //printf("read: %d\n",n);
	for (int i = 0; i < numberfonts * 2; ++i) {
		long j = HersheyFontTableDirectory[i];
		HersheyFontTableDirectory[i] = ntohl(j);
	}
	int nchars;
	// for each font , create and copy the ctab info struct and the vector of encoded coordinates	
	for (int ifont = 0; ifont < numberfonts; ++ifont) {
		if (HersheyFontTableDirectory[2 * ifont] == -1) {
			hersheyNumberChars[ifont] = 0;
			continue; //null font
		}
		hersheyNumberChars[ifont] = 128 - 32;
		if ((HersheyFontTableDirectory[2 * ifont + 1] & (~mask)) != 0) hersheyNumberChars[ifont] = 256 - 32;

		long len = (HersheyFontTableDirectory[2 * ifont + 1] & mask); //printf("len: %d\n",len);
		if (hersheyFontLookupStruct[ifont] != 0) free(hersheyFontLookupStruct[ifont]);
		CTAB* ctab = (CTAB*) malloc(hersheyNumberChars[ifont] * sizeof (CTAB));
		hersheyFontLookupStruct[ifont] = ctab; //memorize this
		off_t pos = lseek(fd, HersheyFontTableDirectory[2 * ifont] & 0x0fffffffUL, SEEK_SET);
/*
		printf("ifont=%d nchars=%d pos=%d,",ifont,hersheyNumberChars[ifont],pos); printf("\n");
*/
		ssize_t n = read(fd, ctab, hersheyNumberChars[ifont] * sizeof (CTAB)); //printf("read: %d\n",n);
		int k = 0; //Current offset
		for (int i = 0; i < hersheyNumberChars[ifont]; ++i) { //Un swap shorts
			short j = ntohs(ctab[i].offset);
			if (j == 0) j = k; //Put empty chars in their proper place
			else if (j != k) fprintf(stderr, "Inconsistent vector offset/length, chr = %d, j=%d, k=%d\n", i + 32, j, k);
			ctab[i].offset = j;
			k = k + ctab[i].nvecs;
		}
		if (hersheyFontVectors[ifont] != 0) free(hersheyFontVectors[ifont]);
		short *vects = malloc(len); //len/2 shorts
		hersheyFontVectors[ifont] = vects;
		n = read(fd, vects, len); //printf("read: %d\n",n);
		for (int i = 0; i < len / 2; ++i) vects[i] = ntohs(vects[i]);
	}
/*
		for (int ifont=0; ifont<numberfonts; ++ifont) {
			printf("FONT #%d: %d characters.\n",ifont+1,hersheyNumberChars[ifont]);
			if (HersheyFontTableDirectory[2*ifont] == -1) continue; //null font
			for (int i=0; i<hersheyNumberChars[ifont]; ++i) {
				printf("char #%d :nvecs=%d, width=%d, offset=%d\n",i,hersheyFontLookupStruct[ifont][i].nvecs, hersheyFontLookupStruct[ifont][i].width,hersheyFontLookupStruct[ifont][i].offset);
			}
		}
*/
	return;


}
extern char* getFontPath(const char* name);
extern int getFontIndex(const char* name);
extern const char* getFontName(int n);
extern int loadFontPath(const char *name);
void c_ttFontSet(int n) {	
	printf("c_ttFontSet(%d) (%s)\n",n, getFontName(n));
	if (getFontName(n) != NULL) {
		plsc->ttFontIndex=n;
	} else printf("loading of font #%d failed.\n",n);
}
void c_ttFontLoad(const char* fontName) {
	int n=getFontIndex(fontName);
/*
	printf("c_ttFontLoad(%s) gives index n=%d.\n",fontName,n);
*/
	if (n < 0) {
	n=loadFontPath(fontName); //happens when stream is initialized
	if (n < 0) return; // too silent...
	}
    long size;
    unsigned char* fontBuffer;
    char* fontPath=getFontPath(fontName);
	if (fontPath==NULL) { printf("invalid Font Path!!!\n",n); return;} 
    FILE* fontFile = fopen(fontPath, "rb");
	
    fseek(fontFile, 0, SEEK_END);
    size = ftell(fontFile); /* how long is the file ? */
    fseek(fontFile, 0, SEEK_SET); /* reset */
    
    fontBuffer = malloc(size);
    
    fread(fontBuffer, size, 1, fontFile);
    fclose(fontFile);

    /* prepare font */
    stbtt_fontinfo* info=(stbtt_fontinfo*) malloc(sizeof(stbtt_fontinfo));
    if (!stbtt_InitFont(info, fontBuffer, 0))
    {
        printf("loading of %s failed.\n",fontName);
    }
    ttfVectors[n]=info;
	plsc->ttFontIndex=n;
	plsc->ttFontName=fontName;
	// to be optimized:
	int x0, y0, x1, y1;
	stbtt_GetFontBoundingBox(info, &x0, &y0, &x1, &y1);
	//the height of the “average” character (for IDL it is determined by the width of the rectangle?)
	float averheight = (float) (y1 - y0);
	//The aspect ratio of the “average” character remains fixed; each character is then scaled so that its width is the value of X_CH_SIZE.
	float aspectratiooffont = averheight / (float) (x1 - x0);
	charHeightCorr[n] = 20. / averheight; //value found experimentally (?)
	int ascent, descent, lineGap;
    stbtt_GetFontVMetrics(info, &ascent, &descent, &lineGap);
	charDescent[n] = 2.5*descent; //value found experimentally (?)
/*
	printf("averheight=%f, aspectratio=%f, ascent=%d, descent=%d, lineGap=%d, corr=%f\n",averheight,aspectratiooffont, ascent, descent,lineGap, charHeightCorr[n]);
*/
}




#undef PLSYM_H
#endif
