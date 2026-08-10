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
#include "plhershey-unicode.h"

//for ntohl etc
#if defined(_WIN32) && !defined(__CYGWIN__)
#include <winsock2.h>
#else 
#include <arpa/inet.h>
#endif

// Declarations
#define NUMBERHERSHEYFONTS 40 
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
static short int   *fntlkup;
static long   *fntindx;
static signed char *fntbffr;
static short int   numberfonts, numberchars;
static long   indxleng;

static short       fontloaded = 0;
// moved to plstr.h, plsc->cfont  static PLINT font = 1;  current font

//#define PLMAXSTR    300
#define STLEN       250

//static PLUNICODE symbol_buffer[PLMAXSTR];
static signed char xygrid[STLEN];

int hershey2unicode( int in );
int text2num( PLCHAR_VECTOR text, char end, PLUNICODE *num );

// Static function prototypes

static void
pldeco( PLUNICODE *sym, PLINT *length, PLCHAR_VECTOR text, int doUnicode);
static void
plchar( short *xygrid, int len, PLFLT *xform, 
        PLINT refx, PLINT refy, PLFLT scale, PLFLT xpmm, PLFLT ypmm,
        PLFLT *p_xorg, PLFLT *p_yorg, PLFLT width );
static PLINT
plcvec( PLINT ch, signed char **xygr );


//--------------------------------------------------------------------------
// void pllab()
//
// Simple routine for labelling graphs.
//--------------------------------------------------------------------------

void
c_pllab( PLCHAR_VECTOR xlabel, PLCHAR_VECTOR ylabel, PLCHAR_VECTOR tlabel )
{
    if ( plsc->level < 2 )
    {
        plabort( "pllab: Please set up viewport first" );
        return;
    }

    plmtex( "t", (PLFLT) 2.0, (PLFLT) 0.5, (PLFLT) 0.5, tlabel );
    plmtex( "b", (PLFLT) 3.2, (PLFLT) 0.5, (PLFLT) 0.5, xlabel );
    plmtex( "l", (PLFLT) 5.0, (PLFLT) 0.5, (PLFLT) 0.5, ylabel );
}

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
plstr(PLCHAR_VECTOR string, PLINT length_only, PLINT base, PLFLT just, PLFLT *xform, PLINT x, PLINT y, PLINT refx, PLINT refy) {
	static PLFLT saverestore[1000] = {};
	int counter = -1;
	short *charPoints = 0;
	PLFLT save_form[4]={1,0,0,1};
#define HEIGHTRATIO 1.6
	PLINT ch, i, length, style, oline = 0;
	PLFLT width = 0., xorg = 0., yorg = 0., yline = 0., yref = 0., def, ht, dscale, scale;
	plgchr(&def, &ht);
	dscale = 0.05 * ht;
	scale = dscale;
	static const PLFLT scales[2] = {(1 - 0.56), (1 - 0.7)};
	const PLFLT dscale38 = dscale * (1 - 0.38);
	const PLFLT linespacing = HEIGHTRATIO * ht;
	const PLFLT levsuper = HEIGHTRATIO * ht * 0.5 - 0.5 * ht * dscale38;
	const PLFLT firstlevsubs = -HEIGHTRATIO * ht * 0.5 + 0.5 * ht * dscale38;
	const PLFLT secondlevsubs = -HEIGHTRATIO * ht * 0.75 + 0.5 * ht * dscale38;
	int ilev = 0;
	int write = 0;
	// Line style must be continuous

	style = plsc->nms;
	plsc->nms = 0;

	EscText args = {};
	args.text_type = PL_STRING_TEXT;
	args.base = base;
	args.just = just;
	args.scale= dscale;
	//must make a copy of xform because 'args.xform' is modified afterwards and must be resetted each
	// time the string position is called
	args.xform = save_form;
	if (xform) for (int i=0; i< 4; ++i) save_form[i]=xform[i]; //xform may be NULL!
	args.x = x;
	args.y = y;
	args.refx = refx;
	args.refy = refy;
printf("%d,%f\n",x,xorg);
	// Always store the string passed by the caller, even for unicode
	// enabled drivers.  The plmeta driver will use this field to store
	// the string data in the metafile.
	args.string = string;
	args.unicode_array = (PLUNICODE*) calloc(strlen(string), sizeof (PLUNICODE));
	args.unicode_array_len = 0;
	PLUNICODE *symbol = args.unicode_array;

	pldeco(symbol, &length, string, plsc->dev_text); // decode embedded commands, encode to unicode or hershey, depending.

	PLUNICODE ifont = plsc->cfont;
	PLUNICODE oldifont = ifont;
	int revert = 0;

	if (plsc->dev_text) // Does the device render it's own text ?
	{
		for (i = 0; i < length; i++) {
			ch = symbol[i];
			switch (ch) {
				case A: // !A Shift above the division line.
					if (args.unicode_array_len > 0)	plP_esc(PLESC_HAS_TEXT, &args); args.unicode_array_len=0;
					yorg = yref = yline + linespacing / 2;
					ilev = 0;
					scale = dscale;
					break;
				case B: // !B Shift below the division line.
					if (args.unicode_array_len > 0)	plP_esc(PLESC_HAS_TEXT, &args);args.unicode_array_len=0;
					yorg = yref = yline - linespacing / 2;
					ilev = 0;
					scale = dscale;
					break;
				case C: // !C shift back to the starting position and down one line
					if (args.unicode_array_len > 0)	plP_esc(PLESC_HAS_TEXT, &args);args.unicode_array_len=0;
					xorg = 0;
					yline -= linespacing;
					yorg = yref = yline;
					scale = dscale;
					ilev = 0;
					break;
				case D: // !D Shift down to the first level subscript, shrink the character size by 38%.
					if (args.unicode_array_len > 0)	plP_esc(PLESC_HAS_TEXT, &args);args.unicode_array_len=0;
					yorg = yref = yline + firstlevsubs;
					ilev = 1;
					scale = dscale38;
					break;
				case U:// !U Shift to first and unique upper subscript level, shrink the character size by 38%.
					if (args.unicode_array_len > 0)	plP_esc(PLESC_HAS_TEXT, &args);args.unicode_array_len=0;
					yorg = yref = yline + levsuper;
					scale = dscale38;
					write = 1;
					break;
				case L: // !L Shift down to the second level subscript, shrink the character size by 38%.
					if (args.unicode_array_len > 0)	plP_esc(PLESC_HAS_TEXT, &args);args.unicode_array_len=0;
					yorg = yref = yline + secondlevsubs;
					ilev = 1;
					scale = dscale38;
					write = 1;
					break;
					// 2 variable sizes
				case E: // !E Shift up to the exponent level, shrink the character size by 56%.
					if (args.unicode_array_len > 0)	plP_esc(PLESC_HAS_TEXT, &args);args.unicode_array_len=0;
					yorg = yref + (HEIGHTRATIO * ht) * scales[ilev]; //not exactly same as IDL
					scale = dscale * scales[ilev];
					write = 1;
					break;
				case I: // !I Shift down to the index level, shrink the character size by 56%.
					if (args.unicode_array_len > 0)	plP_esc(PLESC_HAS_TEXT, &args);args.unicode_array_len=0;
					yorg = yref - (HEIGHTRATIO * ht) * scales[ilev]; //idem
					scale = dscale * scales[ilev];
					write = 1;
					break;
				case M: // !M Switch to the !9 symbol font for one character, then switch back.
					if (args.unicode_array_len > 0)	plP_esc(PLESC_HAS_TEXT, &args);args.unicode_array_len=0;
					oldifont = ifont;
					ifont = 9;
					revert = 1;
					plP_esc(PLESC_LOAD_FONT, &ifont);
				case N: // !N Shift back to the normal level and original character size.
					if (args.unicode_array_len > 0)	plP_esc(PLESC_HAS_TEXT, &args);args.unicode_array_len=0;
					scale = dscale;
					yorg = yref = yline;
					ilev = 0;
					write = 1;
					break;
				case R: // !R Restore position from the top of the saved positions stack.
					if (counter >= 0) {
					    if (args.unicode_array_len > 0)	plP_esc(PLESC_HAS_TEXT, &args);args.unicode_array_len=0;
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
					if (args.unicode_array_len > 0)	plP_esc(PLESC_HAS_TEXT, &args);args.unicode_array_len=0;
					oldifont = ifont;
					ifont = 20;
					revert = 1;
					plP_esc(PLESC_LOAD_FONT, &ifont);
/*
				case SP:// SPACE : Just add space size
					xorg += ht * args.scale * plsc->xpmm * 10;
					break;
*/
				default:
				{
					args.unicode_array[args.unicode_array_len++] = ch; //if (ch < PRIVATE_UNICODE_PLANE) xorg+=ht*scale;
					if (length_only) {
						if (ch < PRIVATE_UNICODE_PLANE) xorg += ht * scale;
						break;
					}
					xorg += ht * args.scale * plsc->xpmm; //plsc->string_length; printf("%f,%f\n",x,xorg);
					args.x = x + xorg;
					printf("x=%d\n", args.x);
					args.y = y + yorg * plsc->ypmm;
					args.scale = scale;
					if (xform) for (int i = 0; i < 4; ++i) save_form[i] = xform[i]; //restore original xform
				}
			}
			if (revert) {
				revert = 0;
				ifont = oldifont;
				plP_esc(PLESC_LOAD_FONT, &ifont);
			}
		}
	} else {
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
					oldifont = ifont;
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
					oldifont = ifont;
					ifont = 20;
					revert = 1;
				case SP:// SPACE : Just add space size
					xorg += ht * args.scale * plsc->xpmm * 10;
					break;
				default:
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
						break; // do not draw anything
					}
					charPoints = &(hersheyFontVectors[ifont][offset]);
					plchar(charPoints, nvecs, xform, refx, refy, scale,
							plsc->xpmm, plsc->ypmm, &xorg, &yorg, width);

			}
			if (revert) {
				revert = 0;
				ifont = oldifont;
			}
		}
	}
	if (length_only) return xorg; //avoid problems with null-valued xform
	
	if (plsc->dev_text) // Does the device render it's own text ?
	{
		if (args.unicode_array_len) plP_esc(PLESC_HAS_TEXT, &args);
	}
	free(args.unicode_array);
	plsc->nms = style;

	return xorg; //length
}

//--------------------------------------------------------------------------
// plchar()
//
// Plots out a given stroke font character.
//--------------------------------------------------------------------------
static void
plchar( short *vects, int len, PLFLT *xform, 
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
// PLFLT plstrl()
//
// Computes the length of a string in mm, including escape sequences.
//--------------------------------------------------------------------------

PLFLT
plstrl( PLCHAR_VECTOR string) {
	return plstr(string, 1, 0,0, NULL,0,0,0,0);
}

static PLINT
plcvec( PLINT ch, signed char **xygr )
{
    PLINT       k = 0, ib;
    signed char x, y;

    ch--;
    if ( ch < 0 || ch >= indxleng )
        return (PLINT) 0;
    ib = fntindx[ch] - 2;
    if ( ib == -2 )
        return (PLINT) 0;
	// ? and ?
	ib++;
	x           = fntbffr[2 * ib];
	y           = fntbffr[2 * ib + 1];
        xygrid[k++] = x;
        xygrid[k++] = y;
	//min and max
	ib++;
	x           = fntbffr[2 * ib];
	y           = fntbffr[2 * ib + 1];
        xygrid[k++] = x;
        xygrid[k++] = y;
    do
    {
        ib++;
        x           = fntbffr[2 * ib];
        y           = fntbffr[2 * ib + 1];
		if (y!=64) y*=-1;
        xygrid[k++] = x;
        xygrid[k++] = y;
    } while ( ( x != 64 || y != 64 ) && k < ( STLEN - 2 ) );

    if ( k == ( STLEN - 2 ) )
    {
        // This is bad if we get here
        xygrid[k++] = 64;
        xygrid[k++] = 64;
    }

    *xygr = xygrid;
    return (PLINT) 1;
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
pldeco( PLUNICODE *sym, PLINT *length, PLCHAR_VECTOR text, int doUnicode)
{
    PLUNICODE     ch, ifont = plsc->cfont;
	PLINT ig, j = 0, lentxt = (PLINT) strlen( text );
    unsigned char      test, esc;

#define SPACE 32
	// Initialize parameters.

    *length = 0;

    plgesc( &esc );
    if ( ifont > numberfonts || ifont < 3 )  { plsc->cfont=3; ifont = 3;}

// Get next character; treat non-printing characters as spaces.

    while ( j < lentxt )
    {
        test = text[j++];
        ch   = test;

        if ( ch == esc && ( lentxt - j ) >= 1 )
        {
            test = text[j++];
			switch(test) {
				case 0x21 : sym[( *length )++] = doUnicode?  gdlHersheyToUnicode ( fontindex[ifont][test-32]):ch; break; // + ( ifont - 1 ) * numberchars + ch ); break;
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
					if (ifont < 3) ifont = 3;
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
					if (ifont < 3) ifont = 3;
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
					ifont=plsc->cfont; sym[( *length )++]=ifont+PRIVATE_UNICODE_PLANE; break;
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
			if (ifont < 3) ifont=3;
			if (ifont > numberfonts) ifont = 3;
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

//--------------------------------------------------------------------------
//! Calculate scale of font size and scale of magnitude of vertical
//! offset associated with superscripts and subscripts.
//! Notes on arguments: ifupper must be either TRUE or FALSE on every
//! call to plP_script_scale.  The contents of the location pointed to
//! by the level pointer must be zero on the first call to
//! plP_script_scale, but not modified externally from then on.  The
//! contents of the locations pointed to by all other pointer
//! arguments are initialized internally, and should not be modified
//! externally.
//!
//! @param ifupper Value which is TRUE if superscripting, i.e., if
//! incrementing the previous level, and FALSE if subscripting, i.e.,
//! decrementing the previous level.
//! @param level Pointer to a location which contains the value of the
//! superscript/subscript level.  That value is 0, +-1, +-2, etc., for
//! no superscript/subscript, the first level of
//! superscript/subscript, the second level of superscript/subscript,
//! etc.  Before the call the value is the old level, and after the
//! call the value will be incremented (ifupper TRUE) or decremented
//! (ifupper FALSE) from the previous value.
//! @param old_scale A pointer to a location that contains after the
//! call the old font size scale value.
//! @param scale A pointer to a location that contains after the call
//! the font size scale value.  This value is 0.75^{|level|} where
//! |level| is the magnitude of the value of the superscript/subscript
//! level after the call.
//! @param old_offset A pointer to a location that contains after the
//! call the old value of the magnitude of the superscript/subscript
//! offset.
//! @param offset A pointer to a location that contains after the call
//! the value of the magnitude of the superscript/subscript offset
//! which is zero for |level|=0 and sum_{i=1}^{i=|level|} 0.75^{i-1},
//! otherwise.

void
plP_script_scale( PLBOOL ifupper, PLINT *level,
                  PLFLT *old_scale, PLFLT *scale,
                  PLFLT *old_offset, PLFLT *offset )
{
    if ( *level == 0 )
    {
        *old_scale  = 1.;
        *old_offset = 0.;
    }
    else
    {
        *old_scale  = *scale;
        *old_offset = *offset;
    }
    if ( ( *level >= 0 && ifupper ) || ( *level <= 0 && !ifupper ) )
    {
        // If superscript of subscript moves further away from centerline....
        *scale  = 0.75 * *old_scale;
        *offset = *old_offset + *old_scale;
    }
    else
    {
        // If superscript of subscript moves closer to centerline....
        *scale  = *old_scale / 0.75;
        *offset = *old_offset - *scale;
    }
    if ( ifupper )
        ( *level )++;
    else
        ( *level )--;
}


#include <fcntl.h>

void plfntld(char* file) {
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
//--------------------------------------------------------------------------
// void plfontrel()
//
// Release memory for fonts.
//--------------------------------------------------------------------------

void
plfontrel( void )
{
    if ( fontloaded )
    {
        free_mem( fntindx )
        free_mem( fntbffr )
        free_mem( fntlkup )
        fontloaded = 0;
    }
}

//--------------------------------------------------------------------------
//  int plhershey2unicode ( int in )
//
//  Function searches for in, the input hershey code, in a lookup table and
//  returns the corresponding index in that table.
//  Using this index you can work out the unicode equivalent as well as
//  the closest approximate to the font-face. If the returned index is
//  -1 then no match was possible.
//
//  Two versions of the function exist, a simple linear search version,
//  and a more complex, but significantly faster, binary search version.
//  If there seem to be problems with the binary search method, the brain-dead
//  linear search can be enabled by defining SIMPLE_BUT_SAFE_HERSHEY_LOOKUP
//  at compile time.
//--------------------------------------------------------------------------

int plhershey2unicode( int in )
{
#ifdef SIMPLE_BUT_SAFE_HERSHEY_LOOKUP
    int ret = -1;
    int i;

    for ( i = 0; ( i < number_of_entries_in_hershey_to_unicode_table ) && ( ret == -1 ); i++ )
    {
        if ( hershey_to_unicode_lookup_table[i].Hershey == in )
            ret = i;
    }

    return ( ret );

#else

    int jlo = -1, jmid, jhi = number_of_entries_in_hershey_to_unicode_table;
    while ( jhi - jlo > 1 )
    {
        // Note that although jlo or jhi can be just outside valid
        // range (see initialization above) because of while condition
        // jlo < jmid < jhi and jmid must be in valid range.
        //
        jmid = ( jlo + jhi ) / 2;
        // convert hershey_to_unicode_lookup_table[jmid].Hershey to signed
        // integer since we don't lose information - the number range
        // is from 1 and 2932 at the moment
        if ( in > (int) ( hershey_to_unicode_lookup_table[jmid].Hershey ) )
            jlo = jmid;
        else if ( in < (int) ( hershey_to_unicode_lookup_table[jmid].Hershey ) )
            jhi = jmid;
        else
            // We have found it!
            // in == hershey_to_unicode_lookup_table[jmid].Hershey
            //
            return ( jmid );
    }
    // jlo is invalid or it is valid and in > hershey_to_unicode_lookup_table[jlo].Hershey.
    // jhi is invalid or it is valid and in < hershey_to_unicode_lookup_table[jhi].Hershey.
    // All these conditions together imply in cannot be found in
    // hershey_to_unicode_lookup_table[j].Hershey, for all j.
    //
    return ( -1 );
#endif
}

PLUNICODE gdlHersheyToUnicode( int hersh )
{
	int ret=plhershey2unicode(hersh);
	if (ret > -1) { return hershey_to_unicode_lookup_table[ret].Unicode; } else return 0;

}

//--------------------------------------------------------------------------
//  char *
//  plP_FCI2FontName ( PLUNICODE fci,
//                     const FCI_to_FontName_Table lookup[], const int nlookup)
//
//  Function takes an input FCI (font characterization integer) index,
//  looks through the lookup table (which must be sorted by PLUNICODE fci),
//  then returns the corresponding pointer to a valid font name.  If the FCI
//  index is not present the returned value is NULL.
//--------------------------------------------------------------------------

PLCHAR_VECTOR
plP_FCI2FontName( PLUNICODE fci,
                  const FCI_to_FontName_Table lookup[], const int nlookup )
{
    int jlo = -1, jmid, jhi = nlookup;
    while ( jhi - jlo > 1 )
    {
        // Note that although jlo or jhi can be just outside valid
        // range (see initialization above) because of while condition
        // jlo < jmid < jhi and jmid must be in valid range.
        //
        jmid = ( jlo + jhi ) / 2;
        if ( fci > lookup[jmid].fci )
            jlo = jmid;
        else if ( fci < lookup[jmid].fci )
            jhi = jmid;
        else
            // We have found it!
            // fci == lookup[jmid].fci
            //
            return (PLCHAR_VECTOR) ( lookup[jmid].pfont );
    }
    // jlo is invalid or it is valid and fci > lookup[jlo].Unicode.
    // jhi is invalid or it is valid and fci < lookup[jhi].Unicode.
    // All these conditions together imply fci index cannot be found in lookup.
    // Mark lookup failure with NULL pointer.
    //
    return ( NULL );
}

//--------------------------------------------------------------------------
// void plsfont()
//
// Set the family, style and weight of the current font.
// This is a user-friendly front-end to plsfci.
// Note: A negative value signifies that this element should not be changed.
//--------------------------------------------------------------------------
void
c_plsfont( PLINT family, PLINT style, PLINT weight )
{
    PLUNICODE fci;

    plgfci( &fci );

    if ( family >= 0 )
    {
        // Bounds checking assumes symbol is last font
        if ( family > PL_FCI_SYMBOL )
            plwarn( "plsfont: Value for family is out of range" );
        else
            plP_hex2fci( (unsigned char) family, PL_FCI_FAMILY, &fci );
    }

    if ( style >= 0 )
    {
        // Bounds checking assumes oblique is last style
        if ( style > PL_FCI_OBLIQUE )
            plwarn( "plsfont: Value for style is out of range" );
        else
            plP_hex2fci( (unsigned char) style, PL_FCI_STYLE, &fci );
    }

    if ( weight >= 0 )
    {
        // Bounds checking assumes bold is last weight
        if ( weight > PL_FCI_BOLD )
            plwarn( "plsfont: Value for weight is out of range" );
        else
            plP_hex2fci( (unsigned char) weight, PL_FCI_WEIGHT, &fci );
    }

    plsfci( fci );
}

//--------------------------------------------------------------------------
// void plgfont()
//
// Get the family, style and weight of the current font.
// This is a user-friendly front-end to plgfci.
// Note: A NULL pointer signifies that this value should not be returned.
//--------------------------------------------------------------------------
void
c_plgfont( PLINT *p_family, PLINT *p_style, PLINT *p_weight )
{
    PLUNICODE     fci;
    unsigned char val;

    plgfci( &fci );

    if ( p_family )
    {
        plP_fci2hex( fci, &val, PL_FCI_FAMILY );
        *p_family = (PLINT) val;
    }

    if ( p_style )
    {
        plP_fci2hex( fci, &val, PL_FCI_STYLE );
        *p_style = (PLINT) val;
    }

    if ( p_weight )
    {
        plP_fci2hex( fci, &val, PL_FCI_WEIGHT );
        *p_weight = (PLINT) val;
    }
}


#undef PLSYM_H
#endif
