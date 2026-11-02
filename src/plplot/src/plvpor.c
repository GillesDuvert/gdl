//      Functions dealing with viewports.
//
// Copyright (C) 2004  Joao Cardoso
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

#include "plplotP.h"



//--------------------------------------------------------------------------
// void plvsta()
//
// Defines a "standard" viewport with seven character heights for
// the left margin and four character heights everywhere else.
//--------------------------------------------------------------------------

void
c_plvsta( void )
{
    PLFLT xmin, xmax, ymin, ymax;
    PLFLT lb, rb, tb, bb;

    if ( plsc->level < 1 )
    {
        plabort( "plvsta: Please call plinit first" );
        return;
    }

//  Find out position of subpage boundaries in millimetres, reduce by
//  the desired border, and convert back into normalized subpage
//  coordinates

    lb = 8.0 * plsc->chrht;
    rb = 5.0 * plsc->chrht;
    tb = 5.0 * plsc->chrht;
    bb = 5.0 * plsc->chrht;

    xmin = plP_dcscx( plP_mmdcx( (PLFLT) ( plP_dcmmx( plsc->spdxmi ) + lb ) ) );
    xmax = plP_dcscx( plP_mmdcx( (PLFLT) ( plP_dcmmx( plsc->spdxma ) - rb ) ) );
    ymin = plP_dcscy( plP_mmdcy( (PLFLT) ( plP_dcmmy( plsc->spdymi ) + tb ) ) );
    ymax = plP_dcscy( plP_mmdcy( (PLFLT) ( plP_dcmmy( plsc->spdyma ) - bb ) ) );

    plvpor( xmin, xmax, ymin, ymax );
}

//--------------------------------------------------------------------------
// void plvpor()
//
// Creates a viewport with the specified normalized subpage coordinates.
//--------------------------------------------------------------------------

void
c_plvpor( PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax )
{
    if ( plsc->level < 1 )
    {
        plabort( "plvpor: Please call plinit first" );
        return;
    }
    if ( ( xmin >= xmax ) || ( ymin >= ymax ) )
    {
        plabort( "plvpor: Invalid limits" );
        return;
    }
    if ( ( plsc->cursub <= 0 ) || ( plsc->cursub > ( plsc->nsubx * plsc->nsuby ) ) )
    {
        plabort( "plvpor: Please call pladv or plenv to go to a subpage" );
        return;
    }

    plsc->vpdxmi = plsc->spdxmi + ( plsc->spdxma - plsc->spdxmi ) * xmin;
    plsc->vpdxma = plsc->spdxmi + ( plsc->spdxma - plsc->spdxmi ) * xmax;
    plsc->vpdymi = plsc->spdymi + ( plsc->spdyma - plsc->spdymi ) * ymin;
    plsc->vpdyma = plsc->spdymi + ( plsc->spdyma - plsc->spdymi ) * ymax;

    plsc->vppxmi = plP_dcpcx( plsc->vpdxmi );
    plsc->vppxma = plP_dcpcx( plsc->vpdxma );
    plsc->vppymi = plP_dcpcy( plsc->vpdymi );
    plsc->vppyma = plP_dcpcy( plsc->vpdyma );

    plP_sclp( MAX( plsc->vppxmi, plsc->phyxmi ), MIN( plsc->vppxma, plsc->phyxma ),
        MAX( plsc->vppymi, plsc->phyymi ), MIN( plsc->vppyma, plsc->phyyma ) );

    plsc->level = 2;
}

//--------------------------------------------------------------------------
// void plvpas()
//
// Creates the largest viewport of the specified aspect ratio that fits
// within the specified normalized subpage coordinates.
//--------------------------------------------------------------------------

void
c_plvpas( PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax, PLFLT aspect )
{
    PLFLT spxmin, spxmax, spymin, spymax;
    PLFLT vpxmin, vpxmax, vpymin, vpymax;
    PLFLT xsize, ysize, nxsize, nysize;
    PLFLT xoffset, yoffset;

    if ( plsc->level < 1 )
    {
        plabort( "plvpas: Please call plinit first" );
        return;
    }
    if ( ( xmin >= xmax ) || ( ymin >= ymax ) )
    {
        plabort( "plvpas: Invalid limits" );
        return;
    }

    if ( aspect <= 0.0 )
    {
        c_plvpor( xmin, xmax, ymin, ymax );
        return;
    }

    plgspa( &spxmin, &spxmax, &spymin, &spymax );

    xsize = spxmax - spxmin;
    ysize = spymax - spymin;

    xoffset = xsize * xmin;
    yoffset = ysize * ymin;

    spxmax = spxmin + xsize * xmax;
    spxmin = spxmin + xsize * xmin;
    spymax = spymin + ysize * ymax;
    spymin = spymin + ysize * ymin;

    // Adjust size for the requested edging
    xsize = spxmax - spxmin;
    ysize = spymax - spymin;

    if ( aspect * xsize > ysize )
    {
        nxsize = ysize / aspect;
        nysize = ysize;
    }
    else
    {
        nxsize = xsize;
        nysize = xsize * aspect;
    }

// center plot within page

    vpxmin = 0.5 * ( xsize - nxsize ) + xoffset;
    vpxmax = vpxmin + nxsize;
    vpymin = 0.5 * ( ysize - nysize ) + yoffset;
    vpymax = vpymin + nysize;

    plsvpa( vpxmin, vpxmax, vpymin, vpymax );
}

//--------------------------------------------------------------------------
// void plvasp()
//
// Sets the edges of the viewport with the given aspect ratio, leaving
// room for labels.
//--------------------------------------------------------------------------

void
c_plvasp( PLFLT aspect )
{
    PLFLT spxmin, spxmax, spymin, spymax;
    PLFLT vpxmin, vpxmax, vpymin, vpymax;
    PLFLT xsize, ysize, nxsize, nysize;
    PLFLT lb, rb, tb, bb;

    if ( plsc->level < 1 )
    {
        plabort( "plvasp: Please call plinit first" );
        return;
    }

    lb = 8.0 * plsc->chrht;
    rb = 5.0 * plsc->chrht;
    tb = 5.0 * plsc->chrht;
    bb = 5.0 * plsc->chrht;

    plgspa( &spxmin, &spxmax, &spymin, &spymax );
    xsize  = spxmax - spxmin;
    ysize  = spymax - spymin;
    xsize -= lb + rb;           // adjust for labels
    ysize -= bb + tb;
    if ( aspect * xsize > ysize )
    {
        nxsize = ysize / aspect;
        nysize = ysize;
    }
    else
    {
        nxsize = xsize;
        nysize = xsize * aspect;
    }

// center plot within page

    vpxmin = .5 * ( xsize - nxsize ) + lb;
    vpxmax = vpxmin + nxsize;
    vpymin = .5 * ( ysize - nysize ) + bb;
    vpymax = vpymin + nysize;

    plsvpa( vpxmin, vpxmax, vpymin, vpymax );
}

//--------------------------------------------------------------------------
// void plsvpa()
//
// Sets the edges of the viewport to the specified absolute coordinates
// (mm), measured with respect to the current subpage boundaries.
//--------------------------------------------------------------------------

void
c_plsvpa( PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax )
{
    PLFLT sxmin, symin;

    if ( plsc->level < 1 )
    {
        plabort( "plsvpa: Please call plinit first" );
        return;
    }
    if ( ( xmin >= xmax ) || ( ymin >= ymax ) )
    {
        plabort( "plsvpa: Invalid limits" );
        return;
    }
    if ( ( plsc->cursub <= 0 ) || ( plsc->cursub > ( plsc->nsubx * plsc->nsuby ) ) )
    {
        plabort( "plsvpa: Please call pladv or plenv to go to a subpage" );
        return;
    }

    sxmin = plP_dcmmx( plsc->spdxmi );
    symin = plP_dcmmy( plsc->spdymi );

    plsc->vpdxmi = plP_mmdcx( (PLFLT) ( sxmin + xmin ) );
    plsc->vpdxma = plP_mmdcx( (PLFLT) ( sxmin + xmax ) );
    plsc->vpdymi = plP_mmdcy( (PLFLT) ( symin + ymin ) );
    plsc->vpdyma = plP_mmdcy( (PLFLT) ( symin + ymax ) );

    plsc->vppxmi = plP_dcpcx( plsc->vpdxmi );
    plsc->vppxma = plP_dcpcx( plsc->vpdxma );
    plsc->vppymi = plP_dcpcy( plsc->vpdymi );
    plsc->vppyma = plP_dcpcy( plsc->vpdyma );

    plP_sclp( plP_dcpcx( plsc->vpdxmi ), plP_dcpcx( plsc->vpdxma ),
        plP_dcpcy( plsc->vpdymi ), plP_dcpcy( plsc->vpdyma ) );

    plsc->level = 2;
}
