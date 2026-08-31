//      Contains all prototypes for driver functions.
//
//  Copyright (C) 2004  Andrew Roach
//  Copyright (C) 2005  Thomas J. Duck
//  Copyright (C) 2006  Andrew Ross
//
//  This file is part of PLplot.
//
//  PLplot is free software; you can redistribute it and/or modify
//  it under the terms of the GNU Library General Public License as published
//  by the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  PLplot is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Library General Public License for more details.
//
//  You should have received a copy of the GNU Library General Public License
//  along with PLplot; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//

#ifndef __DRIVERS_H__
#define __DRIVERS_H__

#include "pdf.h"
#include "plstrm.h"

#ifdef __cplusplus
extern "C" {
#endif

// The order of these results (in alphabetic order by source file name and device)
// may be found from the following command:
//
// find drivers -type f | grep -v .git | grep -v deprecated | xargs grep 'plD_dispatch_init.*)$'  | grep -v // |sort
//
// with suitable editing afterward to remove file name prepend "PLDLLIMPEXP_DRIVER " and append ";"

PLDLLIMPEXP_DRIVER void plD_dispatch_init_mem( PLDispatchTable *pdt );
PLDLLIMPEXP_DRIVER void plD_dispatch_init_null( PLDispatchTable *pdt );
PLDLLIMPEXP_DRIVER void plD_dispatch_init_psc( PLDispatchTable *pdt );
PLDLLIMPEXP_DRIVER void plD_dispatch_init_ps( PLDispatchTable *pdt );
PLDLLIMPEXP_DRIVER void plD_dispatch_init_svg( PLDispatchTable *pdt );
PLDLLIMPEXP_DRIVER void plD_dispatch_init_wxwidgets( PLDispatchTable *pdt );
PLDLLIMPEXP_DRIVER void plD_dispatch_init_xw( PLDispatchTable *pdt );

// Prototypes for plot buffer calls.

void plbuf_init( PLStream * );
void plbuf_line( PLStream *, short, short, short, short );
void plbuf_polyline( PLStream *, short *, short *, PLINT );
void plbuf_eop( PLStream * );
void plbuf_bop( PLStream * );
void plbuf_tidy( PLStream * );
void plbuf_state( PLStream *, PLINT );
void plbuf_esc( PLStream *, PLINT, void * );
void plbuf_di( PLStream * );
void plbuf_setsub( PLStream * );
void plbuf_ssub( PLStream * );
void plbuf_clip( PLStream * );
PLDLLIMPEXP void * plbuf_save( PLStream *, void * );
PLDLLIMPEXP void * plbuf_switch( PLStream *, void * );
PLDLLIMPEXP void plbuf_restore( PLStream *, void * );

void plFlushBuffer( PLStream *pls, PLBOOL restart, size_t amount );

#ifdef __cplusplus
}
#endif

#endif  // __DRIVERS_H__
