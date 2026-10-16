// PLplot MEM (in user-supplied memory) device driver.
// The idea here is that the user will specify the Y by X by RGB
// area in which to plot using the plsmem function (added by me).
//
// This is a bare-bones driver which allows one to plot on an existing
// image in memory.  This is useful if the user has an image in memory
// that he wants to decorate with PLPLOT.
//
// Contributed by Doug Hunt
// Included in PLplot by Rafael Laboissiere on Sat Feb 22 18:34:06 CET 2003
//

#include "plDevs.h"

#ifdef PLD_mem

#include "plplotP.h"
#include "drivers.h"

// Device info
PLDLLIMPEXP_DRIVER const char* plD_DEVICE_INFO_mem = "mem:User-supplied memory device:-1:mem:46:mem\n";

void plD_dispatch_init_mem( PLDispatchTable *pdt );

void plD_init_mem( PLStream * );
void plD_line_mem( PLStream *, short, short, short, short );
void plD_line_mem_3D( PLStream *, short, short, short, short );
void plD_polyline_mem( PLStream *, short *, short *, PLINT );
void plD_polyline_mem_3D( PLStream *, short *, short *, PLINT );
void polyline_fill_mem( PLStream *, short *, short *, PLINT );
void plD_eop_mem( PLStream * );
void plD_bop_mem( PLStream * );
void plD_tidy_mem( PLStream * );
void plD_state_mem( PLStream *, PLINT );
void plD_esc_mem( PLStream *, PLINT, void * );

#define SPECIFIC_3D
#define LINE3D_FUNCTION plD_line_mem_3D
#define POLYLINE3D_FUNCTION plD_polyline_mem_3D

//define LINE2D, POLYLINE2D
#define LINE2D plD_line_mem
#define POLYLINE2D plD_polyline_mem
#include "plplot3d.h"

#undef MAX
#undef ABS
#define MAX( a, b )    ( ( a > b ) ? a : b )
#define ABS( a )       ( ( a < 0 ) ? -a : a )

#define MAX_INTENSITY    255
void pathFill(PLStream *pls);
void plD_dispatch_init_mem( PLDispatchTable *pdt )
{
  currDispatchTab = pdt;
  Status3D = 0;
    pdt->pl_MenuStr = "User-supplied memory device";
    pdt->pl_DevName = "mem";
    pdt->pl_type     = plDevType_Null;
    pdt->pl_seq      = 45;
    pdt->pl_init     = (plD_init_fp) plD_init_mem;
    pdt->pl_line     = (plD_line_fp) plD_line_mem;
    pdt->pl_polyline = (plD_polyline_fp) plD_polyline_mem;
    pdt->pl_eop      = (plD_eop_fp) plD_eop_mem;
    pdt->pl_bop      = (plD_bop_fp) plD_bop_mem;
    pdt->pl_tidy     = (plD_tidy_fp) plD_tidy_mem;
    pdt->pl_state    = (plD_state_fp) plD_state_mem;
    pdt->pl_esc      = (plD_esc_fp) plD_esc_mem;
}

//--------------------------------------------------------------------------
// plD_init_mem()
//
// Initialize device (terminal).
//--------------------------------------------------------------------------
//special 3D transform for z-buffering

static PLFLT Get3DTransformedZ(int xs, int ys) {
	PLFLT zs = Data3d.zValue;
	if (Status3D == 1) { //enable use everywhere.
		PLFLT x = xs, y = ys, z = zs;
		// x and Y are in raw device coordinates.
		// convert to NORM
		//  x = my_plP_pcdcx(x);
		//  y = my_plP_pcdcy(y);
		x = (x - plsc->phyxmi) / (double) plsc->phyxlen;
		y = (y - plsc->phyymi) / (double) plsc->phyylen;
		//here it is !P.T not a c/c++ transposed matrix
		PLFLT zz;
		zz = x * Data3d.T[8] + y * Data3d.T[9] + z * Data3d.T[10] + Data3d.T[11];
		// convert to device again
		//  *xs = (int) (my_plP_dcpcx(xx));
		//  *ys = (int) (my_plP_dcpcy(yy));
		// z will be between 0 and 1:
		zs = zz;
	}
	return zs;
}


void
plD_init_mem( PLStream *pls )
{
    // plsmem must have already been called to set pls->dev to the
    // user supplied plotting area.  The dimensions of the plot area
    // have also been set by plsmem.  Verify this.
    //

    if ( ( pls->phyxma == 0 ) || ( pls->dev == NULL ) )
    {
        plexit( "Must call plsmem first to set user plotting area!" );
    }

    if ( pls->dev_mem_alpha == 1 )
    {
        plexit( "The mem driver does not support alpha values! Use plsmem!" );
    }

    plP_setpxl( (PLFLT) 4, (PLFLT) 4 ); // rough pixels/mm on *my* screen


    pls->color     = 1;         // Is a color device
    pls->dev_fill0 = 1;         // Handle solid fills
    pls->dev_fill1 = 0;         // Use PLplot core fallback for pattern fills
    pls->nopause   = 1;         // Don't pause between frames
    pls->use_unicode  = 0;      // wants text as unicode
    pls->dev_alt_unicode  = 0;      // use alternate filling for unicode fonts
}

#define sign( a )    ( ( a < 0 ) ? -1 : ( ( a == 0 ) ? 0 : 1 ) )


//2D line plot 

void
plD_line_mem( PLStream *pls, short x1a, short y1a, short x2a, short y2a )
{
    int           i;
    PLINT         idx;
    int           x1 = x1a, y1 = y1a, x2 = x2a, y2 = y2a;
    PLINT         x1b, y1b, x2b, y2b;
    PLFLT         length, fx, fy, dx, dy;
	unsigned char *mem = (unsigned char *) pls->dev;
    PLINT         xm   = pls->phyxma;
    PLINT         ym   = pls->phyyma;

	// Take mirror image, since (0,0) must be at top left

    y1 = ym - ( y1 - 0 );
    y2 = ym - ( y2 - 0 );

    x1b    = x1, x2b = x2, y1b = y1, y2b = y2;
    length = (PLFLT) sqrt( (double)
        ( ( x2b - x1b ) * ( x2b - x1b ) + ( y2b - y1b ) * ( y2b - y1b ) ) );

    if ( length == 0. )
		length = 1.;
    dx = ( x2 - x1 ) / length;
    dy = ( y2 - y1 ) / length;

	fx = x1;
	fy = y1;
    mem[3 * xm * y1 + 3 * x1 + 0] = pls->curcolor.r;
    mem[3 * xm * y1 + 3 * x1 + 1] = pls->curcolor.g;
    mem[3 * xm * y1 + 3 * x1 + 2] = pls->curcolor.b;

    mem[3 * xm * y2 + 3 * x2 + 0] = pls->curcolor.r;
    mem[3 * xm * y2 + 3 * x2 + 1] = pls->curcolor.g;
    mem[3 * xm * y2 + 3 * x2 + 2] = pls->curcolor.b;

    for ( i = 1; i <= (int) length; i++ )
    {
        fx          += dx;
        fy          += dy;
        idx          = 3 * xm * (PLINT) fy + 3 * (PLINT) fx;
			mem[idx + 0] = pls->curcolor.r;
			mem[idx + 1] = pls->curcolor.g;
			mem[idx + 2] = pls->curcolor.b;
		}
	}

//2D line plot as for other devices, but this one uses the zbuffer and the z position passed in pls->zvalue

void
plD_line_mem_3D(PLStream *pls, short x1a, short y1a, short x2a, short y2a) {
	unsigned char *mem = (unsigned char *) pls->dev;
    PLINT         M   = pls->phyxma;
    PLINT         N   = pls->phyyma;

	int i;
	PLINT idx,idzx;
	short zval;
	short* zbuffer = (short*) pls->dev_data;
	int x1 = x1a, y1 = y1a, x2 = x2a, y2 = y2a;
	int zx1 = x1a, zy1 = y1a, zx2 = x2a, zy2 = y2a;
	PLFLT zlength, zfx, zfy, zdx, zdy;
	
	SelfTransform3D(&x1, &y1);
	SelfTransform3D(&x2, &y2);

	PLFLT pz;
	PLFLT length, fx, fy, dx, dy;

	// Take mirror image, since (0,0) must be at top left

	y1 = N - (y1 - 0);
	y2 = N - (y2 - 0);

	length = (PLFLT) sqrt((double)	((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)));
	if (length == 0.) length = 1.;
	dx = (x2 - x1) / length;
	dy = (y2 - y1) / length;
	fx = x1;
	fy = y1;

	// mirror?
	zlength = (PLFLT) sqrt((double)	((zx2 - zx1) * (zx2 - zx1) + (zy2 - zy1) * (zy2 - zy1)));
	if (length == 0.) zlength = 1.;
	zdx = (zx2 - zx1) / zlength;
	zdy = (zy2 - zy1) / zlength;
	zfx = zx1;
	zfy = zy1;


	if (pls->dev_zbuffering) {
        idx = 3 * (M * y1 + x1);
		idzx = M * (N-y1) + x1;
		pz = Get3DTransformedZ(zx1, zy1); //non-projected
		zval = pz * 32768;
		if (zval > zbuffer[idzx]) {
			zbuffer[idzx]=zval;
			mem[idx + 0] = pls->curcolor.r;
			mem[idx + 1] = pls->curcolor.g;
			mem[idx + 2] = pls->curcolor.b;
		}
		idx = 3 * (M * y2 + x2);
		idzx = M * (N-y2) + x2;
		pz = Get3DTransformedZ(zx2, zy2); //non-projected
		zval = pz * 32768;
		if (zval > zbuffer[idzx]) {
			zbuffer[idzx]=zval;
			mem[idx + 0] = pls->curcolor.r;
			mem[idx + 1] = pls->curcolor.g;
			mem[idx + 2] = pls->curcolor.b;
		}
		for (i = 1; i <= (int) length; i++) {
			fx += dx;    fy += dy; 
			idx = 3 * (M * (PLINT) fy + (PLINT) fx);
			idzx = M * (N - (PLINT) fy) + (PLINT) fx;
			zfx += zdx; zfy += zdy;
			pz = Get3DTransformedZ(zfx, zfy);
			zval = pz * 32768; 
			if (zval > zbuffer[idzx]) {
				zbuffer[idzx]=zval;
				mem[idx + 0] = pls->curcolor.r;
				mem[idx + 1] = pls->curcolor.g;
				mem[idx + 2] = pls->curcolor.b;
			}
		}
	} else {
		idx = 3 * (M * y1 + x1);
		mem[idx + 0] = pls->curcolor.r;
		mem[idx + 1] = pls->curcolor.g;
		mem[idx + 2] = pls->curcolor.b;

		idx = 3 * (M * y2 + x2);
		mem[idx + 0] = pls->curcolor.r;
		mem[idx + 1] = pls->curcolor.g;
		mem[idx + 2] = pls->curcolor.b;

		for (i = 1; i <= (int) length; i++) {
			fx += dx;
			fy += dy;
			idx = 3 * (M * (PLINT) fy + (PLINT) fx);

			mem[idx + 0] = pls->curcolor.r;
			mem[idx + 1] = pls->curcolor.g;
			mem[idx + 2] = pls->curcolor.b;
		}
	}
}

 
float edge_function(plVertex a, plVertex b, plVertex c) {
    return (c.x - a.x) * (b.y - a.y) - (c.y - a.y) * (b.x - a.x);
}

void
plD_polyline_mem( PLStream *pls, short *xa, short *ya, PLINT npts )
{
    int i;
    for ( i = 0; i < npts - 1; i++ )
        plD_line_mem( pls, xa[i], ya[i], xa[i + 1], ya[i + 1] );
}
void
plD_polyline_mem_3D( PLStream *pls, short *xa, short *ya, PLINT npts )
{
    int i;
    for ( i = 0; i < npts - 1; i++ )
        plD_line_mem_3D( pls, xa[i], ya[i], xa[i + 1], ya[i + 1] );
}

	void
polyline_fill_mem(PLStream *pls, short *xa, short *ya, PLINT len) {
	PLINT xamin = xa[0];
	PLINT xamax = xa[0];
	PLINT yamin = ya[0];
	PLINT yamax = ya[0];
	for (int i = 1; i < len; ++i) {
		xamin = MIN(xamin, xa[i]);
		xamax = MAX(xamax, xa[i]);
		yamin = MIN(yamin, ya[i]);
		yamax = MAX(yamax, ya[i]);
	}
	PLINT M = pls->phyxma;
	PLINT N = pls->phyyma;
	xamin = MAX(xamin, 0);
	yamin = MAX(yamin, 0);
	xamax = MIN(xamax, M);
	yamax = MIN(yamax, N);
	PLINT m = xamax - xamin + 1;
	PLINT n = yamax - yamin + 1;
	if (n <= 2 || m <= 2) return;
	unsigned char *mem = (unsigned char *) pls->dev;
	unsigned char *pixels = (char*) malloc(m * n);
	memset(pixels, 0, m * n);
	int *subpath_lengths = (int *) malloc(sizeof(*subpath_lengths) * 1);
	subpath_lengths[0]=len;
	plSTBTTFill_short(xa, ya, len, 1, subpath_lengths, pixels, m, n, xamin, yamin );
	unsigned char r = pls->curcolor.r;
	unsigned char g = pls->curcolor.g;
	unsigned char b = pls->curcolor.b;
		for (int j = 0, jref=N-yamax; j < n; ++j, ++jref) {
			int ref = 3 * M * jref; 
			for (int i = 0; i < m; ++i) {
				if (pixels[(n-j-1) * m + i] > 127) {
				int iref = xamin + i;
				int ref2 = ref + 3 * iref;
				mem[ref2++]=r;
				mem[ref2++]=g;
				mem[ref2  ]=b;
				}
			}
		}
	free(pixels);
	free(subpath_lengths);
}
//--------------------------------------------------------------------------
//  static void fill_path( PLStream *pls )
//
//  Fill special arrangement of values defining multiple path at once
//--------------------------------------------------------------------------
static void path_fill_mem( PLStream *pls) {

  if (Status3D == 1) { //enable use everywhere.
    //perform conversion on the fly
    for (PLINT i = 0; i < pls->dev_npath; ++i) {
      PLINT *x = pls->dev_pathx[i];
      PLINT *y = pls->dev_pathy[i];
      for (PLINT j = 0; j < pls->dev_pathnxy[i]; ++j) {
        // 3D convert, must take into account that y is inverted.
        int ix=x[j];
        int iy=y[j];
        if (ix >= 0) { //avoid transforming the negative codes...
          SelfTransform3D(&ix, &iy);
        x[j]=ix;
        y[j]=iy;
        }
      }
    }
  }
  pathFill(pls);
}
void pathFill(PLStream *pls) {
	int len=0;
	for (int i = 0; i < pls->dev_npath; ++i) len+= pls->dev_pathnxy[i];
	PLINT xmin = 32768;
	PLINT xmax = 0;
	PLINT ymin = 32768;
	PLINT ymax = 0;
   for (int i = 0; i < pls->dev_npath; ++i) {
    PLINT* x = pls->dev_pathx[i];
    PLINT* y = pls->dev_pathy[i];
	for (int j = 1; j < pls->dev_pathnxy[i]; ++j) {
		if (x[i] >= 0) {
			xmin = MIN(xmin, x[j + 1]);
			xmax = MAX(xmax, x[j + 1]);
			ymin = MIN(ymin, y[j + 1]);
			ymax = MAX(ymax, y[j + 1]);
		}
	}
   }
	PLINT M = pls->phyxma;
	PLINT N = pls->phyyma;
	xmin = MAX(xmin, 0);
	ymin = MAX(ymin, 0);
	xmax = MIN(xmax, M);
	ymax = MIN(ymax, N);
	PLINT m = xmax - xmin + 1;
	PLINT n = ymax - ymin + 1;
	if (n <= 2 || m <= 2) return;
	unsigned char *mem = (unsigned char *) pls->dev;
	unsigned char *pixels = (char*) malloc(m * n);
	memset(pixels, 0, m * n);
	int *subpath_lengths = (int *) malloc(sizeof(*subpath_lengths) * pls->dev_npath);
	plSTBTTFill_int(pls->dev_pathx[0], pls->dev_pathy[0], len, pls->dev_npath , pls->dev_pathnxy, pixels, m, n, xmin, ymin );
	unsigned char r = pls->curcolor.r;
	unsigned char g = pls->curcolor.g;
	unsigned char b = pls->curcolor.b;
	for (int j = 0, jref=N-ymax; j < n; ++j, ++jref) {
		int ref = 3 * M * jref; 
		for (int i = 0; i < m; ++i) {
			if (pixels[(n-j-1) * m + i] > 127) {
			int iref = xmin + i;
			int ref2 = ref + 3 * iref;
			mem[ref2++]=r;
		    mem[ref2++]=g;
		    mem[ref2]=b;
			}
		}
	}
	free(pixels);
}
void
plD_eop_mem( PLStream *pls )
{
    // Set the 'dev' member (which holds the user supplied memory image)
    // to NULL here so it won't be freed when PLplot is closed.
    // (the user is responsible for freeing it when ready).
    //
    pls->dev = NULL;
}

void
plD_bop_mem( PLStream * PL_UNUSED( pls ) )
{
// Nothing to do here
}

void
plD_tidy_mem( PLStream * PL_UNUSED( pls ) )
{
// Nothing to do here
}

void
plD_state_mem( PLStream * PL_UNUSED( pls ), PLINT PL_UNUSED( op ) )
{
// Nothing to do here
	}

	void
	plD_esc_mem(PLStream *pls, PLINT op, void *ptr) {
		switch (op) {
			case PLESC_FILL_POLYGON: // fill polygon
				if (Status3D == 1) { //enable use everywhere.
					//perform conversion on the fly
					for (PLINT i = 0; i < pls->dev_npts; ++i) {
						int x = pls->dev_x[i];
						int y = pls->dev_y[i];
						// 3D convert, must take into account that y is inverted.
						SelfTransform3D(&x, &y);
						pls->dev_x[i] = x;
						pls->dev_y[i] = y;
					}
				}
				polyline_fill_mem(pls, pls->dev_x, pls->dev_y, pls->dev_npts);
				break;
    case PLESC_FILL_PATH:
        path_fill_mem( pls );
        break;
			case PLESC_3D:
				Set3D(ptr);
				break;
			case PLESC_2D:
				UnSet3D();
				break;
		}
	}
#endif                          // PLD_mem
