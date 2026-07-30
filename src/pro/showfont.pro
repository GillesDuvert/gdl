;+
; NAME: 
;      SHOWFONT
;
; PURPOSE: 
;          Uses current graphics device to draw a map of characters
;          available in the font specified in argument
;
; CATEGORY: 
;          General 
;
; CALLING SEQUENCE:
;          showfont, num, 'title' ; table of font num entitled 'title'
;
; KEYWORD PARAMETERS: 
;          /encapsulated                ; If this keyword is set, and if the current graphics
;			                  device is "PS", makes encapsulated PostScript outputf 
;          /tt_font                     ; ignored (just for compatibility)
;          base = 16                    ; number of columns in the table 
;          beg = 32                     ; first character
;          fin = num eq 3 ? 255 : 127   ; last character
;
; OUTPUTS:
;          None.
;
; OPTIONAL OUTPUTS:
;          None.
;
; COMMON BLOCKS:
;          None.
;
; SIDE EFFECTS:
;          Draws a font table on the current graphic device.
;
; RESTRICTIONS:
;          None.
;
; PROCEDURE:
;
; EXAMPLE:
;          showfont, 9, 'GDL math symbols'   ; show mappings for font 9
;
; MODIFICATION HISTORY:
; 	Written by: Sylwester Arabas (2008/12/28)
;-
; LICENCE:
; Copyright (C) 2008,
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   
;-

pro showfont, num, name, encapsulated=eps, tt_font=tt, offset=offin
  
    on_error, 2
  
  ; handling default keyword values

  if keyword_set(eps) and !d.name eq 'PS' then device, /encapsulated, /color ; , bits_per_pixel=8
  base = 16
  beg = 32
  if keyword_set(tt) then fin=255 else fin = num eq 3 ? 255 : 127
  if not keyword_set(name) then name = ''
  if not keyword_set(offin) then offin = 0
  if not keyword_set(tt) then offin=0
  ; save old decomposition
  device, get_decomposed=old
  device, decomposed=0

  ; save current !P.FONT
  oldp=!P.FONT

  ; we use Hershey fonts for the grid ans axes labelling:
  !P.FONT=-1
  ; constructing horizontal and vertical grid lines
  n_hor = (fin + 1 - beg) / base + 1
  h_x = base * byte(128 * indgen(2 * (n_hor))) / 128
  h_x = rebin(h_x, 4 * n_hor, /sample)
  h_x = (double(h_x))[1:4 * n_hor - 1] - .5
  h_y = beg + indgen(n_hor) * base
  h_y = rebin(h_y, 4 * n_hor, /sample)
  h_y = (double(h_y))[0:4 * n_hor - 2] - base/2.
  v_x = base - indgen(4 * base - 1) / 4 - .5
  v_y = byte(128 * indgen(2 * (base))) / 128
  v_y = rebin(v_y, 4 * base, /sample)
  v_y = (double(v_y))[1:4 * base - 1]  * base * ((fin + 1 - beg) / base) + beg - base / 2.

  ; plotting grid and title
  plot,  [h_x, v_x], [h_y+offin, v_y+offin],/xstyle,/ystyle, $
     title='Font ' + strtrim(string(num), 2) + ', ' + name, $
     xrange=[-0.5, base-0.5], $
     yrange=[base * ((fin +offin + 1) / base) -base/2, beg + offin - base/2], $
     yticks=n_hor-1, $
     xticks=base, $
     ytickformat='(I8)',$
     xtitle='char mod ' + strtrim(string(base), 2), $
     ytitle=strtrim(string(base), 2) + ' * (char / ' + strtrim(string(base), 2) + ')'
  ; plotting characters

  if keyword_set(tt) then begin
     !P.FONT=1
     DEVICE, SET_FONT=num ;, /TT_FONT
     for i = offin, offin+255 do begin
        c=STRING(i, FORMAT='("!Z(",Z4.4,")")')
        xyouts, (i mod base), base * (i / base), c,CHARSIZE=2
     end
  endif else begin
    for c = beg, fin do $
      xyouts, (c mod base), base * (c / base), '!' + strtrim(string(num), 2) + string(byte(c)),CHARSIZE=2
  endelse
  ; reset
  !P.FONT=oldp
  device,decomposed=old
end
