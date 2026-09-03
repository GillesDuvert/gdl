; Procedure to extract hershey fonts (rather, indexes into the slightly
; modified hershey.txt publicly available) from the compacted format
; used by GDL.
pro compacted_fonts_read_font, index, cf_font_table, unit, nchars, ctab, vects
; Read the #indexth font from the compacted hershey fonts file (IDL compatible)
; and return.
; use BYTEORDER for inter-os compatibility 
; assume only 40 fonts max in file - the standard, even if only 20 are present.
  COMPILE_OPT hidden, strictarr

  nfonts = n_elements(cf_font_table)/2
  if nfonts lt 1 then begin     ;Read cf_font_table?
     nfonts = 40
     cf_font_table = lonarr(2, nfonts) ;Get font directory
     point_lun, unit, 0
     readu, unit, cf_font_table
     byteorder, cf_font_table, /NTOHL	;To our order
  endif

  if index lt 0 or index ge nfonts then begin
     Message,/info,'Font index must be in range of 0 to '+strtrim(nfonts,2)
     return
  endif

  if cf_font_table[0,index] eq -1 then begin
     Message,/info,'Font ' + strtrim(index,2) + ' does not exist'
     return
  endif

; seems that if this bit is set then the font contains 224 characters
; instead of 96 (the 32 first ascii chars are not writeable and do not exist).
  mask = '7fffffff'xl
  if (cf_font_table[1,index] and (not mask)) ne 0 then nchars = 256-32 else $
     nchars = 128-32
  print,nchars
  len = cf_font_table[1, index] and mask
  ctab = replicate({CTAB, nvecs: 0b, width: 0b, offset: 0}, nchars)

  point_lun, unit, cf_font_table[0,index] and '0fffffff'xl
  readu, unit, ctab
  k = 0                                              ;Current offset
  for i=0, nchars-1 do begin                         ;swap shorts
     j = ctab[i].offset
     byteorder, j, /NTOHS
     if j eq 0 then j = k $	;Put empty chars in their proper place
     else if j ne k then print,'Inconsistent vector offset/length, chr = ',$
                               i+32
     ctab[i].offset = j
     k = k + fix(ctab[i].nvecs)
  endfor

  vects = intarr(len/2)
  on_ioerror, bad
  readu, unit, vects 		;Read the vectors
  bad: byteorder, vects, /NTOHS
end

PRO cf_unpack_vectors, in, x, y, pen_up
; decode the ints and return the X, Y, and Pen_up code.

COMPILE_OPT hidden, strictarr

x = ishft(in, -7) and 127	;X and Y
y = in and 127
neg = where(x and 64, count)
if count gt 0 then x[neg] = x[neg]-128
neg = where(y and 64, count)
if count gt 0 then y[neg] = y[neg]-128
pen_up = (in and 16384) ne 0
end

; just type "gdl_read_compacted_hershey" to read all the fonts,
; otherwise use fontnum.
; reads the compacted hershey file (idl-compatible) named "hersh1.chr"
; finds the corresponding hershey signature in "hershey_longlines.txt"
; creates the "coresp.txt" with correspondences
; if non-hershey (modified glyphs, probably with EFONT are present, they go in
; "non_hershey.txt".

; at the moment "hershey_longlines.txt" does not contain the eastern Kanji etc
; hershey fonts, but it could.

; produce also a .h file containing the mapping ascii->hershey number per font.
; used in GDL (see plplot/src/plsym.c) 

; these files can be used by gdl_create_font_table.pro to recreate a
; compacted hershey file.
pro gdl_read_compacted_hershey, fontnum
  table="hersh1.chr"
  outlist="coresp.txt"
  additional="non_hershey.txt"
  additional_opened=0
  line=''
  fullline=''
  subline=''
  charnum=0
  id=0
  npairs=0
  npairs=0
  extent=''
  number=0
  iout=9000 ; for additional glyphs
  
  hrsh="hershey_longlines.txt"
  nhersh=FILE_LINES(hrsh)-90 ; drop the 90 first lines, low-res fonts
  openr,lun,hrsh,/get_lun
  hershentry={charnum:0, npairs:0, extent:'', string:''}
  hersh=replicate(hershentry,nhersh)
  ; read hershey
  for iglyph=0,89 do readf,lun,fullline
  for iglyph=0,nhersh-1 do begin
     readf,lun,fullline
     reads,fullline,charnum,npairs,extent,line,format="(I5,I3,A2,A)"
     hersh[iglyph].charnum=charnum
     hersh[iglyph].npairs=npairs
     hersh[iglyph].extent=extent
     if npairs gt 0 then hersh[iglyph].string=line
  end
  close,lun
  free_lun,lun
  
  openw,include,"hershey_mapping.h",/get_lun
  
  openw,index,outlist,/get_lun
  r=82b
  openr,unit,table,/get_lun
  
  if n_elements(fontnum) eq 0 then suggested_list=[[3:9],[11:18],20] else suggested_list=fontnum
  list=indgen(41)
 
  foreach fontnum,list do begin
     ntouse=128-32
     if fontnum eq 3 then ntouse=256-32
     w=where(suggested_list eq fontnum, count)
     if count eq 0 then begin
        printf,include,"static const int font"+string(fontnum+1,format='(i2.2)')+'['+strtrim(ntouse,2)+'] = {'
        goto, nextFont
     endif
     print,"Font #"+strtrim(fontnum,2)
     compacted_fonts_read_font, fontnum, cf_font_table, unit, nchars, ctab, vects
     ; remove bad char in 9 number 35
     if fontnum eq 9 then ctab[35].nvecs=0 ;

     start=1
     printf,include,"static const int font"+string(fontnum+1,format='(i2.2)')+'['+strtrim(ntouse,2)+'] = {'

     for i=0, ntouse-1 do begin ;n_elements(ctab)-1 do begin
        currentchar=fix(fix(i+32,type=1),type=7)
        printf,index,fontnum, i+1,format='($,I3,I5)'
        nvects=ctab[i].nvecs
        if nvects gt 0 then begin
           subvect=vects[ctab[i].offset:ctab[i].offset+ctab[i].nvecs-1]
           cf_unpack_vectors, subvect, x, y, pen_up
           w=where(pen_up eq 1, count)
           if count gt 1 then nvects+=(count-1) ; allow for penup
           width=ctab[i].width
           right=width/2
           left=-right+1
           right=left+width
           chleft=left+r-1
           chright=right+r-1
           chleft=fix(chleft,type=1)
           chright=fix(chright,type=1)
           foreach ix,[0,1,-1] do begin;,1,-1] do begin
              foreach iy,[0,1,-1] do begin;,1,-1,2,-2,3,-3,4,-4] do begin
                 signature=''
                 for j=0,ctab[i].nvecs-1 do begin
                    x = ishft(subvect[j], -7) and 127 ;Get X and Y components
                    y = subvect[j] and 127
                    x+=(r+left-1) ; charx
                    y=10-y+r      ; chary
                    x+=ix
                    y+=iy
                    x=fix(x,type=1) & y=fix(y,type=1)
                    x and= '7F'xb & y and= '7F'xb
                    pen_up = (j ne 0) and ((subvect[j] and 16384) ne 0)
                    if pen_up then signature+=string(" R",format='($,A2)',/PRINT)
                    signature+=string(string(x),string(y),format='($,A1,A1)',/PRINT)
                 endfor
                 ; save basic signature for modified glyph used by IDL 
                 saved_signature=signature
                 ; try  to find in hersh.string
                 w=where(hersh.string eq signature, count)
                 ;stop,nvects,string(chleft),string(chright),signature
                 number=0
                 if (count gt 0) then begin
                    number=hersh[w[0]].charnum
                    goto, nextChar
                 endif
              end
           end
           nextChar:
           if (count eq 0) then begin ;stop,nvects,string(chleft),string(chright),signature
              column=i mod 16 & line=i/16
                    ;; read,number,prompt="Font #"+strtrim(fontnum,2)+": line "+strtrim(line,2)+", column "+strtrim(column,2)+": enter number or 0 to add glyph "
                    ;; if number eq 0 then begin
                       if ~additional_opened then begin
                          openw,out,additional,/get
                          additional_opened=1
                       end
                       number=iout
                       printf,out,iout++,format='($,I5)'
                       printf,out,nvects+1,format='($,I3)'
                       printf,out,string(chleft),string(chright),format='($,A1,A1)'
                       printf,out,saved_signature
                    ;;end
                 endif
           printf,index,number,currentchar,format='(I12,3x,A)'
           if start then begin
              printf,include,number, format='($,I5)'
              start=0
           endif else printf,include,number, format='($,",",I5)'
           if number eq 0 then print, "Failed: "+signature
        endif else begin
           printf,index,0,format='(I12)'
           if start then begin
              printf,include,0, format='($,I1)'
              start=0
           endif else printf,include,0, format='($,",",I1)'
        endelse
     end
nextFont:
        printf,include,'};' & printf,include
  end
  zz="font"+string(bindgen(20)+1,format='(I2.2,:,",")')
  printf,include,"static const int* fontindex[]={",zz,"};"
  close,unit,index,include
  free_lun,unit,index,include
end
