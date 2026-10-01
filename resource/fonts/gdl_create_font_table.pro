; re-create (if lost) the compacted hershey font file used by GDL,
; aptly named "hersh1.chr" (for full compatibility with IDL), that must lie
; in GDLDATADIR
; provides a file named "newhersh1.chr" that can be put in resources/fonts and/or in $GDLDATADIR

PRO getvects, in, x, y, pen_up 
x = ishft(in, -7) and 127
y = in and 127
neg = where(x and 64, count)
if count gt 0 then x[neg] = x[neg]-128
neg = where(y and 64, count)
if count gt 0 then y[neg] = y[neg]-128
pen_up = (in and 16384) ne 0
end

pro gdl_create_font_table, plot=plot
  if n_elements(plot) eq 0 then plot=0
  table="newhersh1.chr"
  hershey_longlines="hershey_longlines.txt"
  hershey_supplement="non_hershey.txt"
  r=82b  & rvals=replicate(r,256) ; "R"
  line=''
  fullline=''
  subline=''
  fontnum=0
  charnum=0
  npairs=0
  nval=0
  ; get max glyph number
  nhershey_glyphs1=file_lines(hershey_longlines)
  nhershey_glyphs2=file_lines(hershey_supplement)
  nhershey_glyphs= nhershey_glyphs1 +  nhershey_glyphs2
  ; last 
  print, "NHERSHEY_GLYPHS: ",nhershey_glyphs
  
  glyph={hershey_glyphs, charnum:0, nval:0, npenup:0, left:0, right:0, string:''}
  hershey_glyphs=replicate(glyph,nhershey_glyphs)

  ; read first part
  openr,hershey_lun,hershey_longlines,/get_lun

  for iglyph=0,nhershey_glyphs1-1 do begin
        readf,hershey_lun,fullline
        reads,fullline,charnum,npairs,line,format="(I5,I3,A)"
        hershey_glyphs[iglyph].charnum=charnum
        if npairs gt 0 then begin
           hershey_glyphs[iglyph].nval=(npairs)*2 ; left, right
           ret=strsplit(line," R",/reg,count=count)
           hershey_glyphs[iglyph].npenup=count
           bytes=fix(line,type=1)
           hershey_glyphs[iglyph].left=bytes[0]
           hershey_glyphs[iglyph].right=bytes[1]
           if n_elements(bytes) gt 2 then hershey_glyphs[iglyph].string=string(bytes[2:-1])
        endif
     end
  close,hershey_lun
  free_lun,hershey_lun
  ; read second part
  openr,hershey_lun,hershey_supplement,/get_lun
  for iglyph=nhershey_glyphs1,nhershey_glyphs-1 do begin
        readf,hershey_lun,fullline
        reads,fullline,charnum,npairs,line,format="(I5,I3,A)"
        hershey_glyphs[iglyph].charnum=charnum
        if npairs gt 0 then begin
           hershey_glyphs[iglyph].nval=(npairs)*2 ; left, right
           ret=strsplit(line," R",/reg,count=count)
           hershey_glyphs[iglyph].npenup=count
           bytes=fix(line,type=1)
           hershey_glyphs[iglyph].left=bytes[0]
           hershey_glyphs[iglyph].right=bytes[1]
           if n_elements(bytes) gt 2 then hershey_glyphs[iglyph].string=string(bytes[2:-1])
        endif
     end
  close,hershey_lun
  free_lun,hershey_lun
  hershey_glyphs.left-=r
  hershey_glyphs.right-=r
; read correspondence table
  coresp="coresp.txt"
  nchars=file_lines(coresp)
  char={chars, fontnum:0, charnum:0, hershey_num:0}
  fontstruct=replicate(char,nchars)
  openr,coresp_lun,coresp,/get_lun
  for ichar=0,nchars-1 do begin
        readf,coresp_lun,fontnum, charnum,hershey_num,format="(I3,I5,I12)"
        fontstruct[ichar].fontnum=fontnum
        fontstruct[ichar].charnum=charnum
        fontstruct[ichar].hershey_num=hershey_num
     end
  close,coresp_lun
  free_lun,coresp_lun

; open output
  max_font=40
  mask = '7fffffff'xl
  openw,output,table,/get_lun
  fonttab=lonarr(2,max_font)& fonttab-=1 ; set empty & write
  byteorder, fonttab, /HTONL	;To network order, long
  writeu, output, fonttab
  byteorder, fonttab, /NTOHL	;back to our order

; starting with empty fonts

  for ifont=3,20 do begin ; avoid 3 first fonts
     w=where(fontstruct.fontnum eq ifont, count) ; a font is defined in coresp

     if count gt 0 then begin   ; must write
        if count ne 256-32 and count ne 128-32 then message,"invalid number of glyphs in font #"+strtrim(ifont,2) 
        
        point_lun,output,0        ; rewind
        readu,output,fonttab
        byteorder, fonttab, /NTOHL ;back to our order
        stat=fstat(output)
        point_lun,output,stat.size ; go to end

        currentFontGlyphs=fontstruct[w]
        print,"writing font #",currentFontGlyphs[0].fontnum
        ctab = replicate({CTAB, nvecs: 0b, width: 0b, offset: 0}, count)

                                ; populate CTAB
        ; for each char get position in hershey_glyphs
        index=intarr(count)
        len = 0
        for j=0,count-1 do begin
           w=where(hershey_glyphs.charnum eq currentFontGlyphs[j].hershey_num, gcount)
           if gcount lt 1 then begin
              ctab[j].nvecs=0
              index[j]=0
              continue          ; not a valid font
           endif
           index[j]=w
           currentCharGlyph= hershey_glyphs[w]
           len += currentCharGlyph.nval/2-currentCharGlyph.npenup
           ctab[j].width=currentCharGlyph.right-currentCharGlyph.left
           ctab[j].nvecs=currentCharGlyph.nval/2-currentCharGlyph.npenup
        end

        vect=intarr(len)

       ; populate vect (shorts) and update ctab offset (relative position yet)
        start=0
        for j=0,count-1 do begin
           off=start; pairs
           byteorder,off, /HTONS			;To network order
           ctab[j].offset=off
           byteorder,off, /NTOHS			;To local
           nvecs=ctab[j].nvecs
           if nvecs gt 0 then begin
              ivec=0
              a=strsplit(hershey_glyphs[index[j]].string," R",/reg,/ext)
              n=n_elements(a)
              if plot then begin
                 plot,[0,1],xrange=[0,36],yrange=[0,36], /iso, /nodata,/xst,/yst
                 dx=indgen(36)& dy=replicate(1,36) & xx=dx##dy & yy=transpose(xx)& plots,xx,yy,psym=1,color='00FF00'x
                 oplot,[0,36],[4,4],color='ff'x
                 oplot,[4,4],[0,36],color='ff'x
              endif
              width=ctab[j].width
              right=hershey_glyphs[index[j]].right

              if plot then oplot,[4+width,4+width],[0,36],color='ff0000'x

              for isubstring=0,n-1 do begin
                 nchars=n_bytes(a[isubstring])
                 nshorts=nchars/2
                 ivec+=nshorts
                 pairness=indgen(nchars)
                 pen_up=intarr(nshorts) & pen_up[0]=1
                 k=fix(a[isubstring],type=1) & k=fix(k,type=2) & k-=rvals 
                 x=k[where(pairness mod 2 eq 0)]+right
                 y=10-k[where(pairness mod 2 eq 1)]
                 shorts=ishft(x and 127,7) + (y and 127) + 16384 * pen_up
                 vect[start:start+nshorts-1]=shorts
                 start+=nshorts
              endfor
              if ivec ne nvecs then stop,"problem"
              if plot then begin
                 getvects, vect[off:start-1], x, y, pen_up
                 ;		Draw each segment
                 nodraw = [where(pen_up, countpu), n_elements(pen_up)]
                 for i=0, countpu-1 do oplot,x[nodraw[i]:nodraw[i+1]-1], y[nodraw[i]:nodraw[i+1]-1]
              endif
           end              

           if plot then begin
              z=""
              read,z
              if z eq "q" or z eq "Q" then plot=0
           endif
           
        endfor
        point_lun,-output,pos                   ; memorize current pos
        fonttab[0,ifont]=pos and '0fffffff'xl	;Beginning of font
        ; len plus bit test
        fonttab[1,ifont] = len*2                ; length of vect as bytes
        if count eq 256-32 then fonttab[1,ifont] or= (not mask)
        byteorder, vect, /HTONS
        writeu, output, ctab, vect      ; write ctab and vect
        byteorder, vect, /NTOHS

        point_lun,output,0           ; rewind
        byteorder, fonttab, /HTONL ;To network order, long
        writeu, output, fonttab      ; update fonttab
        byteorder, fonttab, /NTOHL ;back to our order
     endif
  endfor
  free_lun,output
end
