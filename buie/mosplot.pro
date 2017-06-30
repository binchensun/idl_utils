;+
; NAME:
;  mosplot
; PURPOSE:
;  Plot Mosaic astrometric solution for DES data and do astrometry.
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  mosplot,root,first,last,ext
; INPUTS:
;  root - root of data area and file names
;  first - number of first image to plot
;  last  - number of last image to plot
;  ext   - (single) extension number to work
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SKIP - If set, quietly skips frames with no valid solution.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2001/08/28
;  2001/10/09, MWB, added KPNO keyword flag
;  2002/04/16, MWB, changed the command option structure and added help
;  2002/04/25, MWB, changed the default option operation to be adaptive
;  2002/08/11, MWB, added SKIP keyword flag
;  2008/10/31, MWB, changed to use rdref
;  2013/03/25, MWB, changed to support longer suffix values
;-
pro mosplot,root,first,last,ext,SKIP=skip,BINFAC=binfac,BINDISP=bindisp

   self='MOSPLOT '
   if badpar(root,7,0,caller=self+'(root) ') then return
   if badpar(first,[2,3],0,caller=self+'(first) ') then return
   if badpar(last,[2,3],0,caller=self+'(last) ') then return
   if badpar(ext,[2,3],0,caller=self+'(ext) ') then return
   if badpar(skip,[0,1,2,3],0,caller=self+'(SKIP) ',default=0) then return
   if badpar(binfac,[0,1,2,3],0,caller=self+'(BINFAC) ',default=1) then return
   if badpar(bindisp,[0,1,2,3],0,caller=self+'(BINFAC) ',default=2) then return

   rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,prot,renormfac,cra,cdec,photzp, $
      terms,coeffarr,ncoeffs,nlines

   ; Setup the file name format string
   digits = fix(ceil(alog10(max(last)+1))) > 3
   dig = strn(digits)
   fnfmt = '(i'+dig+'.'+dig+')'

   lastans='a'
   defaultans=''

   nx=float(2048/binfac)
   ny=float(4096/binfac)

   for num=first,last do begin

checkit:
      imnameroot = root+'.'+strn(num,format=fnfmt)

      npts=10
      lside = findgen(npts+1)/npts * ny
      sside = findgen(npts+1)/npts * nx
      strut = fltarr(npts+1)

      xpos=[strut,sside,   strut+nx,  reverse(sside)]
      ypos=[lside,strut+ny,reverse(lside),     strut]

      a_xi  = dblarr(n_elements(xpos),8)
      a_eta = dblarr(n_elements(xpos),8)
      a_ra  = dblarr(n_elements(xpos),8)
      a_dec = dblarr(n_elements(xpos),8)
      a_ra[*] = -1.0
      valid = intarr(8)

      for i=1,8 do begin
         exttag = 'x'+strn(i)
         imname = imnameroot+exttag

         z = where(imname eq ffn,count)

         if count ne 0 then begin

            valid[i-1] = 1
            ; Get the index to the coefficients
            if ftype[z[0]] eq 'eta' then begin
               floce = z[0]
               flocx = z[1]
            endif else begin
               floce = z[1]
               flocx = z[0]
            endelse

            ; Extract the coefficients
            cxi  = trimrank(coeffarr[flocx,0:ncoeffs-1])
            ceta = trimrank(coeffarr[floce,0:ncoeffs-1])
            cprot= trimrank(prot[flocx])

            info = { $
               renormfac: renormfac[flocx], $
               cxi: cxi, $
               ceta: ceta, $
               prot: cprot, $
               terms: terms, $
               xcref: xc[flocx], $
               ycref: yc[flocx], $
               raref: cra[flocx], $
               decref: cdec[flocx] $
               }

            astcvt,'xy',xpos,ypos,info,'SN',xi,eta
            a_xi[*,i-1]  = xi
            a_eta[*,i-1] = eta
            astcvt,'xy',xpos,ypos,info,'rd',ra,dec
            a_ra[*,i-1]  = ra
            a_dec[*,i-1] = dec

            if i eq ext then thisinfo=info

         endif
      endfor

      z=where(valid eq 1,count)

      if count eq 0 then begin
         if not skip then print,'no valid solutions ',imnameroot
         found=1   ; this is logically strange but it works.
         goodref=2
      endif else begin

         xirange=minmax(a_xi[*,z])
         etarange=minmax(a_eta[*,z])

         reffile = 'Refstars/'+string(num,format=fnfmt)+'x'+strn(ext)+'.ref'
         if exists(reffile) then begin
            goodref=1
            rdref,reffile,ref,referr
            if referr ne 0 then goodref=0
            if goodref then begin
               astcvt,'rd',ref.ra,ref.dec,thisinfo,'SN',rxi,reta
            endif
            defaultans=''
         endif else begin
            defaultans=lastans
            goodref=0
         endelse

         setwin,1
         xr=[1200,-1200]
         yr=[-1200,1200]
         xtitle='(E)  xi  (W)'
         ytitle='(S) eta  (N)'
         plot,[0],[1],/nodata,xr=xr,yr=yr,/iso,title=imnameroot, $
            ytitle=ytitle,xtitle=xtitle
         if (goodref) then $
            oplot,rxi,reta,color='00ffff'xl,psym=3
         found=0
         for i=0,count-1 do begin
            txi=a_xi[*,z[i]]
            teta=a_eta[*,z[i]]
            if ext eq z[i]+1 then begin
               oplot,txi,teta,color='0000ff'xl
               xyouts,mean(minmax(txi)),mean(minmax(teta)),strn(z[i]+1), $
                  color='0000ff'xl
               found=1
            endif else begin
               oplot,txi,teta
               xyouts,mean(minmax(txi)),mean(minmax(teta)),strn(z[i]+1)
            endelse
            oplot,[txi[0]],[teta[0]],psym=8,color='ff00ff'xl
         endfor

         setwin,2
         z2=where(a_ra ge 0.)
         xr=maxmin(a_ra[z2])
         yr=minmax(a_dec[z2])
         xtitle='(E)  RA  (W)'
         ytitle='(S)  Dec  (N)'
         plot,[0],[1],/nodata,xr=xr,yr=yr,title=imnameroot, $
            ytitle=ytitle,xtitle=xtitle
         if (goodref) then $
            oplot,ref.ra,ref.dec,color='00ffff'xl,psym=3
         for i=0,count-1 do begin
            tra=a_ra[*,z[i]]
            tdec=a_dec[*,z[i]]
            if ext eq z[i]+1 then begin
               oplot,tra,tdec,color='0000ff'xl
               xyouts,mean(minmax(tra)),mean(minmax(tdec)),strn(z[i]+1), $
                  color='0000ff'xl
            endif else begin
               oplot,tra,tdec
               xyouts,mean(minmax(tra)),mean(minmax(tdec)),strn(z[i]+1)
            endelse
            oplot,[tra[0]],[tdec[0]],psym=8,color='ff00ff'xl
         endfor

      endelse

;      if found then begin
         a=''
;print,'[',defaultans,']   [',lastans,']'
         prompt=strn(num,format=fnfmt)
         if defaultans ne '' then prompt=prompt+':'+defaultans
         prompt=prompt+'>'
         if not skip or goodref ne 2 then begin
            read,a,format='(a)',prompt=prompt
            a=strcompress(a,/remove_all)
         endif else begin
            a=''
         endelse
         if a eq '' then a=defaultans
;      endif else begin
;         a='y'
;      endelse

      if a eq 't' then begin
         mosast,num,/twostar,ext=ext,/killref,binfac=bindisp
         rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,prot,renormfac,cra,cdec, $
            photzp,terms,coeffarr,ncoeffs,nlines
         lastans=a
         goto,checkit
      endif else if a eq 'T' then begin
         mosast,num,/twostar,ext=ext,/killref,/edit,binfac=bindisp
         rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,prot,renormfac,cra,cdec, $
            photzp,terms,coeffarr,ncoeffs,nlines
         lastans=a
         goto,checkit
      endif else if a eq 'm' then begin
         mosast,num,/twostar,ext=ext,/killref,/ignoresrc,binfac=bindisp
         rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,prot,renormfac,cra,cdec, $
            photzp,terms,coeffarr,ncoeffs,nlines
         lastans=a
         goto,checkit
      endif else if a eq 'M' then begin
         mosast,num,/twostar,ext=ext,/killref,/ignoresrc,/edit,binfac=bindisp
         rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,prot,renormfac,cra,cdec, $
            photzp,terms,coeffarr,ncoeffs,nlines
         lastans=a
         goto,checkit
      endif else if a eq 'a' then begin
         mosast,num,ext=ext,/killref,binfac=bindisp
         rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,prot,renormfac,cra,cdec, $
            photzp,terms,coeffarr,ncoeffs,nlines
         lastans=a
         goto,checkit
      endif else if a eq 'A' then begin
         mosast,num,ext=ext,/killref,/edit,binfac=bindisp
         rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,prot,renormfac,cra,cdec, $
            photzp,terms,coeffarr,ncoeffs,nlines
         lastans=a
         goto,checkit
      endif else if a eq 'v' then begin
         mosast,num,ext=ext,binfac=bindisp,skipgood=0
         rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,prot,renormfac,cra,cdec, $
            photzp,terms,coeffarr,ncoeffs,nlines
         goto,checkit
      endif else if a eq 'n' then begin
         ; NOP
      endif else if a eq '?' then begin
         print,''
         print,'Available options: '
         print,'t - Twostar reduction using source list (if available)'
         print,'a - Automatic reduction (requires source list)'
         print,'m - Twostar reduction (full manual, ignores source list)'
         print,''
         print,'v - Displays image and shows previous solution.'
         print,'<cr> or n - Go on to the next image'
         print,'q - Quit program now'
         print,''
         print,'Upper case option turns on /EDIT for astrometry'
         print,''
         goto,checkit
      endif else if a eq 'q' then begin
         return
      endif else if a ne '' then begin
         print,'***Unrecognized option!***'
         print,''
         print,'Here are the valid options: '
         print,'t - Twostar reduction using source list (if available)'
         print,'a - Automatic reduction (requires source list)'
         print,'m - Twostar reduction (full manual, ignores source list)'
         print,''
         print,'v - Displays image and shows previous solution.'
         print,'<cr> - Go on to the next image'
         print,'q - Quit program now'
         print,''
         print,'Upper case option turns on /EDIT for astrometry'
         print,''
         goto,checkit
      endif

   endfor

end
