;+
; NAME:
;  psfstack
; PURPOSE:
;  Generate an average numerical psf by stacking observed images.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
;
; CALLING SEQUENCE:
;  psfstack,image,x,y,psf
;
; INPUTS:
;  image - Input image from which psf is collected.
;  x     - Scalar or vector list of positions of stars (x coordinate).
;  y     - Scalar or vector list of positions of stars (y coordinate).
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  APODIZE - Flag, if set turns on an apodization constraint that forces the
;               PSF decrease to zero by the edge.  This is implemented by 
;               looking at the mean radial profile of the final PSF.  In
;               this profile, the routine looks for a local minimum.  From
;               that point to the edge (corners, really) the PSF is reduced
;               linearly from that radial distance to the corners so that
;               the edge goes to zero.  It is best to not use this but if
;               you have problems with the PSF turning up at the edge, this can
;               help make it better.
;  DW      - half-width of box centered around PSF, final size is 2*DW+1,
;             default size is DW=9 pixels.  It is really important that
;             DW be larger than FWHM.
;  SILENT  - Flag, if set suppresses all printed output.
;  SKY1    - Inner radius of the sky annulus (in pixels), default=10.
;  SKY2    - Outer radius of the sky annulus (in pixels), default=40.  The sky
;               value is computed for every source to get a local sky value.
;  SNRTHRESH - Signal-to-noise ratio threshold.  Any source pointed to by x,y
;                 that has a lower SNR than this will be ignored.  The default
;                 is 25.
;  TOPCLIP - Fraction of the brightest pixels that are excluded before doing
;               the initial median on the psf stack.  This should be a number
;               between 0 and 1, though if you set it to 1 everything would be
;               excluded.  The default is 0.
;
; OUTPUTS:
;  psf   - Final average psf, normalized to unit _volume_
;
; KEYWORD OUTPUT PARAMETERS:
;  FLUX  - Fraction of PSF volume interior to FWHM from center of PSF.
;  FWHM  - FWHM of final stacked PSF
;  XCEN  - Centroided X location of center of PSF within the psf array
;  YCEN  - Centroided Y location of center of PSF within the psf array
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODifICATION HISTORY:
;  97/10/22, Written by Marc W. Buie, Lowell Observatory
;  2013/03/30, MWB, added APODIZE keyword
;  2013/03/31, MWB, added SKY1, SKY2, TOPCLIP, FWHM, XCEN, YCEN, FLUX
;                      also some tweaks to the algorithm that leads to
;                      a significantly improved PSF.
;
;-
pro psfstack,image,x,y,psf, $
       DW=dw,SNRTHRESH=snrthresh,SILENT=silent,APODIZE=apodize, $
       SKY1=sky1,SKY2=sky2, $
       TOPCLIP=topclip, $
       FWHM=fwhm,XCEN=xcen,YCEN=ycen,FLUX=flux

   self='PSFSTACK: '
   if badpar(image,[2,3,4,5],2,caller=self+'(image) ') then return
   if badpar(x,[2,3,4,5],1,caller=self+'(x) ') then return
   if badpar(y,[2,3,4,5],1,caller=self+'(y) ') then return
   if badpar(dw,[0,2,3,4,5],0,caller=self+'(DW) ',default=9) then return
   if badpar(sky1,[0,2,3,4,5],0,caller=self+'(SKY1) ',default=10) then return
   if badpar(sky2,[0,2,3,4,5],0,caller=self+'(SKY1) ',default=40) then return
   if badpar(snrthresh,[0,2,3,4,5],0,caller=self+'(SNR) ', $
                                     default=25.0) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ',default=0) then return
   if badpar(apodize,[0,1,2,3],0,caller=self+'(APODIZE) ',default=0) then return
   if badpar(topclip,[0,4,5],0,caller=self+'(TOPCLIP) ',default=0.0) then return

   imsz = size(image)
   pad = 10
   sz = 2*dw+1
   psfcount = n_elements(x)
   psfcount0=psfcount
   ix = fix(x+0.5)
   iy = fix(y+0.5)
   xg = x
   yg = y

   ; Weed out objects too close to the edge.
   z = where( ix ge dw+pad and ix lt imsz[1]-dw-pad and $
              iy ge dw+pad and iy lt imsz[2]-dw-pad,count)
   if count ne 0 then begin
      ix = ix[z]
      iy = iy[z]
      xg = xg[z]
      yg = yg[z]
      psfcount=count
   endif
   edge=psfcount0-count

   ; First, get the sky and signal to noise ratio on each psf.
   snr = fltarr(psfcount)
   sky = fltarr(psfcount)
   skyerr = fltarr(psfcount)
   for i=0,psfcount-1 do begin

      ; find local sky level
      getannul,image,xg[i],yg[i],sky1,sky2,skybuf
      robomean,skybuf,3.0,0.5,skymean,stdmean=stdmean
      sky[i] = skymean
      skyerr[i] = stdmean

      ; get region around psf
      psf = image[ix[i]-dw:ix[i]+dw,iy[i]-dw:iy[i]+dw]

      psf = psf - skymean
      snr[i] = total(psf)/(sz^2*stdmean)

   endfor

   ; Weed out faint stars.
   z = where(snr gt snrthresh,count)
   if count ne 0 then begin
      snr = snr[z]
      sky = sky[z]
      skyerr = sky[z]
      ix = ix[z]
      iy = iy[z]
      xg = xg[z]
      yg = yg[z]
      psfcount=count
   endif
   faint=psfcount0-count

   ; Collect stack of good psf stars.
   psfstack=fltarr(sz,sz,psfcount)

   for i=0,psfcount-1 do begin

      ; extract slightly larger array than desired psf
      tmparr = image[ix[i]-dw-pad:ix[i]+dw+pad,iy[i]-dw-pad:iy[i]+dw+pad]

      ; Shift image to center star on center pixel
      dx = ix[i] - xg[i]
      dy = iy[i] - yg[i]
      arr = sshift2d(tmparr,[dx,dy])
      psf = arr[pad:pad+sz-1,pad:pad+sz-1]

      psfstack[*,*,i] = psf - sky[i]
      snr[i] = total(psfstack[*,*,i])/(sz^2*stdmean)
      psfstack[*,*,i] = psfstack[*,*,i] / max(psfstack[*,*,i])

   endfor

   ; Compute first stacked psf average
   avgclip,psfstack,psf,/silent,ntopclip=fix(psfcount*topclip)

   ; Compute the image residuals
   ttl   = fltarr(psfcount)
   for i=0,psfcount-1 do $
      ttl[i] = total((psfstack[*,*,i]-psf)^2)

   ; Filter out those with unusual residual levels.
   bad = intarr(psfcount)
   robomean,ttl,2.0,0.5,bad=bad
   z = where(bad eq 0, count)
   badresid=psfcount-count
   if count ne 0 then begin
      psfstack=psfstack[*,*,z]
      psfcount=count
   endif

   ; Recompute the stacked psf average
   avgclip,psfstack,psf,/silent,ntopclip=fix(psfcount*topclip)

   ; Compute the actual center (peak) of the PSF generated
   basphote,1.0,psf,1.0,dw+1,dw+1,dw-1,0.,-1., $
      fwhm=fwhm,xcen=xcen,ycen=ycen,/silent,/nolog
   if fwhm lt 1.6 then begin
      print,self,'WARNING! PSF fwhm is too small, fwhm=',fwhm
   endif
   basphote,1.0,psf,1.0,xcen,ycen,fwhm<(dw-1),0.,-1., $
      flux=flux,/exact,/silent,/nolog

   if apodize then begin
      xv   = findgen(sz)
      oney = replicate(1.0,sz)
      xarr = xv#oney-xcen
      yv   = findgen(sz)
      onex = replicate(1.0,sz)
      yarr = onex#yv-ycen
      rarr = sqrt(xarr^2+yarr^2)
      r = rarr[*]
      f = psf[*]
      idx=sort(r)
      r=r[idx]
      f=f[idx]
      lowess,r,f,3,fsmo,order=2
      z=where(f eq min(psf))
      if not silent then print,'min at ',r[z[0]],' corner at ',max(r)
      rtail=[r[z[0]],max(r)]
      tcorr=[1.0,0.0]
      coeff=poly_fit(rtail,tcorr,1)
      z=where(rarr ge rtail[0])
      psf[z]=psf[z]*poly(rarr[z],coeff)
      psf=psf/total(psf)
   endif

   psf = psf/total(psf)

   if not silent then begin
      ; Compute x and y FWHM for diagnostics.
      xsum=fltarr(sz)
      ysum=fltarr(sz)
      xwd=fltarr(psfcount)
      ywd=fltarr(psfcount)
      for i=0,psfcount-1 do begin
         xsum[*]=0.0
         ysum[*]=0.0
         for ii=0,sz-1 do xsum = xsum + psfstack[*,ii,i]
         for ii=0,sz-1 do ysum = ysum + psfstack[ii,*,i]
         xwd[i] = total(xsum)/max(xsum)
         ywd[i] = total(ysum)/max(ysum)
      endfor
      print,psfcount0,edge,faint,badresid,mean(xwd),mean(ywd),psfcount, $
        format='(i4," total, ",i4," edge, ",i4," faint, ",i4," bad, ",f4.1,' + $
               '" xFWHM, ",f4.1," yFWHM, ",i4," total")'
   endif

end
