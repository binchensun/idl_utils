;+
; NAME:
;  addpsf
; PURPOSE:   (one line only)
;  Insert (add) one or more PSFs into an image
; DESCRIPTION:
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  addpsf,image,x,y,f,psf
; INPUTS:
;  image - 2-d array to insert the PSF(s) into (MODIFIED).
;  x     - PSF x position (expected to be a float)
;  y     - PSF y position (expected to be a float)
;  f     - Scaling factor for source (multiplied against psf)
;  psf   - Normalized PSF to use
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  PSFMAX - 2-element vector that contains the x,y position of the
;              peak of the input PSF.  These coordinates are in the
;              native image position coordinates of the PSF.  If not
;              provided it will be computed.  You can save on this
;              calculation by taking care of this outside this routine.
; OUTPUTS:
;  image - modified version of the image with the PSFs added
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2012/09/28
;-
pro addpsf,image,x,y,f,psf,VERBOSE=verbose,PSFMAX=psfmax

   self='addpsf: '
   if badpar(image,[2,3,4,5],2,caller=self+'(image) ') then return
   if badpar(x,[2,3,4,5],[0,1],caller=self+'(x) ',npts=npts) then return
   if badpar(y,[2,3,4,5],[0,1],caller=self+'(y) ') then return
   if badpar(f,[2,3,4,5],[0,1],caller=self+'(f) ') then return
   if badpar(psf,[2,3,4,5],2,caller=self+'(psf) ') then return
   if badpar(psfmax,[0,2,3,4,5],1,caller=self+'(PSFMAX) ',default=[-1,-1]) then return
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ',default=0) then return

   szi=size(image,/dimen)
   szp=size(psf,/dimen)
   xmidp=szp[0]/2.0-0.5
   ymidp=szp[1]/2.0-0.5
   if min(psfmax) lt 0 then begin
      boxm,psf,szp[0]/2,szp[1]/2,szp[0]/2-1,szp[1]/2-1,xm,ym
      findmax,xm,ym,psf,xmax,ymax,fm
   endif else begin
      xmax=psfmax[0]
      ymax=psfmax[1]
   endelse

   if verbose then begin
      print,'psf pos ',xmax,ymax
      print,'psf size',szp[0],szp[1],' and middle',xmidp,ymidp
   endif

   for i=0,npts-1 do begin
      ; fractional pixel shift
      dx = x[i]-(xmax-xmidp)
      dy = y[i]-(ymax-ymidp)
      if verbose then begin
         print,'raw dx,dy',dx,dy
      endif

      dx = dx - fix(x[i])
      dy = dy - fix(y[i])
      spsf = sshift2d(psf,[dx,dy],/edge_zero)

      if verbose then begin
         print,'desired position ',x[i],y[i]
         print,'shift is ',dx,dy
         boxm,spsf,szp[0]/2,szp[1]/2,szp[0]/2-1,szp[1]/2-1,xmc,ymc
         findmax,xmc,ymc,spsf,xmaxc,ymaxc,fmc
         print,'computed psf pos',xmax+dx,ymax+dy
         print,'psf pos ',xmaxc,ymaxc
      endif

      ix=fix(x[i])
      iy=fix(y[i])

      ; image coordinates for inserted region
      ii0=ix-szp[0]/2 > 0
      ij0=iy-szp[1]/2 > 0
      pi0=szp[0]/2-ix > 0
      pj0=szp[1]/2-iy > 0

      if ix+szp[0]/2 ge szi[0] then $
         pi1=szp[0]-((szp[0]/2-(szi[0]-ix))+2) $
      else $
         pi1=szp[0]-1

      if iy+szp[1]/2 ge szi[1] then $ 
         pj1=szp[1]-((szp[1]/2-(szi[1]-iy))+2) $
      else $
         pj1=szp[1]-1

      if ix+szp[0]/2 ge szi[0] then $
         ii1=szi[0]-1 else $ 
         ii1=ix+szp[0]/2
      if iy+szp[1]/2 ge szi[1] then ij1=szi[1]-1 else $ 
         ij1=iy+szp[1]/2

      ;insert psf in desired locations in image and in blank array
      image[(ii0):ii1,(ij0):ij1] += spsf[(pi0):pi1,(pj0):pj1]*f[i]

   endfor

end
