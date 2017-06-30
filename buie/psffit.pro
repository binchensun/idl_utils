;+
; NAME:
;  psffit
; PURPOSE:   (one line only)
;  Fit a numerical PSF to one or more sources in an image.
; DESCRIPTION:
;  The PSF must be provided as an input to this routine.  Suitable
;    PSFs can be generated with PSFSTACK.  For this routine to work, the
;    resulting PSF must be amenable to shifting by interpolation.  Thus
;    an under-sampled PSF will not work with this tool.
;  Generally speaking, if you ask to fit more than one source at a time,
;    the sources should have overlapping PSFs.  Otherwise, you are better
;    off with separate calls.  This is NOT enforced.
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  psffit,image,psf,region,xin,yin,xout,yout,counts
; INPUTS:
;  image  - Input image to be fitted.
;  psf    - Numerical PSF to fit to image.  This must be normalized to have
;              unit volume.
;  region - Fitting region for chi-sq calculation.  Provide
;            [x1,x2,y1,y2] in image index coordinates.
;  xin    - Starting x location for source(s)
;  yin    - Starting y location for source(s)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SILENT - Flag, if set will suppress all diagnostic output
;  PSFMAX - Two element vector containing the x,y position of the peak of
;             the input PSF.  The position is in the native array coordinates
;             of the psf array.  If not provided, this will be computed.
;             It will save time for many fits with the same psf to provide
;             this value.  If computed internally, the values will be returned
;             to this keyword.
;  MEANSKY - Background sky value (of entire image).  If this is provided
;             and calcsky is not set then this value is used for sky.  If
;             calcsky is set then sky is computed and returned in this variable.
;  CALCSKY - Flag, if set forces this routine to compute sky background.
;             By using this flag you get a sky value that is global for the
;             entire image based on up to 60000 randomly selected pixels.
;             This flag is ignored if you use SKY1 and SKY2.
;  GAIN   - Gain of the image, e-/ADU.  This is used to generate an array
;             of uncertainties for each pixel based on photon statistics.
;             default=1.0
;  RDNOISE - Readout noise of CCD (default=10)  [e-]
;  NMAX    - Maximum number of iterations for the amoeba call.  Default=5000
;             If this limit is reached, the source will be tagged as
;             not fittable, ie., chisq = -1.
;  EXPTIME - Exposure time, in seconds, default=1
;  SKY1  - Inner radius of the sky annulus (unit=pixels), default=0
;  SKY2  - Outer radius of the sky annulus, default=0
;
;  PSFFACTOR- Area of constraint factor relative to the size of the PSF.
;                Pixels within the distance FWHMPSF*PSFFACTOR of the starting
;                location of the source(s) are used to guide the fit.
;                The default value is 1.0 and seems to work pretty well.
;                This value cannot be arbitrarly large or small.  In general
;                it needs to be between 1.0 and 2.0.  If it is too small it
;                may give bad answers.  If it is too big the program will
;                likely crash.
;
;  CSTART - optional vector, length must match xin,yin.  This gives starting
;              values for the source flux that override the automatic values
;              computed in this routine.  Any value that is greater than 0 is
;              taken to be the starting value to use.  For those that are
;              not specified (ie., <=0), the ones that are known are computed
;              and subtracted prior to computing the automatic starting
;              flux.
;
; OUTPUTS:
;  xout   - Fitted x location for source(s)
;  yout   - Fitted y location for source(s)
;  counts - Object flux (photons per second)
; KEYWORD OUTPUT PARAMETERS:
;  CHISQ  - final goodness of fit for the returned values
;              If this value is negative, the return values are meaningless.
;  FLERR  - Uncertainty (1 sigma) of object flux.
;  FLUX   - Object flux (photons per second)
;  MAG    - Optional return of the instrumental magnitude
;  ERR    - Optional return of the magnitude error
;  FWHM   - FWHM from basphote prior to fitting
;  PSF_FWHM - FWHM of PSF from basphote, same parameters as FWHM for calculation
; COMMON BLOCKS:
;  PSFFIT_COM, for internal use only
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2012/09/28
;  2012/10/04, MWB, added weighted fitting.
;  2012/12/06, MWB, fixed a problem with the MEANSKY and CALCSKY keywords
;  2013/03/05, MWB, added NMAX keyword and proper trap of bad fit from amoeba
;  2013/03/29, BLE, Added new input keywords EXPTIME, SKY1, and SKY2.
;                   Calculate and output new keywords FLUX, FLERR, MAG, ERR.
;                   The PSFTHRES input keyword was also added.
;  2013/04/14, MWB, tweak sigma array to avoid negative flux values
;  2013/06/19, MWB, removed PSFTHRES keyword.  Added PSFFACTOR plus numerous
;                     improvements including better starting values.
;                     CSTART keyword was added.
;  2013/07/12, MWB, added FWHM and PSF_FWHM output keywords
;-
function psffit_func,vals
   common psffit_com,info

   nvals=n_elements(vals)
   nsrc = nvals/3
   x=vals[0:nsrc-1]
   y=vals[nsrc:2*nsrc-1]
   f=abs(vals[2*nsrc:3*nsrc-1])

   model=fltarr(info.nx,info.ny)
   addpsf,model,x,y,f,info.psf,psfmax=[info.xm,info.ym]

   dimage=info.image[info.x0:info.x1,info.y0:info.y1] $
             -model[info.x0:info.x1,info.y0:info.y1] $
             -info.sky

;   no weighting
;   chisq = total(dimage^2) / info.npix
;   full area
;   chisq = total((dimage/sigarr)^2) / info.npix

   sigarr=info.simage[info.x0:info.x1,info.y0:info.y1]
   chisq = total((dimage[info.zm]/sigarr[info.zm])^2) / float(info.mpix)

   if chisq lt info.chibest then begin
      if not info.silent then print,info.chicount,chisq,x[0],y[0],f[0]
      info.chibest = chisq
   endif

   info.chicount++
   return,chisq

end

pro psffit,image,psf,region,xin,yin,xout,yout,counts, $
       SKY1=sky1,SKY2=sky2,EXPTIME=exptime,SKYERR=skyerr, $
       SILENT=silent,PSFMAX=psfmax,MEANSKY=meansky,CALCSKY=calcsky, $
       CHISQ=chisq,MAG=mag,ERR=err,GAIN=gain,RDNOISE=rdnoise,NMAX=nmax, $
       FLUX=flux,FLERR=flerr,PSFFACTOR=psffactor,CSTART=cstart, $
       FWHM=fwhm,PSF_FWHM=fwhmpsf

   common psffit_com,info

   self='psffit: '
   if badpar(image,[2,3,4,5],2,caller=self+'(image) ') then return
   if badpar(psf,[2,3,4,5],2,caller=self+'(psf) ') then return
   if badpar(region,[2,3],1,caller=self+'(region) ') then return
   if badpar(xin,[2,3,4,5],1,caller=self+'(xin) ',npts=npts) then return
   if badpar(yin,[2,3,4,5],1,caller=self+'(yin) ') then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ',default=0) then return
   if badpar(psfmax,[0,2,3,4,5],1,caller=self+'(PSFMAX) ', $
                              default=[-1,-1]) then return
   if badpar(calcsky,[0,2,3,4,5],0,caller=self+'(CALCSKY) ', $
                                   default=0) then return
   if badpar(gain,[0,2,3,4,5],0,caller=self+'(GAIN) ',default=1.) then return
   if badpar(rdnoise,[0,2,3,4,5],0,caller=self+'(RDNOISE) ', $
                                   default=10.) then return
   if badpar(nmax,[0,2,3],0,caller=self+'(NMAX) ',default=5000) then return
   if badpar(exptime,[0,2,3,4,5],0,caller=self+'(EXPTIME) ', $
             default=1.0) then return
   if badpar(sky1,[0,2,3],0,caller=self+'(SKY1) ',default=0) then return
   if badpar(sky2,[0,2,3],0,caller=self+'(SKY2) ',default=0) then return
   if badpar(psffactor,[0,2,3,4,5],0,caller=self+'(PSFFACTOR) ', $
                                default=1.0) then return
   if badpar(cstart,[0,2,3,4,5],1,caller=self+'(CSTART) ', $
                                default=replicate(0.,npts)) then return

   sz=size(image,/dimen)
   psfdim=size(psf,/dimen)
   if min(psfmax) lt 0 then begin
      boxm,psf,psfdim[0]/2,psfdim[1]/2,psfdim[0]/2-1,psfdim[1]/2-1,xm,ym
      findmax,xm,ym,psf,xmax,ymax,fm
      psfmax=[xmax,ymax]
   endif else begin
      xmax=psfmax[0]
      ymax=psfmax[1]
   endelse

   ;----if sky1 and sky2 provided (annulus radii), use them for sky calc
   if sky2 lt 0 then begin
      meansky = sky1
      skyerr  = abs(sky2)
   ; positive values define an annulus for local sky
   endif else if sky1 gt 0 and sky2 gt 0 then begin
      Getannul,image,mean(minmax(xin)),mean(minmax(yin)),sky1,sky2,skybuf
      Robomean,skybuf,3.0,0.5,meansky,avgdev,tmp_skysig, $
         var,skew,kurt,nsky,stdmean=skyerr
   endif else begin
      ;----if calcsky provided, do whole-sky calculation
      if calcsky then begin
         skysclim,image,lowval,hival,meansky,sigma, $
            stdmean=skyerr,lowclip=0.1,hiclip=0.9,npts=60000
      endif else begin
         ;----no sky calc. user must have provided sky mean/err values.
         if badpar(meansky,[2,3,4,5],0,caller=self+'(MEANSKY) ') then return
         if badpar(skyerr,[2,3,4,5],0,caller=self+'(SKYERR) ') then return
      endelse
   endelse

   simage=sqrt((image>0)*gain+rdnoise^2)/gain

   npix = (region[1]-region[0]+1) * (region[3]-region[2]+1)

   x0=region[0]
   x1=region[1]
   y0=region[2]
   y1=region[3]

   ; compute the FWHM of the psf (if needed)
   basphote,1.0,psf,1.0,psfmax[0],psfmax[1],mean(psfmax)-1,0,-0.00001, $
      /nolog,/silent,/exact,fwhm=fwhmpsf
   if fwhmpsf lt 1.6 then begin
      fwhmpsf=max(psfdim)/4
      print,self,'Warning! FWHM of psf came out negative.  Using',fwhmpsf
   endif

   if x0 ge 0 and x0 lt sz[0] and x1 ge 0 and x1 lt sz[0] and $
      y0 ge 0 and y0 lt sz[1] and y1 ge 0 and y1 lt sz[1] then begin

      objrad=fwhmpsf*psffactor
      if not silent then $
         print,'FWHM of PSF',fwhmpsf,' fac',psffactor,' objrad',objrad
      ; compute the chisq mask
      cmask=bytarr(x1-x0+1,y1-y0+1)
      for i=0,npts-1 do begin
         getannul,cmask,xin[i]-x0,yin[i]-y0,0.,objrad,dummy,idx
         cmask[idx]=1B
      endfor
      zm=where(cmask eq 1,countm)
      if countm eq 0 then begin
         print,'FATAL error in psffit, fitting mask is empty'
         return
      endif

      info= { image: image, $
              simage: simage, $
              nx:    sz[0], $
              ny:    sz[1], $
              psf:   psf, $
              xm:    xmax, $
              ym:    ymax, $
              x0:    x0, $
              x1:    x1, $
              y0:    y0, $
              y1:    y1, $
              sky:   meansky, $
              silent: silent, $
              cmask: cmask, $ ; chisq mask region
              zm:    zm, $ ; index array to get chisq area
              chicount: 0L, $
              chibest: 1.0e29, $
              mpix:  countm, $
              npix:  npix $
              }

      ; get psf information
      basphote,1.0,psf,1.0,psfmax[0],psfmax[1],fwhmpsf,0,-0.00001, $
         /nolog,/silent,/exact,flux=psfflux,fwhm=fwhm

      ; handle case where we get a useful starting flux from the outside
      zg=where(cstart gt 0.,countg)
      zb=where(cstart le 0.,countb)
      if countb eq 0 then begin
         basphote,1.0,image,1.0,xin,yin,fwhmpsf,meansky,-skyerr, $
            /nolog,/silent,/exact,flux=smapflux,fwhm=fwhm
         count0=cstart
      endif else if countg ne 0 then begin
         count0=cstart
         model=fltarr(info.nx,info.ny)
         addpsf,model,xin[zg],yin[zg],cstart[zg],info.psf, $
                psfmax=[info.xm,info.ym]
         basphote,1.0,image-model,1.0,xin[zb],yin[zb],fwhmpsf,meansky,-skyerr, $
            /nolog,/silent,/exact,flux=smapflux,fwhm=fwhm
         count0[zb]=smapflux/psfflux
      endif else begin
         basphote,1.0,image,1.0,xin,yin,fwhmpsf,meansky,-skyerr, $
            /nolog,/silent,/exact,flux=smapflux,fwhm=fwhm
         count0=smapflux/psfflux
      endelse

      if not silent then begin
         print,'pixel values for input positions'
         print,image[round(xin),round(yin)]
         print,'peak of psf position ',psfmax
         print,'psf: max',max(psf),' total',total(psf), $
               ' objrad flux',psfflux,' fwhm',fwhm
         print,'start count',count0
      endif
      start=[xin,yin,count0]
      if not silent then print,'start ',start
      scale=[replicate(0.1,npts*2),count0*0.01]
      if not silent then print,'scale ',scale
      ftol=1.0e-4
      fitval=amoeba(ftol,FUNCTION_NAME='psffit_func', $
                         P0=start, SCALE=scale, NMAX=nmax)

      if n_elements(fitval) gt 1 then begin
         chisq=psffit_func(fitval)
         xout=trimrank(fitval[0:npts-1])
         yout=trimrank(fitval[npts:2*npts-1])
         counts=trimrank(fitval[2*npts:3*npts-1])
      endif else begin
         xout=xin
         yout=yin
         counts=fltarr(npts)
         chisq=-1.0
      endelse

      area = !pi*objrad^2

   endif else begin
      xout=xin
      yout=yin
      counts=fltarr(npts)
      chisq=-1.0
      fwhm=replicate(-1,npts)
      area=!pi ; placeholder
   endelse

   ;----calculate flux error
   photons=counts*gain
   skyphot=meansky*gain
   sigphotsky=area*skyerr*gain
   varphotobj=photons+sigphotsky^2
   photerr=sqrt((varphotobj>0) + sigphotsky^2 + area*rdnoise^2)
   flux=photons/exptime
   flerr=photerr/exptime

   ;----calculate i-mag and error
   mag = replicate(99.999,npts)
   err = replicate(0.0,npts)
   z=where(photons gt 0,count)
   if count gt 0 then begin
      mag[z] = -2.5 * alog10(flux[z]) + 24.0
      err[z] = 1.085736205 * photerr[z] / photons[z]
   endif

   info=0

end
