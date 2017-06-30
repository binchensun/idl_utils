pro starfit,image,xguess,yguess,xc,yc,FWHM1,INFO=INFO
;+
;NAME:
;      STARFIT
;PURPOSE:
;      Routine to fit a 2D gaussian to a stellar object
;CALLING SEQUENCE:
;      STARFIT,image,xguess,yguess,xc,yc,FWHM,INFO=INFO
;INPUTS:
;      IMAGE  - Input image
;      XGUESS - Estimated initial X position of star (scalar!)
;      YGUESS - Estimated initial Y position of star (scalar!)
;OUTPUTS:
;      XC     - Fit X position of star
;      YC     - Fit Y position of star
;      FWHM   - Returned FWHM of star
;PROCEDURE:
;      The IDL library routine for fitting a Gaussian to a distribution
;      is used to fit to the marginal distribution.
;MODIFICATION HISTORY
;      Modified from XSPAM to generic use by EWDeutsch Nov93
;-

  if (n_params(0) lt 5) then begin
    print,'Call> starfit,image,xguess,yguess,xc,yc,FWHM,INFO=info'
    print,'e.g.> starfit,img,434,212,xc,yc,FWHM1'
    return
    endif

  xg=fix(xguess+.5)-7 & yg=fix(yguess+.5)-7
  array=double(extrac(image,xg,yg,15,15))

  array_param=size(array)
  xlen=array_param(1)
  ylen=array_param(2)
  sumx=fltarr(xlen)
  sumy=fltarr(ylen)
  x=indgen(xlen)
  y=indgen(ylen)

; Compute sums
  for k=0,xlen-1 do sumx(k)=total(array(k,*))/float(ylen)
  for k=0,ylen-1 do sumy(k)=total(array(*,k))/float(xlen)

; Fit gaussians
  xfit=gaussfit(x,sumx,a)
  yfit=gaussfit(y,sumy,b)
  xc=a(1)+xg
  yc=b(1)+yg

  INFO=[a,b]

  FWHM1=avg([info(2),info(6+2)])*2.35

  return
end
