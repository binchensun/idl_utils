;+
; NAME:
;  goodpoly
; PURPOSE: (one line)
;  Robust fitting of a polynomial to data.
; DESCRIPTION:
;  This is a multi-pass fitting routine that fits a fixed order polynomial
;  to the input data.  After each pass, the scatter of the fit relative
;  to the fitted line is computed.  Each point is examined to see if it
;  falls beyond THRESH sigma from the line.  If is does, it is removed
;  from the data and the fit is tried again.  This will make up to two
;  attempts to remove bad data.
; CATEGORY:
;  Function fitting
; CALLING SEQUENCE:
;  coeff = goodpoly(x,y,order,thresh,yfit,newx,newy,newerr)
; INPUTS:
;  x      - Input dataset, independant values.
;  y      - Input dataset, dependant values.
;  order  - Order of the polynomial fit (linear = 1).
;  thresh - Sigma threshold for removing outliers.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  BAD     - Vector of flags, same length as x and y, that indicate bad values
;              in the y vector.  The default is that all points are considered
;              good at the stars.  If supplied, any additional bad values found
;              will be marked as bad in this vector upon output.  Also, any
;              NaN values found in either the x or y vector will be trimmed
;              from the data (and marked bad) prior to any processing.
;  EXCLUDE - Number of points to exclude from initial pass fit.  This number
;              is rounded up to the next even number.  Then EXCLUDE/2 of the
;              highest and lowest points are removed before the first fit.
;              If these numbers are reasonable, they will not be excluded
;              in the second pass.  This helps prevent biasing the first fit
;              with the worst points in the array.  Default is to not exclude
;              any points on the first pass.
;  MAX_VALUE - The maximum value to be fitted.  If this keyword is provided,
;                data values greater than MAX_VALUE are treated as missing
;                and are not used in the fit at any pass.
;  MIN_VALUE - The minimum value to be fitted.  If this keyword is provided,
;                data values greater than MIN_VALUE are treated as missing
;                and are not used in the fit at any pass.
;  YERR      - Uncertainties on the y values.  Default=no weighting.
;                This option changes how goodpoly works.  By default, the
;                point-to-point scatter from the fit is used for the
;                threshold.  If these errors are provided, the threshold
;                is based on (observed-computed)/yerr.
; OUTPUTS:
;  yfit   - Fitted values for y that match the input vector.
;  newx   - X values from input that were considered good.
;  newy   - Y values from input that were considered good.
;  newerr - uncertainties from input that were considered good.
;  Return value is the set of polynomial coefficients.
; KEYWORD OUTPUT PARAMETERS:
;  COEFFSIG - Uncertainty on the final returned coefficients
;  SIGMA   - Standard deviation of difference between good points and the
;              fitted curve.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODifICATION HISTORY:
;  Written 1991 Feb., Marc W. Buie, Lowell Observatory
;  93/11/12, MWB, Program fixed to return a computed y for all input x.
;  95/09/20, MWB, Added EXCLUDE keyword.
;  98/02/09, MWB, Added SIGMA keyword.
;  98/06/08, MWB, Added MIN/MAX_VALUE keywords.
;  98/08/12, MWB, Revamped some logic plus added direct support for badflags.
;                    The new version is probably a tad faster and more robust
;                    than the old version.
; 2009/10/02, MWB, removed obsolete Poly_fit arguments
; 2010/04/29, MWB, added a trimrank call on the return value
; 2011/04/29, MWB, added COEFFSIG keyword
; 2013/07/19, MWB, added YERR keyword and newerr argument
;-
function goodpoly,x,y,order,thresh,yfit,newx,newy,newerr, $
            EXCLUDE=exclude,SIGMA=sigma,BAD=bad,YERR=yerr, $
            MIN_VALUE=min_value,MAX_VALUE=max_value,COEFFSig=coeffsig

   self='GOODPOLY: '
   ; The required parameters are not validated so that this runs faster
   if badpar(exclude,  [0,1,2,3],    0,CALLER=self+'(EXCLUDE) ', $
                                       DEFAULT=0) then return,0
   if badpar(min_value,[0,1,2,3,4,5],0,CALLER=self+'(MIN_VALUE) ', $
                                       type=minvtype) then return,0
   if badpar(max_value,[0,1,2,3,4,5],0,CALLER=self+'(MAX_VALUE) ', $
                                       type=maxvtype) then return,0
   if badpar(yerr,[0,2,3,4,5],1,CALLER=self+'(YERR) ', $
                                       type=yerrtype) then return,0

   if yerrtype ne 0 then weighted=1 else weighted=0

   savebad = n_elements(bad) eq n_elements(y)

   xx = x
   yy = y
   if weighted then ye = yerr else ye=replicate(1.0,n_elements(xx))
   arlen=n_elements(xx)
   ;print,'arlen at start',arlen,total(bad)

   ; Take note of all points that start out good for later indexing into the
   ;   input bad array.  Also weed out the points that are known bad at the
   ;   start.
   if savebad then begin
      z = where(bad eq 0,count)
      if count ne 0 and count ne arlen then begin
         xx = xx[z]
         yy = yy[z]
         ye = ye[z]
      endif else if count eq 0 then begin
         print,self,'Error! All values are marked bad, nothing to fit.'
         coeff=fltarr(order+1)
         coeff[0]=0.0
         return,coeff
      endif
      ; Save the indicies into the original array for all the good points.
      ;   This will be needed later if any points are marked bad.  This array
      ;   should maintain the same length as the xx,yy arrays but will point
      ;   back into the original arrays, x,y,bad
      goodidx = z
   endif
   arlen=n_elements(xx)

   ; Filter out any NaNs
   z = where(finite(xx) eq 0 or finite(yy) eq 0,count)
   if count ne 0 and count ne arlen then begin
      if savebad then bad[goodidx[z]]=1
      z = where(finite(xx) eq 1 and finite(yy) eq 1)
      xx = xx[z]
      yy = yy[z]
      ye = ye[z]
      if savebad then goodidx = goodidx[z]
   endif else if count eq arlen then begin
      print,self, $
         'Error! All values are either NaNs or marked bad, nothing to fit.'
      coeff=fltarr(order+1)
      coeff[0]=0.0
      return,coeff
   endif
   arlen=n_elements(xx)

   ; Need an inverted bad flag array for further processing.
   if savebad then gflag = 1B-bad

   ; If min_value provide, exclude those values now
   if minvtype ne 0 then begin
      z = where(yy gt min_value,count)
      if count ne 0 and count ne arlen then begin
         xx = xx[z]
         yy = yy[z]
         ye = ye[z]
         if savebad then begin
            gflag[goodidx[z]]=gflag[goodidx[z]]*2B
            gflag = gflag/2B
            goodidx = goodidx[z]
         endif
      endif else if count eq 0 then begin
         print,self, $
            'Error! All y values are below min_value, nothing to fit.'
         coeff=fltarr(order+1)
         coeff[0]=min_value
         return,coeff
      endif
   endif
   arlen=n_elements(xx)

   ; If max_value provide, exclude those values now
   if maxvtype ne 0 then begin
      z = where(yy lt max_value,count)
      if count ne 0 and count ne arlen then begin
         xx = xx[z]
         yy = yy[z]
         ye = ye[z]
         if savebad then begin
            gflag[goodidx[z]]=gflag[goodidx[z]]*2B
            gflag = gflag/2B
            goodidx = goodidx[z]
         endif
      endif else if count eq 0 then begin
         print,self, $
            'Error! All y values are above max_value, nothing to fit.'
         coeff=fltarr(order+1)
         coeff[0]=max_value
         return,coeff
      endif
   endif
   arlen=n_elements(xx)

   ; Initial fit with all the data.
   if exclude eq 0 or arlen le (exclude+order) then begin
      if arlen le order then begin
         coeff = fltarr(order+1)
         if arlen ne 1 then begin
            sigma = stdev(yy,mean)
            coeff[0] = mean
         endif else begin
            coeff[0] = yy[0]
            sigma = yy[0]
            if sigma eq 0.0 then sigma = 1.0
         endelse
         yfit = replicate(coeff[0],n_elements(x))
         newx = xx
         newy = yy
         print,self, $
            'not enough data to support even a non-robust polynomial fit.'
         return,coeff
      endif else begin
         if weighted then $
            coeff=poly_fit(xx,yy,order,yfit=yfit,sigma=coeffsig, $
                           measure_errors=ye) $
         else $
            coeff=poly_fit(xx,yy,order,yfit=yfit,sigma=coeffsig)
;         chisq0=total((yy-yfit)^2)/(arlen-order)  
         flat = (yy-yfit)+(total(yfit)/arlen)
         sigma = stdev(flat,mean)
      endelse
 
   ; Initial fit excluding EXCLUDE extrema points before fit.
   endif else begin
      nex = ceil(exclude/2.0)
      sidx=sort(yy)
      sidx=sidx[nex:arlen-1-nex]
      if weighted then $
         coeff=poly_fit(xx[sidx],yy[sidx],order,sigma=coeffsig, $
                        measure_errors=ye[sidx]) $
      else $
         coeff=poly_fit(xx[sidx],yy[sidx],order,sigma=coeffsig)
      yfit = poly(xx,coeff)
;      chisq0=total((yy-yfit)^2)/(arlen-order)  
      flat = (yy-yfit)+(total(yfit)/arlen)
      sigma = stdev(flat[sidx],mean)
   endelse

   ;Remove all points beyond threshold sigma
   if weighted then $
      good=where( abs(yfit-yy)/ye lt thresh,goodnum) $
   else $
      good=where( abs(flat-mean) lt thresh*sigma,goodnum)
   nbad = arlen-goodnum 
   if goodnum ne 0 and goodnum ne arlen then begin
      xx=xx[good]
      yy=yy[good]
      ye=ye[good]
      if savebad then begin
         gflag[goodidx[good]]=gflag[goodidx[good]]*2B
         gflag = gflag/2B
         goodidx = goodidx[good]
      endif
      arlen=goodnum
   endif
 
   if nbad ne 0 then begin

      ; Second pass fit with bad points removed (if needed).
      if weighted then $
         coeff=poly_fit(xx,yy,order,yfit=yfit,sigma=coeffsig, $
                        measure_errors=ye) $
      else $
         coeff=poly_fit(xx,yy,order,yfit=yfit,sigma=coeffsig)
;      chisq1=total((yy-yfit)^2)/(arlen-order)  
      flat = (yy-yfit)+(total(yfit)/arlen) 
      sigma = stdev(flat,mean)
    
      ;Remove all points beyond threshold sigma
      if weighted then $
         good=where( abs(yfit-yy)/ye lt thresh,goodnum) $
      else $
         good=where( abs(flat-mean) lt thresh*sigma,goodnum)
      nbad = arlen-goodnum 

      if goodnum ne 0 and goodnum ne arlen then begin
         xx=xx[good]
         yy=yy[good]
         ye=ye[good]
         if savebad then begin
            gflag[goodidx[good]]=gflag[goodidx[good]]*2B
            gflag = gflag/2B
            goodidx = goodidx[good]
         endif
         arlen=goodnum
      endif
   endif

   ; Third pass fit with bad points removed.
   if nbad ne 0 then begin
      if weighted then $
         coeff=poly_fit(xx,yy,order,yfit=yfit,sigma=coeffsig, $
                        measure_errors=ye) $
      else $
         coeff=poly_fit(xx,yy,order,yfit=yfit,sigma=coeffsig)
;      chisq2=total((yy-yfit)^2)/(arlen-order)
      flat = (yy-yfit)+(total(yfit)/arlen)
      sigma = stdev(flat,mean)
   endif

   newx=xx
   newy=yy
   newerr=ye

   yfit = poly(x,coeff)
   if savebad then bad = 1B - gflag

   return,trimrank(coeff)

end
