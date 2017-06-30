;+
; NAME:
;    sumann
; PURPOSE: (one line)
;    Integrate over an annulus.
; DESCRIPTION:
;
;    This procedure computes the first two image moments of the pixels
;    contained within the input annulus.  The position and inner and outer
;    radii define the annulus.  Each pixel in the input image is then
;    assigned a weighting value which is its areal overlap between the pixel
;    and the annulus.  This weight varies from 0 to 1 and is the precise
;    analytic area of overlap (computed by pixwt.pro).  Of course, this
;    program doesn't do the computation for all pixels, only those near
;    the edge.
;
;    For historical reasons, the moments are split into two components:
;    The moments of all negative pixels in the annulus and the moments of
;    all positive pixels in the annulus.  Therefore, to get the true total
;    number of counts you must add possum+negsum.  Likewise, to get the
;    true center-of-mass (center-of-light) moment in either x or y, you
;    should add the positive and negative moments, ie.,
;       xmom = xcen + posxmom/possum + negxmom/negsum
;       ymom = ycen + posymom/possum + negymom/negsum
;    These numbers are returned separately because there are other reasons
;    for wanting the distributions separated.  For example, one use of this
;    routine is to compute the position of a star in an image.  For this
;    you would make two calls.  The first is a call with back=0. centered
;    (roughly) on the object.  you would then set inradious to be larger
;    than the object and outradius to something larger still.  The sky
;    (background) signal would then be possum+negsum.  Note this is NOT
;    how I use this routine for photometry, it's meant as an example ONLY.
;    The second call would then be made with inradius = 0 and outradius
;    to be just larger than the star.  This time, set back to the
;    background found in the first call.  The object brightness is then
;    possum+negsum counts above sky.  Then, I use just posxmom and posymom
;    as the position of the object.  Strictly speaking I should add the
;    negative moments, but, I know that the negative stuff is most likely
;    to be sky or bad pixels and irrelevant for the object location.
;    (see also centrod.pro)
;
;    Note: the use of this routine for computing sky signals has been
;    superceded by a very different algorithm.  basphote.pro uses a
;    combination of getannul.pro and robomean.pro to compute good sky
;    background values.  This routine is used mostly for object counts and
;    circular radial profiles.
;
; CATEGORY:
;    CCD data processing
; CALLING SEQUENCE:
;    Sumann, image, xcen, ycen, inradius, outradius, back, totweight, $
;            possum, negsum, posxmom, negxmom, posymom, negymom
;
; INPUTS:
;    image       : CCD image array.
;    xcen,ycen   : Center of annulus.
;    inradius    : Radius of inner circle.
;    outradius   : Radius of outer circle.
;    back        : Background to subtract from each pixel.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    totweight          : Area of annulus.
;    possum, negsum     : Sums of positive and negative pixels.
;    posxmom, negxmom   : Positive and negative x moments relative to xcen.
;    posymom, negymom   : Positive and negative y moments relative to ycen.
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Ported by Doug Loucks, Lowell Observatory, 1992 Oct, from the
;    C-language version written by Marc Buie.
;    April, 1993. DWL. Replaced the inner FOR loop with vectors.
;    98/09/21, MWB, optimizations
;    98/09/29, MWB, fixed nasty bug introduced during optimization.
;    2011/11/30, MWB, added output keywords XVAL, YVAL, WVAL
;-
pro sumann,image,xcen,ycen,inradius,outradius,back,totweight, $
           possum,negsum,posxmom,negxmom,posymom,negymom, $
           XVAL=x,YVAL=y,WVAL=wts

   totweight = 0.0

   possum  = 0.0
   negsum  = 0.0
   posxmom = 0.0
   negxmom = 0.0
   posymom = 0.0
   negymom = 0.0

   image_size=size(image)

   r2    = outradius + 2
   npts  = long(!pi*r2^2)
   x     = intarr(npts,/nozero)
   y     = intarr(npts,/nozero)
   flags = intarr(npts)

   ;  As the pixel location vectors (x, y) are assembled, the vector flags
   ;  is constructed with the following values:
   ;
   ;     0  : Trivial.  pixwt not needed.
   ;     1  : Trivial.  pixwt not needed.
   ;    -1  : On or near outer circle only.
   ;    -2  : On or near inner circle only.
   ;    -4  : On or near both circles.
   ;
   ;  Then the flags are used to calculate the final weights for each pixel
   ;  location.

   if 0 le inradius and inradius lt outradius then begin

      ; Compute the y-range limits.
      r2    = outradius
      r1    = inradius
      r2sq  = r2^2
      r1sq  = r1^2
      yp    = ycen-r2

      outy0=fix(yp)
      if outy0+0.5 lt yp then outy0=outy0+1

      yp=ycen+r2

      outy3=fix(yp+1)
      if outy3-0.5 gt yp then outy3=outy3-1

      base=0
      for yp=outy0,outy3 do begin

         cgetrng,ycen,xcen,r2,yp,outx0,outx1,outx2,outx3
         cgetrng,ycen,xcen,r1,yp,inx0,inx1,inx2,inx3
         n=outx3-outx0+1

         ; Assume on outer circle.
         xx=outx0+indgen(n)
         yy=replicate(yp,n)
         f =replicate(-1,n)  ; -1 = on or near outer circle

         ; Entirely inside outer circle.   (1)
         t = where( outx1 le xx and xx lt outx2, count )
         if count gt 0 then f[t] = 1

         ; On inner circle.
         t=where((inx0 le xx and xx lt inx1) or $
                 (inx2 le xx and xx lt inx3),count)
         if count gt 0 then f[t]=f[t]-3

         ; Entirely inside inner circle.  (0)
         t=where(inx1 le xx and xx lt inx2,count)
         if count gt 0 then f[t]=0
         
         x[base:base+n-1]=xx
         y[base:base+n-1]=yy
         flags[base:base+n-1]=f
         base=base+n

      endfor

      ; This weeds out all non-contributing pixels as well as the extra
      ;   storage (npts-base).
      z=where(flags ne 0,count)
      if count gt 0 then begin
         x=x[z]
         y=y[z]
         flags=flags[z]
      endif

      ; Keep only those pixels which are actually in the image array.
      xmin=min(x,max=xmax)
      ymin=min(y,max=ymax)
      if xmin lt 0 or xmax ge image_size[1] or $
         ymin lt 0 or ymax ge image_size[2] then begin
         z=where(x ge 0 and x le image_size[1] and $
                 y ge 0 and y le image_size[2],count)
         if count eq 0 then return
         x=x[z]
         y=y[z]
         flags=flags[z]
      endif

      wts=replicate(1.0,n_elements(flags))

      z=where(flags eq -1,count)
      if count gt 0 then wts[z]=pixwt(xcen,ycen,outradius,x[z],y[z]) > 0.

      z=where(flags eq -2,count)
      if count gt 0 then wts[z]=(1.0-pixwt(xcen,ycen,inradius,x[z],y[z])) > 0.

      z=where(flags eq -4,count)
      if count gt 0 then begin
         wts[z]=(pixwt(xcen,ycen,outradius,x[z],y[z] ) - $
                 pixwt(xcen,ycen,inradius, x[z],y[z] )) > 0.
      endif

      totweight=total(wts,/double)

      val=image[x,y]
      vxw=(val-back)*wts

      z=where(vxw gt 0,count)
      if count ne 0 then begin
         tmp     = vxw[z]
         possum  = total(tmp,/double)
         posxmom = total(tmp*(x[z]-xcen),/double)
         posymom = total(tmp*(y[z]-ycen),/double)
      endif

      z=where(vxw lt 0,count)
      if count ne 0 then begin
         tmp     = vxw[z]
         negsum  = total(tmp, /double )
         negxmom = total(tmp*(x[z]-xcen),/double)
         negymom = total(tmp*(y[z]-ycen),/double)
      endif

   endif

end
