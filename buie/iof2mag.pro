;+
; NAME:
;  iof2mag
; PURPOSE:   (one line only)
;  Compute the flux (mJy) of unit surface area (1 km$^2$) with unit reflectivity
; DESCRIPTION:
;  Cloned from amag.f
;
;  The magnitude of the object is computed using an absolute B magnitude
;  of the sun of 5.51 (at 10 parsecs).  This corresponds to a B magnitude of
;  -26.066 at 1 AU.  The formula for computing the magnitude of the object
;  is:                         ( galb * rad^2 )
;        Mb = -26.066 - 2.5 log( ------------ )
;                              (   r^2 * d^2  )
;  The units in this equation are not the same as those passed to the function.
;  The units are: [rad] = cm, [r] = cm, [d] = AU.
;  All full powers of ten are removed from inside the log to prevent
;  under/overflow.
;
;  here galb is taken to be the same as I/f, or bi-directional reflectance
;    divided by pi.
;
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  result=iof2mag(iof,filter,radius,sun,earth)
; INPUTS:
;  iof    - I/F value, averaged over body
;  filter = scalar string of filter of the calculation
;  radius = radius of object in kilometers, only used to calculate area
;  sun    - distance from sun to object in AU
;  earth  - distance from earth (observer) to object in AU
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is the standard magnitude for the given I/F value
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2012/01/29, Written by Marc W. Buie, Southwest Research Institute
;-
function iof2mag,iof,filter,radius,sun,earth

   au=1.495979D13 ; [km]

   if filter eq 'B' then begin
      sunmag = -26.066d0 ; reference?
   endif else if filter eq 'V' then begin
      sunmag = -26.79d0  ; not a rigoruous value, need to look this up
   endif else begin
      print,'Filter [',filter,'] is not supported.'
      return,-1
   endelse

   const=40.8746
   stdmag= (sunmag+const) - 2.5*alog10(iof*(radius/sun/earth)^2)
   return,stdmag

end
