;+
; NAME:
;  recsph
; PURPOSE:   (one line only)
;  Convert from rectangular to spherical coordinates
; DESCRIPTION:
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
;  recsph,x,y,z,r,lat,lon
; INPUTS:
;  x     - Cartesian x-coordinate (same units as r).
;  y     - Cartesian y-coordinate (same units as r).
;  z     - Cartesian z-coordinate (same units as r).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  r - distance from origin
;  lat - angular distance from x-y plane (radians)
;  lon - longitude (angle from x axis) in radians
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  rank and size of all inputs must match or be scalar.
; PROCEDURE:
; MODIFICATION HISTORY:
;  2011/11/29, Written by Marc W. Buie, Southwest Research Institute
;                converted from sphrec.f
;-
pro recsph,x,y,z,r,lat,lon

   self='recsph: '
   if badpar(x,[4,5],[0,1,2,3,4,5,6,7,8],CALLER=self+'(x): ') then return
   if badpar(y,[4,5],[0,1,2,3,4,5,6,7,8],CALLER=self+'(y): ') then return
   if badpar(z,[4,5],[0,1,2,3,4,5,6,7,8],CALLER=self+'(z): ') then return

   r = sqrt( double(x)^2 + double(y)^2 + double(z)^2 )

   lat=x*0.0d0
   lon=lat

   z0=where(r ne 0,count)

   if count ne 0 then begin
      lat[z0] = asin(z[z0]/r[z0])
      lon[z0] = atan(y[z0],x[z0])
   endif

end
