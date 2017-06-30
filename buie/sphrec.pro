;+
; NAME:
;  sphrec
; PURPOSE:   (one line only)
;  Convert from spherical to rectangular coordinates
; DESCRIPTION:
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
;  sphrec,r,lat,lon,x,y,z
; INPUTS:
;  r - distance from origin
;  lat - angular distance from x-y plane (radians)
;  lon - longitude (angle from x axis) in radians
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  x     - Cartesian x-coordinate (same units as r).
;  y     - Cartesian y-coordinate (same units as r).
;  z     - Cartesian z-coordinate (same units as r).
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  rank and size of all inputs must match or be scalar.
; PROCEDURE:
; MODIFICATION HISTORY:
;  2011/11/29, Written by Marc W. Buie, Southwest Research Institute
;                guided by SPTORC.F written by David J. Tholen, and
;                sphrec.f written by Marc Buie
;-
pro sphrec,r,lat,lon,x,y,z

   self='sphrec: '
   if badpar(r,[4,5],[0,1,2,3,4,5,6,7,8],CALLER=self+'(r): ') then return
   if badpar(lat,[4,5],[0,1,2,3,4,5,6,7,8],CALLER=self+'(lat): ') then return
   if badpar(lon,[4,5],[0,1,2,3,4,5,6,7,8],CALLER=self+'(lon): ') then return

   clat = cos(lat)

   x = r*cos(lon)*clat
   y = r*sin(lon)*clat
   z = r*sin(lat)

end
