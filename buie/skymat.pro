;+
; NAME:
;  skymat
; PURPOSE:   (one line only)
;  Compute the sky plane rotation matrix from angular coordinates
; DESCRIPTION:
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
;  skymat,lat,lon,rotmat
; INPUTS:
;  lat - latitude (spherical coordinate, radians, scalar)
;  lon - longitude (spherical coordinate, radians, scalar)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  rotmat - rotation matrix (always double, 3x3 matrix)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2011/11/29, Written by Marc W. Buie, Southwest Research Institute
;                cloned from SKYMAT.F written by David J. Tholen
;-
pro skymat,lat,lon,rotmat

   self='skymat: '
   if badpar(lat,[4,5],0,CALLER=self+'(lat): ') then return
   if badpar(lon,[4,5],0,CALLER=self+'(lon): ') then return

   clat = cos(double(lat))
   slat = sin(double(lat))

   clon = cos(double(lon))
   slon = sin(double(lon))

   rotmat=dblarr(3,3)

   rotmat[0,0] = -clon*slat
   rotmat[0,1] = -slon*slat
   rotmat[0,2] =       clat

   rotmat[1,0] = -slon
   rotmat[1,1] =  clon
   rotmat[1,2] =  0.0D0

   rotmat[2,0] = -clon*clat
   rotmat[2,1] = -slon*clat
   rotmat[2,2] =      -slat

end
