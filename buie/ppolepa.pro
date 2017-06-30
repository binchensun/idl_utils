;+
; NAME:
;  ppolepa
; PURPOSE:   (one line only)
;  Compute the position angle of Pluto's pole on the plane of the sky
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  ppolepa,jd,posang
; INPUTS:
;  jd - Julian date for the position angle calculation
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  posang - position angle of the projected pole, E of N, radians
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  There is a hardcoded value for the inclination and node of Charon's orbit
;  that is used to compute the pole of Charon's orbit.  This pole is assumed
;  be coincident with Pluto's rotation pole.
; PROCEDURE:
; MODIFICATION HISTORY:
;  2011/11/29, Written by Marc W. Buie, Southwest Research Institute
;                guided by PPOLEPA.F written by David J. Tholen
;-
pro ppolepa,jd,posang

   self='ppolpa: '
   if badpar(jd,[4,5],[0,1],CALLER=self+'(jd): ',npts=npts) then return

   posang=dblarr(npts)

   ; If the orbit of Charon is updated, the values for its inclination
   ; and ascending node (mean equator and equinox of J2000, units of
   ; degrees) should be updated here (these values are from Table 3 of
   ; the 2008 paper on the Masses of Nix and Hydra, AJ 135, 779)
   node = 223.054d0 / 180.0d0 * !dpi
   inc  =  96.168d0 / 180.0d0 * !dpi

   ; Compute Charon's pole direction
   polera = node - !dpi/2.0d0 
   poledec = !dpi/2.0d0 - inc

   ; Convert direction of pole into a unit vector
   sphrec,1.0d0,poledec,polera,xpole,ypole,zpole

   ; Compute the ra,dec of Pluto (geocentric, barycenter)
   ephem,jd,500,2,'P9',eph
   ra = trimrank(eph[0,*])
   dec = trimrank(eph[1,*])

   ; Must compute one point at a time (skymat only takes scalars right now)
   for i=0,npts-1 do begin

      ; rotate onto plane of sky
      skymat,dec[i],ra[i],rotmat
      rotvec,xpole,ypole,zpole,rotmat,xp,yp,zp

      ; compute position angle using the x and y components of the sky
      ;   plane vector
      posang[i] = atan(yp,xp)
   endfor

   z=where(posang lt 0.0,count)
   if count ne 0 then posang[z]=posang[z]+2.0d0*!dpi
   posang=trimrank(posang)

end
