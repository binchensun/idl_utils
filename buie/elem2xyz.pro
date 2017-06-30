;+
; NAME:
;  elem2xyz
; PURPOSE:   (one line only)
;  Compute position given orbital elements (2-body Keplerian)
; DESCRIPTION:
; CATEGORY:
;  Celestial Mechanics
; CALLING SEQUENCE:
;  elem2xyz,jdepoch,manom,arg,node,inc,ecc,semi,jdpos,x,y,z
; INPUTS:
;  jdepoch - Julian date of mean anomaly
;  m       - Mean anomaly (radians)
;  arg     - Argument of perihelion (radians)
;  node    - Longitude of ascending node (radians)
;  inc     - Inclination (radians)
;  ecc     - Eccentricity
;  a       - Semi-major axis (AU)
;  jdpos   - Julian date desired (scalar or vector)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  ECLIPTIC - Flag, if set returns x,y,z in Heliocentric ecliptic coordinates
; OUTPUTS:
;  x    - Heliocentric x coordinates (AU)
;  y    - Heliocentric y coordinates (AU)
;  z    - Heliocentric z coordinates (AU)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;  Note that there is nothing special about the time.  You can, if you wish
;    specify jdepoch as 0. and then have jdpos be a simple incrementing
;    vector starting at zero.  In other words, jdpos is really just relative
;    to jdepoch.
; MODIFICATION HISTORY:
;  2002/05/29 - Marc W. Buie, Lowell Observatory.  Ported from fortran routines
;    supplied by Larry Wasserman and Ted Bowell.
;  2012/12/09, MWB, changed calling sequence to require time and now allow
;                     vector input on jdpos.
;  2013/08/16, MWB, improve options for mixing and matching vector and
;                     scalar inputs.
;-
pro elem2xyz,in_jdepoch,in_m,arg,node,inc,ecc,a,jdpos,x,y,z, $
             ECLIPTIC=ecliptic

   self='ELEM2XYZ: '
   if badpar(in_jdepoch,[4,5],0,caller=self+'(jdepoch) ',npts=njde) then return
   if badpar(in_m,[4,5],[0,1],caller=self+'(m) ',npts=nm) then return
   if badpar(arg,[4,5],[0,1],caller=self+'(arg) ',npts=narg) then return
   if badpar(node,[4,5],[0,1],caller=self+'(node) ',npts=nnode) then return
   if badpar(inc,[4,5],[0,1],caller=self+'(inc) ',npts=ninc) then return
   if badpar(ecc,[4,5],[0,1],caller=self+'(ecc) ',npts=necc) then return
   if badpar(a,[4,5],[0,1],caller=self+'(a) ',npts=na) then return
   if badpar(jdpos,[4,5],[0,1],caller=self+'(jdpos) ',npts=njd) then return

   if badpar(ecliptic,[0,1,2,3],0,caller=self+'(ECLIPTIC) ', $
                                  default=0) then return

   ; all element vectors must have the same length
   check=[nm,narg,nnode,ninc,necc,na]
   if min(check) ne max(check) then begin
      print,self,'Orbital element vectors do not have the same length.'
      return
   endif

   ; clean up jdepoch
   if njde ne 1 and njde ne nm then begin
      print,self,'JDEPOCH must either be scalar or match length of elements.'
      return
   endif else if njde eq 1 and nm eq 1 then begin
      jdepoch=in_jdepoch
   endif else if njde eq 1 then begin
      jdepoch=replicate(in_jdepoch,nm)
   endif else begin
      jdepoch=in_jdepoch
   endelse

   ; assume here that time will be increasing, start with given mean anomaly
   ;   and keep moving it along from time to time
   m = in_m

   x=dblarr(njd,nm)
   y=dblarr(njd,nm)
   z=dblarr(njd,nm)

   for i=0,njd-1 do begin

      newanom,jdepoch,m,a,jdpos[i],m

      ; compute auxillary elements
      auxelem,node,arg,inc,px,qx,py,qy,pz,qz

      ; solve Kepler's Equation
      kepler1,m,ecc,eccanom

      ; Given eccentric anomaly, compute equatorial positions 
      eccrec,a,ecc,eccanom,px,qx,py,qy,pz,qz,x0,y0,z0

      x[i,*]=x0
      y[i,*]=y0
      z[i,*]=z0

   endfor

   ; Rotate to ecliptic coordinates, if desired
   if ecliptic then begin

; J2000 obliquity in radians (Standish, 1981)
;      obl = 0.4090926292045900525273794d0

      ; J2000 obliquity in radians, IAU
      obl = 0.4090928042223289096135375d0

      ye =  y*cos(obl) + z*sin(obl)
      ze = -y*sin(obl) + z*cos(obl)
      y  = ye
      z  = ze

   endif

   x=trimrank(x)
   y=trimrank(y)
   z=trimrank(z)

end
