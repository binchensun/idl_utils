;+
; NAME:
;  platlon
; PURPOSE:   (one line only)
;  Compute viewing and illumination geometry for Pluto
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  platlon,jd,elat,elon,slat,slon,polang,phang,earth,sun,elong
; INPUTS:
;  jd - Julian date for the geometry calculation
;               (double precision, scalar or vector)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  ORBIT - String with name of Charon orbit solution to use, the default
;             is to use the most recent orbit.  This default can change
;             as new orbits are added.
;            Currently supported:
;              '2012' - From AJ, 144,  15 (2012)
;              '2008' - From AJ, 135, 777 (2008)
;              '1997' - From Icarus, 125, 245 (1997)
; OUTPUTS:
;  elat - Latitude of the sub-earth point [radians]
;  elon - East longitude of the sub-earth point [radians]
;  slat - Latitude of the sub-solar point [radians]
;  slon - East longitude of the sub-solar point [radians]
;  polang - Position angle of Pluto's north pole, measured eastward from
;              north [radians]
;  phang - Solar phase angle (Sun-Pluto-Earth angle) [radians]
;  earth - Geocentric distance [AU]
;  sun   - Heliocentric distance [AU]
;  elong - Solar elongation [radians]
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;  This is a slow routine for vector calls.  Some of the supporting routines
;    cannot handle vectorized input so each output value is computed in an
;    explicit loop.
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2011/12/30 - Written by Marc W. Buie, Southwest Research Institute
;                 code based on fortran routines written by David Tholen
;  2012/06/04 - MWB, fixed typo on 2012 orbit.
;  2013/06/06 - MWB, added elong output.
;-
pro platlon,jd,elat,elon,slat,slon,polang,phang,earth,sun,elong,ORBIT=in_orbit

   self='platlon: '
   if badpar(jd,5,[0,1],caller=self+'(jd) ',npts=npts) then return
   if badpar(in_orbit,[0,7],0,caller=self+'(ORBIT) ', $
                              default='latest') then return

   if in_orbit eq 'latest' then orbit='2012' else orbit=in_orbit

   ; The zero-point of the longitude calculation for each orbit solution
   ;    is taken from mutual event calculations where JD=2447352.95988 has
   ;    a sub-earth longitude of 0.039396 degrees.  The first number in the
   ;    lonzpt line comes from running this program with lonzpt set to 0,
   ;    compute a longitude, and insert that into the lonzpt equation.

   case orbit of

      ; Orbit of Charon from Table 5 of the 2012 paper on Charon's orbit
      '2012': begin
         node   = 3.892490                   ; longitude of ascending node
         inc    =  96.218d0 / 180.0d0 * !dpi ; inclination
         L      = 4.50177d0                  ; mean longitude
         omega  = 0.0d0                      ; longitude of periapsis
         epoch  = 2452600.5d0                ; Epoch of elements
         period = 6.3872273d0                ; Orbital period in days
;         lonzpt = (39.128733d0 - 0.039396d0) / 180.0d0 * !dpi
         lonzpt = (359.698560 - 0.039396d0) / 180.0d0 * !dpi
;         lonzpt=0.0d0
      end

      ; Orbit of Charon from Table 3 of the 2008 paper on the Masses of Nix
      ;   and Hydra, AJ 135, 779
      '2008': begin
         node   = 223.054d0 / 180.0d0 * !dpi ; longitude of ascending node
         inc    =  96.168d0 / 180.0d0 * !dpi ; inclination
         L      = 257.960d0 / 180.0d0 * !dpi ; mean longitude
         omega  = 157.9d0   / 180.0d0 * !dpi ; longitude of periapsis
         epoch  = 2452600.5d0                ; Epoch of elements
         period = 6.38720d0                  ; Orbital period in days
         lonzpt = (158.87454d0 - 0.039396d0) / 180.0d0 * !dpi
      end

      ; Orbit values from 1997 HST orbit paper, Icarus, 125, p245, this is
      ;   the unqueual weight solution, first column of Table 3
      '1997': begin
         node   = 222.993d0 / 180.0d0 * !dpi ; longitude of ascending node
         inc    =  96.163d0 / 180.0d0 * !dpi ; inclination
         L      =  32.875d0 / 180.0d0 * !dpi ; mean longitude
         omega  = 219.1d0   / 180.0d0 * !dpi ; longitude of periapsis
         epoch  = 2449000.5d0                ; Epoch of elements
         period = 6.387223                   ; Orbital period in days
         M0     =  prival(L-node-omega)      ; mean anomaly at epoch
         lonzpt = (218.94368d0 - 0.039396d0) / 180.0d0 * !dpi
      end

      else: begin
         print,'[',orbit,'] is an invalid orbit choice'
         elat=!NULL
         elon=!NULL
         return
      end

   endcase

   ; Mean motion, radians/day
   n = 2.0d0*!dpi/period

   ; Compute Charon's orbit pole direction
   polera  = node - !dpi/2.0d0 
   poledec = !dpi/2.0d0 - inc

   ; Convert direction of pole into a unit vector
   sphrec,1.0d0,poledec,polera,xpole,ypole,zpole

   ; Mean anomaly at epoch
   M0 = prival(L-node-omega)

   ; Compute the position of Pluto (geocentric, barycenter of Pluto)
   ephem,jd,500,22,'P9',eph
   ra = trimrank(eph[0,*])
   dec = trimrank(eph[1,*])
   xobj = trimrank(eph[2,*])
   yobj = trimrank(eph[3,*])
   zobj = trimrank(eph[4,*])
   xsun = trimrank(eph[5,*])
   ysun = trimrank(eph[6,*])
   zsun = trimrank(eph[7,*])
   xos  = xobj - xsun
   yos  = yobj - ysun
   zos  = zobj - zsun

   au    = sqrt( xsun^2 + ysun^2 + zsun^2 )
   earth = sqrt( xobj^2 + yobj^2 + zobj^2 )
   sun   = sqrt( xos^2  + yos^2  + zos^2  )

   phang = acos((xos*xobj + yos*yobj + zos*zobj)/(earth*sun))

   elong = acos((xsun*xobj + ysun*yobj + zsun*zobj)/(earth*au))

   ; compute the Pluto-centric time
   ic = 0.0057755184970626873D ; day / AU (inverse speed of light)
   jdsys = jd - earth*ic

   ; compute mean anomaly
   manom = (n*(jdsys-epoch)+M0) mod (2.0d0*!dpi)
   manom = prival(manom+lonzpt)

   elat=dblarr(npts)
   elon=dblarr(npts)
   slat=dblarr(npts)
   slon=dblarr(npts)
   polang=dblarr(npts)

   for i=0,npts-1 do begin

      orbvec,inc,node,manom[i],p,q,r
      rotmat=[[p],[q],[r]]
      rotvec,-xobj[i],-yobj[i],-zobj[i],rotmat,xp,yp,zp,/reverse
      recsph,xp,yp,zp,dummy,elat0,elon0
      rotvec,-xos[i],-yos[i],-zos[i],rotmat,xp,yp,zp,/reverse
      recsph,xp,yp,zp,dummy,slat0,slon0

      elat[i] = elat0
      elon[i] = prival(elon0)
      slat[i] = slat0
      slon[i] = prival(slon0)

      ; rotate onto plane of sky
      skymat,dec[i],ra[i],rotmat
      rotvec,xpole,ypole,zpole,rotmat,xp,yp,zp

      ; compute position angle using the x and y components of the sky
      ;   plane vector
      polang[i] = prival(atan(yp,xp))

   endfor

   elat=trimrank(elat)
   elon=trimrank(elon)
   slat=trimrank(slat)
   slon=trimrank(slon)
   polang=trimrank(polang)

;print,elon*!radeg

end
