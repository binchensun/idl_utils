;+
; NAME:
;  sc_pstar
;
; PURPOSE:
;  Search a star catalog for an isolated star near a given position.
;
; DESCRIPTION:
;  Search a star catalog for a reference star that is within a given
;  arc (RADIUS) from a point in the sky and has no brighter stars nearby within
;  a given distance (SUB_RADIUS).
;
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  sc_pstar,cra,cdec,radius,id,ra,dec,dra,ddec,mag,odra,oddec
; 
; INPUTS:
;  cra    - ra of search location  (radians, string)
;  cdec   - dec of search location  (radians, string)
;  radius - maximum disance from cra/cdec to look  (degrees)
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  MAG_MIN    - lower bound of star magnitude to return (default= 12.0)
;  MAG_MAX    - upper bound of star magnitude to return (default= -3.0)
;  SCLASS     - Spectral classification, as a regexp (default=all types)
;                   (eg. [ab]0 for a0 and b0 class)
;  DBNAME     - Name of DB to access (default='obs')
;  TBLNAME    - Name of table to query in the DB (default='ppm_catalog')
;  SUB_RADIUS - region around reference star where no brighter star is
;                 allowed (arcsec, default=300 arcsec).
;  SILENT     - Flag, if set will suppress printed output
;
; OUTPUTS: Returns a suitable reference star
;  id   - name of star
;  ra   - ra  (radians)
;  dec  - dec  (radians)
;  dra  - ra proper motion
;  ddec - dec proper motion
;  mag  - magnitude of star
;  odra - ra corrected for proper motion to date program is run
;  oddec - dec corrected for proper motion to date program is run
;  
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2002/02/19, Writen by David Tucker
;  2012/04/03, MWB, added SILENT, changed units on radius and sub_radius,
;                 added string input option on ra,dec, added printed output
;-

pro sc_pstar, in_ra, in_dec, radius,$
     ret_id, ret_ra, ret_dec, ret_dra, ret_ddec, ret_mag, $
     od_ra, od_dec, $
     MAG_MIN=mag_min, MAG_MAX=mag_max, SCLASS=sclass, $
     DBNAME=dbname, TBLNAME=tblname, SUB_RADIUS=sub_radius, $
     SILENT=silent

   self='sc_pstar: '
   if badpar(in_ra,[4,5,7],0,caller=self+'(ra) ',type=ratype) then return
   if badpar(in_dec,[4,5,7],0,caller=self+'(dec) ',type=dectype) then return
   if badpar(radius,[2,3,4,5],0,caller=self+'(radius) ') then return

   if badpar(dbname,[0,7],0,caller=self+'(DBNAME) ', $
                            default='obs') then return
   if badpar(tblname,[0,7],0,caller=self+'(TBLNAME) ', $
                             default='ppm_catalog') then return
   if badpar(sub_radius,[0,2,3,4,5],0,caller=self+'(SUB_RADIUS) ', $
                        default=300.0) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                        default=0) then return

   if ratype  eq 7 then ra=raparse(in_ra)   else ra=in_ra
   if dectype eq 7 then dec=decparse(in_dec) else dec=in_dec

   dist1 = radius/!radeg            ; large radius for primary search
   dist2 = sub_radius/3600.0/!radeg ; small radius for confusion filter

   ; this will flag an empty return
   ret_id = -1

   jdnow=systime(/ut,/julian)
   dt=(jdnow-jdparse('2000-01-01'))/365.25 ; years

   ;open the db
   openmysql, db, dbname

   ;query mysql db for bounding box
   sc_nearest, db, ra, dec, dist1, $
      t_id, t_ra, t_dec, t_dra, t_ddec, t_mag, $
      MAG_MIN=mag_min, MAG_MAX=mag_max, SCLASS=sclass, $
      TBLNAME=tblname

   ncand=n_elements(t_id)
   if ncand eq 1 and t_id[0] eq -1 then begin
      print,self,'No stars found in ',strn(radius),' degrees.'
      goto,bailout
   endif

   sep=angsep(ra,dec,t_ra,t_dec)*!radeg
   if not silent then begin
      rastr,ra,1,ras
      decstr,dec,0,decs
      print,'Searching within ',strn(radius),' degree of ',ras,' ',decs
      print,'Magnitude range is from ', $
            strn(mag_max,format='(f10.1)'),' to ', $
            strn(mag_min,format='(f10.1)')
      print,'Initial search came up with ',strn(ncand),' candidates.'
      print,'Maximum distance of the candidates is ', $
            strn(max(sep),format='(f10.2)'),' degrees.'
   endif

   ;query mysql db for brighter star within sub_radius distance
   for i=0, n_elements(t_ra)-1 do begin
      sc_bcheck, db, t_ra[i], t_dec[i], dist2, t_mag[i], count, $
         TBLNAME=tblname
      if count le 1 then begin
         ret_id   = t_id[i]
         ret_ra   = t_ra[i]
         ret_dec  = t_dec[i]
         ret_dra  = t_dra[i]
         ret_ddec = t_ddec[i]
         ret_mag  = t_mag[i]
         od_ra    = ret_ra + ret_dra*dt
         od_dec   = ret_dec + ret_ddec*dt
         if not silent then begin
            rastr,ret_ra,2,ras
            decstr,ret_dec,1,decs
            rastr,od_ra,2,odras
            decstr,od_dec,1,oddecs
            print,''
            print,'Acquisition star found: ', $
                  'PPM ',string(ret_id,format='(i6.6)')
            print,'Epoch/equinox 2000 position ',ras,' ',decs
            print,'Proper motion ', $
                  strn(ret_dra*!radeg/15.0*3600.0,format='(f10.4)'),' s/yr ', $
                  strn(ret_ddec*!radeg*3600.0,format='(f10.3)'),' "/yr', $
                  '  [dt=',strn(dt,format='(f10.1)'),'y]'
            print,'   correction ', $
                  strn(ret_dra*dt*!radeg/15.0*3600.0,format='(f10.2)'), $
                  '        ', $
                  strn(ret_ddec*dt*!radeg*3600.0,format='(f10.1)')
            print,''
            print,'Current epoch J2000 position ',odras,' ',oddecs, $
                  ' mag=',strn(ret_mag,format='(f10.1)'),'  ', $
                  strn(sep[i],format='(f10.1)'),' deg away'
         endif
         break
      endif else begin
         if not silent then begin
            print,'Reject PPM ',string(t_id[i],format='(i6.6)'), $
                  ' [',strn(t_mag[i],format='(f10.1)'),'],', $
                  ' it has ',strn(count),' brighter sources within ', $
                  strn(sub_radius,format='(f10.1)'),' arcsec'
         endif
      endelse
   endfor

bailout:
   free_lun, db

end
