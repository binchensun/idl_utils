;+
; NAME:
;  stageselect
; PURPOSE:
;  Selects catalog stars for a given RA/Dec range.
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  stageselect,minra,maxra,mindec,maxdec, $
;                ra,dec,rasig,decsig,b,bsig,v,vsig,bmv,bmvsig
; INPUTS:
;  minra - lower Right Ascension constraint (radians)
;  maxra - upper Right Ascension constraint (radians)
;  mindec - lower Declination constraint (radians)
;  maxdec - upper Declination constraint (radians)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  ra     - array containing RAs of standards within limits, J2000, radians
;  dec    - array containing Decs of standards within limits, J2000, radians
;  rasig  - array of uncertainties of RAs (arcsec)
;  decsig - array of uncertainties of Decs (arcsec)
;  b      - array of B magnitudes of standards
;  bsig   - array of uncertainties of B magnitudes
;  v      - array of V magnitudes of standards
;  vsig   - array of uncertainties of V magnitudes
;  bmv    - array of colors of standards
;  bmvsig - array of uncertainties of colors
;  nstars - number of stars returned
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  This gets its data from the phot.plphot database table.
; PROCEDURES:
; MODIFICATION HISTORY:
;  2012/03/06 Written by Erin R. George, Southwest Research Institute
;  2012/12/06, MWB, added nstars return and some cosmetic cleanup
;-
pro stageselect,minra,maxra,mindec,maxdec, $
                ra,dec,rasig,decsig,b,bsig,v,vsig,bmv,bmvsig,nstars

   self='stageselect: '
   if badpar(minra,[4,5],0,CALLER=self+'(minra): ') then return
   if badpar(maxra,[4,5],0,CALLER=self+'(maxra): ') then return
   if badpar(mindec,[4,5],0,CALLER=self+'(mindec): ') then return
   if badpar(maxdec,[4,5],0,CALLER=self+'(maxdec): ') then return

;NEED to watch out for RA wrap!  This routine does not yet handle this case.

   openmysql,dblun,'phot'
   cmd=['select ra,decl,rasig,decsig,b,bsig,v,vsig,bmv,bmvsig', $
        'from plphot', $
        'where subid like '+quote('Stage%'), $
        'and ra>='+strn(minra), $
        'and ra<='+strn(maxra), $
        'and decl>='+strn(mindec), $
        'and decl<='+strn(maxdec), $
        'order by decl;']
   mysqlquery,dblun,cmd,ra,dec,rasig,decsig,b,bsig,v,vsig,bmv,bmvsig, $
              format='d,d,d,d,f,f,f,f,f,f',ngood=nstars
   free_lun,dblun

end


