;+
; NAME:
;  mkrundate
; PURPOSE:   (one line only)
;  Create a run date string given a Julian Date.
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  rundate = mkrundate(jd)
; INPUTS:
;  jd - Julian Date (scalar)
; OPTIONAL INPUT PARAMETERS:
;  DATE - Flag, if set will format the output as a valid mysql date string
;           (ex: 2012-03-12).  The default is a rundate string which is
;           a 6-digit string (ex: 120312).
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is a 6-digit string that is the run date for the input JD
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2008/06/22, Written by Marc W. Buie, Southwest Research Institute
;  2012/03/07, MWB, added DATE keyword
;-
function mkrundate,jd,DATE=date

   self='rundate: '
   if badpar(jd,[4,5],0,caller=self+'(jd) ') then return,''
   if badpar(date,[0,1,2,3],0,caller=self+'(DATE) ',default=0) then return,''

   if date then begin
      jdstr,jd,100,str,timesep='-'
      return,str
   endif else begin
      jdstr,jd,300,str
      return,strmid(str,2,6)
   endelse
   
end
