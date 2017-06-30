;+
; NAME:
;  naifname
; PURPOSE:
;  Convert an ephem standard name to a common name (NAif name scheme)
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  propername = naifname(standardname)
; INPUTS:
;  standardname = string, standard name code (see EPHEM)
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  return is the proper string name, or, the input if standard name isn't
;    recognized.
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODifICATION HISTORY:
;  1997/02/12 - Written by Marc W. Buie, Lowell Observatory
;  2009/08/19, MWB, added some new codes
;  2013/03/06, MWB, allow vector input, added new names
;-
function naifname,stdname
   
   self='NAIFname: '
   if badpar(stdname,7,[0,1],caller=self+'(stdname) ',npts=npts) then return,''

   name=strlowcase(stdname)
   code=strmid(name,0,1)
   id=strmid(name,1)

   for i=0,npts-1 do begin

      if code[i] eq 'p' then begin
         case id[i] of
            '1': begin
               name[i] = 'Mercury'
               end
            '2': begin
               name[i] = 'Venus'
               ;  2013/03/06, MWB, allow vector input, added new names
               end
            '3': begin
               name[i] = 'Earth'
               end
            '301': begin
               name[i] = 'Moon'
               end
            '4': begin
               name[i] = 'Mars'
               end
            '5': begin
               name[i] = 'Jupiter'
               end
            '501': begin
               name[i] = 'Io'
               end
            '502': begin
               name[i] = 'Europa'
               end
            '503': begin
               name[i] = 'Ganymede'
               end
            '504': begin
               name[i] = 'Callisto'
               end
            '6': begin
               name[i] = 'Saturn'
               end
            '7': begin
               name[i] = 'Uranus'
               end
            '8': begin
               name[i] = 'Neptune'
               end
            '801': begin
               name[i] = 'Triton'
               end
            '9': begin
               name[i] = 'Pluto Barycenter'
               end
            '901': begin
               name[i] = 'Charon'
               end
            '902': begin
               name[i] = 'Nix'
               end
            '903': begin
               name[i] = 'Hydra'
               end
            '904': begin
               name[i] = 'P4'
               end
            '905': begin
               name[i] = 'P5'
               end
            '999': begin
               name[i] = 'Pluto'
               end
            else: begin
               name[i] = stdname[i]
               endelse
         endcase
      endif else begin
         name[i]=stdname[i]
      endelse
   endfor

   return,trimrank(name)
end
