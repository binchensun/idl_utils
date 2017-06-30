;+
; NAME:
;  newanom
; PURPOSE:   (one line only)
;  Compute new mean anomaly from old given date.
; DESCRIPTION:
; CATEGORY:
;  Celestial Mechanics
; CALLING SEQUENCE:
;  newanom,jdepoch,m,a,newjd,newm
; INPUTS:
;  jdepoch - Julian date of mean anomaly
;  m       - Mean anomaly (radians)
;  a       - Semi-major axis (AU)
;  newjd   - Julian date of mean anomaly desired
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  newm    - New mean anomaly (radians)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2002/05/29 - Marc W. Buie, Lowell Observatory.  Ported from fortran routines
;    supplied by Larry Wasserman and Ted Bowell.
;  2013/08/16, MWB, modified to allow the same variable to be given for
;                     input and output mean anomaly
;-
pro newanom,jdepoch,in_m,a,newjd,newm

   gk=0.01720209895d0

   m = in_m

   dt = newjd-jdepoch

   mean_motion = gk*a^(-1.5d0)

   newm = m + dt*mean_motion

   newm = newm mod (2.0d0*!dpi)

   z=where(newm lt 0.0d0, count)
   if count ne 0 then newm[z]=newm[z]+2.0d0*!dpi

end
