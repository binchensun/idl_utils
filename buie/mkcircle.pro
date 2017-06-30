;+
; NAME:
;  mkcircle
; PURPOSE:   (one line only)
;  Compute points that lie on a circle
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  mkcircle,xcenter,ycenter,radius,x,y
; INPUTS:
;  xcenter - Scalar, x position of the center of the circle
;  ycenter - Scalar, y position of the center of the circle
;  radius  - Radius of the circle
; OPTIONAL INPUT PARAMETERS:
;  NPTS    - Number of points along the circle (default=100)
;             Note that the first point and the last point are identical
;             and that the length of the output vector is 1 larger than
;             the value for NPTS.
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  x - vector with the points
;  y - vector with the points
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2013/03/06
;-
pro mkcircle,xcenter,ycenter,radius,x,y,NPTS=npts

   self='mkcircle: '
   if badpar(xcenter,[2,3,4,5],0,caller=self+'(xcenter) ') then return
   if badpar(ycenter,[2,3,4,5],0,caller=self+'(ycenter) ') then return
   if badpar(radius,[2,3,4,5],0,caller=self+'(radius) ') then return
   if badpar(npts,[0,2,3],0,caller=self+'(NPTS) ',default=100) then return

   ang=(2*!pi/float(npts))*findgen(npts+1)
   x=xcenter+radius*cos(ang)
   y=ycenter+radius*sin(ang)

end
