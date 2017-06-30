;+
; NAME:
;  rotvec
; PURPOSE:   (one line only)
;  Rotate 3-D vectors from one coordinate system to another
; DESCRIPTION:
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
;  rotvec,x,y,z,rotmat,xp,yp,zp
; INPUTS:
;  x     - Cartesian x-coordinate of point.
;  y     - Cartesian y-coordinate of point.
;  z     - Cartesian z-coordinate of point.
;  rotmat - 3x3 rotation matrix
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  REVERSE - Flag, if set performs the inverse rotation.
; OUTPUTS:
;  xp - Rotated x-coordinates.
;  yp - Rotated y-coordinates.
;  zp - Rotated z-coordinates.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2011/11/29, Written by Marc W. Buie, Southwest Research Institute
;                cloned from ROTATE.F written by David J. Tholen
;-
pro rotvec,x,y,z,in_rotmat,xp,yp,zp,REVERSE=reverse

   self='rotvec: '
   if badpar(x,[4,5],[0,1,2,3,4,5,6,7,8],CALLER=self+'(x): ') then return
   if badpar(y,[4,5],[0,1,2,3,4,5,6,7,8],CALLER=self+'(y): ') then return
   if badpar(z,[4,5],[0,1,2,3,4,5,6,7,8],CALLER=self+'(z): ') then return
   if badpar(in_rotmat,[4,5],2,CALLER=self+'(rotmat): ') then return
   if badpar(reverse,[0,1,2,3],0,CALLER=self+'(REVERSE): ', $
                default=0) then return

   if reverse then begin
      rotmat=transpose(in_rotmat)
   endif else begin
      rotmat=in_rotmat
   endelse

   xp = x*rotmat[0,0] + y*rotmat[0,1] + z*rotmat[0,2]
   yp = x*rotmat[1,0] + y*rotmat[1,1] + z*rotmat[1,2]
   zp = x*rotmat[2,0] + y*rotmat[2,1] + z*rotmat[2,2]

end
