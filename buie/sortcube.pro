;+
; NAME:
;  sortcube
; PURPOSE:   (one line only)
;  Sort an image cube by brightness at each pixel
; DESCRIPTION:
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  sortcube,incube,outcube
; INPUTS:
;  incube - 3-D image cube to be sorted
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  outcube - 3-D image cube with the brightest pixels in plane 0, faintest
;               pixels in the last plane.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2012/07/16, Written by Marc W. Buie, Southwest Research Institute based on
;                 and algorithmic description provided by Mark Showalter.
;-
pro sortcube,incube,outcube

   self='sortcube: '
   if badpar(incube,[2,3,4,5],3,caller=self+'(incube) ') then return

   sz=size(incube,/dimen)

   outcube=incube
   
   for i=0,sz[0]-1 do begin
      for j=0,sz[1]-1 do begin
         idx=reverse(sort(outcube[i,j,*]))
         outcube[i,j,*] = outcube[i,j,idx]
      endfor
   endfor

end
