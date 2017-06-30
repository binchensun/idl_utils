;+
; NAME:
;    boxm
; PURPOSE: (one line)
;    Find location of a maximum within a sub-array.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;    boxm, image, xcen, ycen, deltay, deltax, xmax, ymax
; INPUTS:
;    image      : Image array.
;    xcen, ycen : Center of sub-array.
;    deltax     : Half-width of sub-array.
;    deltay     : Half-height of sub-array.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    ABSMAX     : Flag, if set, looks for pixel with greatest absolute value.
;
; OUTPUTS:
;    xmax, ymax : Coordinates, in image, of local maximum.
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Ported by Doug Loucks, Lowell Observatory, 1992 Oct, from the
;    C-language version written by Marc Buie.
;    4/1/93, DWL, Added argument validation (badpar).
;    1/26/94, MWB, Added ABS argument.
;    12/13/95, MWB, added support for 3-d input arrays.
;    98/09/21, MWB, added NOCHECK keyword to speed up execution.
;    2002/07/12, MWB, changed function so that xcen,ycen,deltax,deltay
;                  are changed to long.  xcen,ycen are rounded, deltax/y
;                  are truncated.
;-
pro boxm,image,xcen,ycen,deltax,deltay,xmax,ymax, $
         ABSMAX=absmax,NOCHECK=nocheck

   if not keyword_set(nocheck) then begin
      ; Validate the number of arguments.  If not seven, print a usage line.
      if n_params() ne 7 then begin
         message,'boxm,image,xcen,ycen,deltax,deltay,xmax,ymax',/info
         return
      endif

      ; Validate the input parameters.
      self='boxm: '
      if badpar(image,[1,2,3,4,5,12,13,14,15],[2,3],CALLER=self+'(image) ', $
                                                    rank=imrank) then return
      if badpar(xcen,[1,2,3,4,5],0,CALLER=self+'(xcen) ') then return
      if badpar(ycen,[1,2,3,4,5],0,CALLER=self+'(ycen) ') then return
      if badpar(deltax,[1,2,3,4,5],0,CALLER=self+'(deltax) ') then return
      if badpar(deltay,[1,2,3,4,5],0,CALLER=self+'(deltay) ') then return
   endif else begin
      imrank = size(image,/n_dimensions)
   endelse

   deltax = long(deltax)
   deltay = long(deltay)

   s_image = size(image)

   x_size = s_image[1]
   y_size = s_image[2]

   startx = long(xcen - deltax + 0.5)
   stopx = startx + 2*deltax
   starty = long(ycen - deltay + 0.5)
   stopy = starty + 2*deltay

   ; Make selected sub-array fit into the large array.
   startx = 0 or (startx ge 0 and startx lt x_size) * startx or $
                   (startx ge x_size) * (x_size - 1)
   stopx  = 0 or (stopx ge 0 and stopx lt x_size) * stopx or $
                   (stopx ge x_size) * (x_size - 1)
   starty = 0 or (starty ge 0 and starty lt y_size) * starty or $
                   (starty ge y_size) * (y_size - 1)
   stopy  = 0 or (stopy ge 0 and stopy lt y_size) * stopy or $
                   (stopy ge y_size) * (y_size - 1)

   if imrank eq 2 then begin

      ; Extract the sub-array.
      t = image[ startx : stopx, starty : stopy ]

      ; Take absolute value (if requested)
      if keyword_set(absmax) then t=abs(t)

      ; Get size info.
      t_size = size(t)
      t_xsize = t_size[1]

      ; Locate the local maximum.
      t1=where(t eq max(t), count)

      ; Compute the image array coordinates of the local maximum.
      if count ne 0 then begin
         w = t1[0] / t_xsize
         xmax = startx + t1[0] - w * t_xsize
         ymax = starty + w
      endif

   endif else begin
      nframes = s_image[3]
      xmax = replicate(-10,nframes)
      ymax = replicate(-10,nframes)
      for i=0,nframes-1 do begin
         t = image[ startx : stopx, starty : stopy, i ]
         if keyword_set(absmax) then t = abs(t)
         t_size = size(t)
         t_xsize = t_size[1]
         t1 = where( t eq max(t), count )
         t_size = size(t)
         t_xsize = t_size[1]
         t1 = where( t eq max(t), count )
         if count ne 0 then begin
            w = t1[0] / t_xsize
            xmax[i] = startx + t1[0] - w * t_xsize
            ymax[i] = starty + w
         endif
      endfor
   endelse

end
