; docformat = 'rst'
;
; NAME:
;   bcLayout
;+
; The purpose of this program is to return the normalized position coordinates for 
; a line plot, contour plot, or image plot with a specific "layout" in the current
; graphics window. A "layout" has a specified grid of columns and rows organized 
; inside a graphics display window. This is similar to the positions calculated by 
; !P.Multi, although a great deal more flexible. Of course, with additional flexibility 
; and power comes more responsibility. You will be required to use the NoErase keyword
; correctly and you will be responsible for setting the character size on your plots.
; These jobs are normally handled by !P.Multi. Some users will find this liberating, some 
; will find it a pain in the keister. There is always !P.Multi to go back to.
; 
; A grid position is a combination of the number of columns and rows desired, plus
; the application of inside and outside margins, as well as a desired aspect ratio.
; Margins are specified in units of one-third the !D.X_PX_CM value to be device independent. 
; The outside margins locate the grid inside the graphics display window.
; (These are equivalent to !X.OMargin and !Y.OMargin system variable when displaying
; a grid with !P.Multi, for example.) Inside margins use the same units, but are
; used to modify the initial grid positions in the window. (These are equivalent
; to using the XMargin and YMargin keywords on a plot command.) For example, inside margins
; might be used to leave room inside a larger position for color bars or other annotations
; you wish to put on a graphics display. The aspect ratio modifies the grid position
; after the outside and inside margins have been applied to create the final grid position,
; which will be centered in its initial grid position.
;
; :Categories:
;    Graphics, Utilities
;    
; :Returns:
;    This function returns the normalized position coordinates for a particular
;    location in a grid of locations (if the layout parameter is a three-element array),
;    or a 4-by-(ncols*nrows) array of grid positions with one position for each location
;    in the ncols by nrows grid of locations (if the layout parameter is a two-element array).
;    The return variable is suitable for passing to the POSITION keyword of an IDL graphics
;    command (e.g., cgPlot, cgContour, cgImage, etc.). The grid positions are organized in
;    row order, starting with the grid location in the upper-left of the graphihcs display window.
;    
; :Params:
; 
;    layout: in, required, type=integer
;         This parameter sets up the grid layout for the current graphics window. A grid
;         is organized by columns and rows, with the first grid position located in the upper-left
;         corner of the current graphics display window and proceeding in row order. This parameter
;         is either a two-element vector, giving the number of columns and number of rows,
;         respectively (e.g., [ncols,nrows]), or it is a three-element vector, giving, in addition, the
;         specific grid location for which a position is required (e.g, [ncols, nrows, gridPosition]).
;         Grid positions start at 1 with the first grid in the upper left corner of the graphics display 
;         window and proceed in row order, sequentually, until the last grid position, which is equal 
;         to the number of columns times the number of rows.
;       
; :Keywords:
; 
;    aspect: in, optional, type=float
;         This kewyord allows you to specify a specific aspect ratio for the return
;         positions. The aspect ratio is calculated as YDimension/XDimension. So, for
;         example, if you wish the positions to be twice as wide as they are high, you
;         would set the Aspect keyword to 1.0/2.0 or 0.5. If you wish your positions to
;         have a square aspect ratio, you would set the Aspect keyword to 1.0.
;    fov: in, optional, type=float
;         This keyword is a four-element vector that sets the left, bottom, right, and top position
;         of the entire plot (same structure as the standard POSITION keyword). Units are normalized
;         ratio of the current device
;         
;    ixmargin: in, optional, type=float
;         This keyword is a two-element vector that sets the left and right, respectively, inside
;         X margin for the grid position. Units are normalized ratio of the current device. 
;         Default = [0.0,0.0].
;         
;    iymargin: in, optional, type=float
;         This keyword is a two-element vector that sets the bottom and top, respectively, inside
;         Y margin for the grid position. Units are normalized ratio of the current device.
;         Default = [0.0,0.0].
;         
;    oxmargin: in, optional, type=float
;         This keyword is a two-element vector that sets the left and right, respectively, outside
;         X margin for the grid position. Units are normalized ratio of the current device. 
;         Default = [0.05,0.05].
;         
;    oymargin: in, optional, type=float
;         This keyword is a two-element vector that sets the bottom and top, respectively, outside
;         Y margin for the grid position. Units are normalized ratio of the current device. 
;         Default = [0.05,0.05].
;         
;    xgap: in, optional, type=float, default=0.05
;         This keywords sets the distance between plots in the X dimension. Units are normalized
;         ratio of the current device
;         
;    ygap: in, optional, type=integer, default=0.05
;         This keywords sets the distance between plots in the X dimension. Units are normalized
;         ratio of the current device
;    
;   EXAMPLE: if you want 3 col x 2 row subpanels distributed from x_lower=0.1, y_lower=0.1,
;            x_upper=0.9, y_upper=0.9, each subpanel is separated by 0.02 in X and 0.02 in y,
;            do the following:
;            pos=bclayout([3,2], fov=[0.1,0.1,0.9,0.9],xgap=0.02,ygap=0.02]
;            It returns a 4 x 6 array, the first is for col=1, row=1, the second is for 
;            col=2, row=1, the third is for col=3, row=1, the third is for col=1, row=2, and so on
FUNCTION bcLayout, layout, $
   ASPECT=aspect, $
   IXMARGIN=ixMargin, $
   IYMARGIN=iyMargin, $
   OXMARGIN=oxMargin, $
   OYMARGIN=oyMargin, $
   FOV=fov, $
   XGAP=xgap, $
   YGAP=ygap
   
   Compile_Opt idl2
   
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN, -1
   ENDIF
   
   ; Check parameters.
   IF N_Elements(layout) EQ 0 THEN BEGIN
      Print, 'Syntax: pos = cgLocator([2,3,1])'
      RETURN, -1
   ENDIF
   IF N_Elements(layout) EQ 3 THEN layoutIndex = layout[2]-1
   
   ; Check keywords.
   area = Keyword_Set(area)
   IF N_Elements(ixmargin) EQ 0 THEN ixmargin = [0.,0.]
   IF N_Elements(iymargin) EQ 0 THEN iymargin = [0.,0.]
   IF N_Elements(oxmargin) EQ 0 THEN oxmargin = [0.08,0.05]
   IF N_Elements(oymargin) EQ 0 THEN oymargin = [0.06,0.05]
   IF N_Elements(xgap) EQ 0 THEN xgap = 0.08
   IF N_Elements(ygap) EQ 0 THEN ygap = 0.06
   
   ; Get the size of the window. If the current device supports windows,
   ; a window has to be open.
   IF ((!D.FLAGS AND 256) NE 0) AND !D.Window LT 0 THEN BEGIN
        createdWindow = 1
        Window, /Pixmap
   ENDIF ELSE createdWindow = 0
   xsize = Double(!D.X_Size)
   ysize = Double(!D.Y_Size)
   IF createdWindow THEN WDelete, !D.Window
   
   ; Number of columns and rows.
   IF N_Elements(layout) LT 2 THEN Message, 'Layout parameter must be a 2- or 3-element vector.'
   ncols = layout[0]
   nrows = layout[1]
   
   ; Set up the inside and outside margins and the gaps between positions.
   ;unit = !D.X_PX_CM / 3
   ;xomargin = oxmargin * unit
   ;yomargin = oymargin * unit
   ;ximargin = ixmargin * unit / xsize
   ;yimargin = iymargin * unit / ysize
   ;gapx = xgap * unit
   ;gapy = ygap * unit
   xomargin=oxmargin*xsize
   yomargin=oymargin*ysize
   if keyword_set(fov) then begin
        if n_elements(fov) lt 4 then message, 'FOV parameter must be a 4-element vector [x0,y0,x1,y1].'
        xomargin[0]=fov[0]*xsize
        xomargin[1]=(1.-fov[2])*xsize
        yomargin[0]=fov[1]*ysize
        yomargin[1]=(1.-fov[3])*ysize
   endif
   ximargin=ixmargin
   yimargin=iymargin
   gapx=xgap*xsize
   gapy=ygap*ysize

   ; Calculate the window or drawing area inside the graphics window.
   winarea = [ xomargin[0], yomargin[0], xsize - xomargin[1], ysize - yomargin[1] ]   
   
   ; Calculate the plot width and height.
   plot_width  = (winarea[2] - winarea[0] - gapx*(ncols-1)) / ncols
   plot_height = (winarea[3] - winarea[1] - gapy*(nrows-1)) / nrows
   
   ; Calculate the plot areas inside the drawing area.
   plot_areas = FltArr(4, ncols, nrows)
   FOR j=0,nrows-1 DO BEGIN
      FOR k=0,ncols-1 DO BEGIN
         plot_areas[0,k,j] = winarea[0] + (plot_width + gapx) * k  ; x0
         plot_areas[2,k,j] = plot_areas[0,k,j] + plot_width        ; x1
         plot_areas[1,k,j] = winarea[3] - (plot_height + gapy) * j ; y0
         plot_areas[3,k,j] = plot_areas[1,k,j] - plot_height       ; y1
      ENDFOR
   ENDFOR
   
   ; Normalize the plot areas.
   plot_areas[[0,2],*,*] = plot_areas[[0,2],*,*] / xsize
   plot_areas[[1,3],*,*] = plot_areas[[1,3],*,*] / ysize
   
   ; Calculate the plot positions. These are the plot areas with the
   ; inside margins subtracted.
   positions = FltArr(4, ncols, nrows)
   positions[0,*,*] = plot_areas[0,*,*] + ximargin[0]
   positions[2,*,*] = plot_areas[2,*,*] - ximargin[1]
   positions[3,*,*] = plot_areas[1,*,*] + yimargin[0]
   positions[1,*,*] = plot_areas[3,*,*] - yimargin[1]
   
   ; Reform the positions into a 4 by ncols*rows array.   
   positions = Reform(positions, 4, ncols*nrows)
   
   ; Did the user ask for an aspect ratio?
   IF N_Elements(aspect) NE 0 THEN BEGIN
   
      ; Make sure aspect is not 0.
      IF aspect[0] EQ 0 THEN Message, 'The aspect ratio cannot be zero.'
   
      ; Calculate the same aspect ratio for each of the positions.
      FOR j=0,ncols*nrows-1 DO BEGIN
      
          p = positions[*,j]
          xpixSize = (p[2] - p[0]) * xsize
          ypixSize = (p[3] - p[1]) * ysize
          ratio = aspect[0]
          
          ; Try to fit the width. If you can't maintain
          ; the aspect ratio, fit the height.
          trialX = xpixSize
          trialY = trialX * ratio
          IF trialY GT ypixSize THEN BEGIN
             trialY = ypixSize
             trialX = trialY / ratio
          ENDIF
          
          ; Recalculate the position of the plot in the window.
          p[0] = (((xpixSize - trialX) / 2.0) / xsize) + p[0]
          p[2] = p[0] + (trialX/Float(xsize))
          p[1] = (((ypixSize - trialY) / 2.0) / ysize)  + p[1]
          p[3] = p[1] + (trialY/Float(ysize))
          
          positions[*,j] = p
        ENDFOR
   ENDIF
   
   ; If you have a layout index use that to return a specific
   ; position. Otherwise, return all the positions calculated
   ; for the window.
   IF N_Elements(layoutIndex) EQ 0 THEN BEGIN
      RETURN, positions
   ENDIF ELSE BEGIN
      RETURN, positions[*,layoutIndex]
   ENDELSE
   
END      
      
