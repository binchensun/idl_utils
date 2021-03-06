;+
; NAME: 
;     centfit
; PURPOSE:
;     Compute centroid coordinates of an image using 2D Gaussian fit.
; CALLING SEQUENCE: 
;     centfit, image, imfit, XCEN, YCEN, XY_PEAK=[x,y], [FWHM=fwhm], [/PEAK_LOC]
; INPUTS:     
;     image - Two dimensional image array
; KEYWORDS:
;     XY_PEAK = [x,y] vector of 2 integers giving approximate image center.
;		Pixel at maximum in subimage will then be used to start.
;		If not specified then max pixel of whole image is used.
;     FWHM - floating scalar or 2 elements (for x & y), default FWHM = 3 pixels.
;		Centroid computed in box of half width = 1.5*sigma = 0.637*FWHM
;     /PEAK_LOCATE - causes the peak (maximum pixel) of image to be used
;		as approximate centroid location (overrides XY_PEAK).
; OUTPUTS:   
;     IMFIT - the fitted image
;     XCEN - floating scalar, giving the computed X centroid position
;     YCEN - floating scalar, giving the computed Y centroid position
;          Values for XCEN and YCEN will not be computed if the
;          centroid falls outside of the box. If the centroid cannot be 
;          computed, then a message is displayed and XCEN and YCEN are 
;          set to -1.
;     XWIDTH - fitted Gaussian width, in x
;     YWIDTH - fitted Gaussian width, in y
;     RX - the subimage boundary in x, the unit is the index in the input image
;     RY - the subimage boundary in y, the unit is the index in the input image
;     flag - if there is no fit, then flag is set to 1, otherwise 0
; RESTRICTIONS: 
;    Program does not check if the input X and Y position falls within image.
; SYSTEM VARIABLES:
;    !DEBUG - If set, the subarray used to compute the centroid is printed
; EXTERNAL CALLS:
;		pro centfit, image, xmax, ymax
; MODIFICATION HISTORY:
;    Based on centroid.pro originally written 2/25/86, by J. K. Hill, S.A.S.C., following
;               algorithm used by P. Stetson in DAOPHOT.
;    Added vector subscripting, W. Landsman   5/21/87
;    Mod: F.Varosi 1991-92, changed name from CNTRD to CENTROID, optimized,
;       changed SUM calls to SUM_ARRAY, added keyword XY_PEAK=[x,y],
;       option /PEAK_LOC to locate and use maximum,
;       add 0.5 to centroid x & y values so it is in center of pixel.
;    Edit: FV 1999, added more comments explaining keywords in header info.
;-   Edit: Bin Chen 2013, removed some unused feature, and added keywords 
;           to select only a portion of the image for fitting
pro centfit, image, imfit, Xcen, Ycen, Xwidth, Ywidth, rx, ry, flag,$
    XY_PEAK=xy_peak, FWHM=fwhm, peak_locate=peak_loc, plot=plot
IF N_PARAMS() LT 1 THEN BEGIN
	PRINT,STRING(7B),$
   "CALL: centfit ,image, xcen, ycen, XY_PEAK=[x,y], [FWHM=fwhm, /PEAK_LOCATE]"
	RETURN
   ENDIF

if N_elements( fwhm ) LE 0 then fwhm = [10,10]
if N_elements( fwhm ) eq 1 then fwhm = replicate( fwhm(0), 2 )

NHALF_xy =  fix( 0.637 * fwhm + 0.5 ) > 2
NBOX_xy = 2*NHALF_xy+1	      ;Width of box to be used to compute centroid
sim = size( image )
L = sim-1

if keyword_set( peak_Loc ) then begin

	Locate_peak, image, xmax, ymax
	xy_peak = [xmax,ymax]

endif else if N_elements( xy_peak ) EQ 2 then begin

	X = xy_peak(0)		;Locate maximum pixel in subimage
	Y = xy_peak(1)		;  around the given (X,Y) pixel.
	;NHALFBIG = NHALF_xy +3	;Extend box 3 pixels on each side
	rX = ( [ X-NHALF_xy(0) , X+NHALF_xy(0) ] > 0 ) < L(1)
	rY = ( [ Y-NHALF_xy(1) , Y+NHALF_xy(1) ] > 0 ) < L(2)
	locate_peak, image( rX(0):rX(1), rY(0):rY(1) ), xmax, ymax
	XMAX = rX(0) + xmax  	;X coordinate in original image array
	YMAX = rY(0) + ymax  	;Y coordinate in original image array

endif else begin

	locate_peak, image, xmax, ymax
	xy_peak = [xmax,ymax]
	if !DEBUG then print," using (x,y) peak: ",xy_peak
endelse
				;extract subimage centered on maximum pixel
rx = [ XMAX-NHALF_xy(0), XMAX+NHALF_xy(0) ]
ry = [ YMAX-NHALF_xy(1), YMAX+NHALF_xy(1) ]

if (min( [rx,ry] ) LT 0) OR max( ([rx(1),ry(1)] GT L(1:2)) ) then begin
	message,"box ouside the image boundary, no fitting!",/INFO
        flag=1
	rX = ( [ X-NHALF_xy(0) , X+NHALF_xy(0) ] > 0 ) < L(1)
	rY = ( [ Y-NHALF_xy(1) , Y+NHALF_xy(1) ] > 0 ) < L(2)
	imfit = image[rx[0]:rx[1], ry[0]:ry[1]]
        xcen=xmax & ycen=ymax
        xwidth=0 & ywidth=0
	return
endif else subim = image[ rx(0):rx(1), ry(0):ry(1) ]

if !DEBUG then begin
	PRINT,' Maximum pixel value is ',image[xmax,ymax],'  at',xmax,ymax
	PRINT,'Subarray used to compute centroid:'
	PRINT,subim
endif

imfit=gauss2dfit(subim,cfit,/tilt)
flag=0
xcen=cfit[4]+rx[0] & ycen=cfit[5]+ry[0] 
  ;the fitted centroid coordinates in the original input image
xwidth=cfit[2] & ywidth=cfit[3] 

if keyword_set (plot) then begin
  pos=[0.13,0.13,0.95,0.9]
  xsize=(pos[2] - pos[0]) * !D.X_VSIZE
  ysize=(pos[3] - pos[1]) * !D.Y_VSIZE
  xstart=pos[0] * !D.X_VSIZE
  ystart=pos[1] * !D.Y_VSIZE
  loadct,39
  ;image1=bytscl(alog10(image>0.1),max=1,min=-1)
  tv,congrid(image,xsize,ysize),xstart,ystart
  xs=indgen(sim[1]) & ys=indgen(sim[2])
  plot,xs,ys,/nodata,/noerase,position=pos,$
    /xsty,/ysty,chars=1.3
  max=max(imfit)
  levels=[0.2,0.4,0.6,0.8]*max
  contour,imfit,xs[rx[0]:rx[1]],ys[ry[0]:ry[1]],/overplot,levels=levels
  plots,xmax,ymax,syms=1.3,psym=1
  plots,xcen,ycen,syms=1.3,psym=2,color=250
endif
return
end
