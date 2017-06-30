; NAME:
;    maxfit
; PURPOSE:
;    find maximum position of an image using either parabolic or 2D Gaussian fit
; CALLING SEQUENCE:
;    maxfit, image, xycen[, peak=peak, blc=blc, trc=trc, width=width, gauss=gauss, plot=plot]  
; INPUTS:
;    image: 2-dimensional image to fit. maximum four dimensions. 
;           The input image can also be a ssw map structure.
; KEYWORDS:
;    blc: two-element vector. bottom-left corner of the maxfit region. 
;    trc: two-element vector. top-right corner of the maxfit region. 
;    width: half-width to fit the parabolic or Gaussian function [xwidth, ywidth] in 
;           pixel unit. Default is [5, 5]. If parabolic, all pixels within [xmax-xwidth, 
;           xmax+xwidth] and [ymax-ywidth, ymax+ywidth] are used for the fitting. If 
;           Gaussian, this should be FWHM/2 of the initial guess of the source size.  
;    gauss: if defined, use 2D Gaussian fit to find the maximum. Default is to fit the 
;           maximum using parabolic function
; OUTPUTS: 
;    xycen: two-element vector containing fitted x and y coordinates of the image maximum. 
;          If the input is a regular image, then the x- (or y-) 
;          coordinates are in pixel units. If the input is a ssw map structure, then
;          the fitted coordinates are in actual map units.
;    peak: value of the fitted maximums of the image. 

pro maxfit,image0,xycen,peak=peak,blc=blc,trc=trc,$
           gauss=gauss,width=width,plot=plot,quiet=quiet
if n_params() lt 2 then begin
    print,string(7B),$
   "CALL: maxfit, image, cent, [peak=peak, blc=blc, trc=trc, width=width, gauss=gauss]"
    return
endif

if n_elements( width ) le 0 then width = [5,5]
if n_elements( width ) eq 1 then width = replicate(width, 2)
if datatype(image0) eq 'STC' then ismap=1 else ismap=0

;check if the input image is a regular n-d array or a ssw map structure
if datatype(image0) eq 'STC' then begin
    if valid_map(image0) then begin
        image=image0.data
        imsize=size(image) 
        if imsize[0] ne 2 then begin
            print, 'input image is not two-dimensional, abort...'
            return
        endif
        nx=imsize[1] & ny=imsize[2]
        dx=image0.dx & dy=image0.dy
        xc=image0.xc & yc=image0.yc
        ;print,'size of the input map: ', nx,' x ',ny, ' pixel'
    endif else begin
        print, 'invalid input map'
        return
    endelse
endif else begin
    image=image0
    imsize=size(image0)
    if imsize[0] ne 2 then begin
        print, 'input image is not two-dimensional, abort...'
        return
    endif
    nx=imsize[1] & ny=imsize[2]
    print, 'size of the input map: ',nx,' x ',ny, ' pixel'
endelse

;setup the region to find the maximum
if ~keyword_set(blc) then blc=replicate(0,2)
if ~keyword_set(trc) then trc=[imsize[1]-1,imsize[2]-1]
if n_elements(blc) lt 2 or n_elements(trc) lt 2 then begin
    print, 'blc and trc should have two elements'
    return
endif

subimg=image[blc[0]:trc[0],blc[1]:trc[1]]  
;find the pixel having the peak intensity
locate_peak,subimg,xmax,ymax
if ~keyword_set(gauss) then begin
    ngrid=10
    xran=indgen(2*width[0]+1)-width[0]+xmax
    yran=indgen(2*width[1]+1)-width[1]+ymax
    if ((xmax-width[0]) lt 0) or ((xmax+width[0]) gt (trc[0]-blc[0])) $
        or ((ymax-width[1]) lt 0) or ((ymax+width[1]) gt (trc[1]-blc[1])) then begin
        if ~keyword_set(quiet) then $
            print,'peak is found to be close to the region boundary, return xycen=[1e10,1e10] and peak=-1'
        xycen=[1e10,1e10] & peak=-1
    endif else begin
        xf=poly_fit(xran,subimg[xmax-width[0]:xmax+width[0],ymax],2,yfit=imxfit)
        yf=poly_fit(yran,subimg[xmax,ymax-width[1]:ymax+width[1]],2,yfit=imyfit)
        xcen=-xf[1]/2./xf[2] & ycen=-yf[1]/2./yf[2]
        xpeak=xf[0]+xf[1]*xcen+xf[2]*xcen^2. & ypeak=yf[0]+yf[1]*ycen+yf[2]*ycen^2.
        xycen=[xcen+blc[0],ycen+blc[1]]
        peak=mean([xpeak,ypeak])
    endelse
    if ismap then begin
        xcen=(xycen[0]-(nx-1.)/2.)*dx+xc 
        ycen=(xycen[1]-(ny-1.)/2.)*dy+yc 
        xycen=[xcen,ycen]
    endif
endif else begin
    imfit=gauss2dfit(subimg,cfit,/tilt)
    peak=max(imfit)
    xcen=cfit[4]+blc[0] & ycen=cfit[5]+blc[1]
    xycen=[xcen,ycen]
    if ismap then begin
        xcen=(xycen[0]-(nx-1.)/2.)*dx+xc 
        ycen=(xycen[1]-(ny-1.)/2.)*dy+yc 
        xycen=[xcen,ycen]
    endif
endelse

if keyword_set(plot) then begin
    pos=[0.1,0.1,0.95,0.95]
    ys=800
    xs=ys*(pos[3]-pos[1])/(pos[2]-pos[0])*nx/ny
    window,0,xs=xs,ys=ys
    xsize=(pos[2] - pos[0]) * !D.X_VSIZE
    ysize=(pos[3] - pos[1]) * !D.Y_VSIZE
    xstart=pos[0] * !D.X_VSIZE
    ystart=pos[1] * !D.Y_VSIZE
    loadct,39
    tvscl,congrid(image,xsize,ysize),xstart,ystart
    if ismap then begin
        xs=findgen(nx+1)*dx-nx/2.*dx+xc
        ys=findgen(ny+1)*dy-ny/2.*dy+yc
    endif else begin
        xs=indgen(nx+1)
        ys=indgen(ny+1)
    endelse
    plot,xs,ys,/nodata,/noerase,position=pos,$
        /xsty,/ysty,chars=1.3
    plots,xycen[0],xycen[1],syms=2,psym=1,color=255
    if keyword_set(gauss) then $
        contour,imfit,xs[blc[0]:trc[0]],ys[blc[1]:trc[1]],/overplot,levels=[0.2,0.5,0.8]*peak
endif
return
end
